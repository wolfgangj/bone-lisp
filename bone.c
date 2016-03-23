/* bone.c -- The Bone Lisp interpreter.
 * Copyright (C) 2016 Wolfgang Jaehrling
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#define _GNU_SOURCE 1 // for mmap()s MAP_ANONYMOUS
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/mman.h>

#define my static

typedef uint64_t any;
typedef enum { t_cons = 0, t_sym = 1, t_uniq = 2, t_str = 3, t_reg = 4, t_sub = 5, t_num = 6, t_other = 7 } type_tag;

#define UNIQ(n) (t_uniq | (010*(n)))
#define NIL     UNIQ(0)
#define BTRUE   UNIQ(1)
#define BFALSE  UNIQ(2)
#define HASH_SLOT_UNUSED  UNIQ(100)
#define HASH_SLOT_DELETED UNIQ(101)

void type_error(any x, type_tag t) {
  puts("type error"); // FIXME: show more info
  exit(1);
}
type_tag tag_of(any x) { return x & 7; }
bool is_tagged(any x, type_tag t) { return tag_of(x) == t; }
void check(any x, type_tag t) { if(!is_tagged(x, t)) type_error(x, t); }
any tag(any x, type_tag t) { return x | t; }
any untag(any x) { return x & ~7; }
any untag_check(any x, type_tag t) { check(x, t); return untag(x); }

any to_bool(int x) { return x ? BTRUE : BFALSE; }
bool is(any x) { return x != BFALSE; }

// FIXME: these assume little-endian
int32_t any2int(any x) { check(x, t_num); return ((int32_t *) &x)[1]; }
any int2any(int32_t n) { any r = t_num; ((int32_t *) &r)[1] = n; return r; }

//////////////// regions ////////////////

#define ALLOC_BLOCKS_AT_ONCE 16
my size_t blocksize;  // in bytes
my size_t blockwords; // words per block
my any blockmask; // to get the block an obj belongs to
my any **free_block;
// A block begins with a pointer to the next block that belongs to the region.
// The metadata of a region (i.e. this struct) is stored in its first block.
typedef struct { any **end, **allocp; } *reg;

my any **block(any *x) { return (any **) (blockmask & (any) x); } // get ptr to start of block that x belongs to.
my any **blocks_alloc(int n) { return mmap(NULL, blocksize*n, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0); }
my void block_point_to_next(any **p, int i) { p[i*blockwords] = (any *) &p[(i+1)*blockwords]; }
my void blocks_init(any **p, int n) { n--; for(int i = 0; i < n; i++) block_point_to_next(p, i); p[n*blockwords] = NULL; }
my any **fresh_blocks() { any **p = blocks_alloc(ALLOC_BLOCKS_AT_ONCE); blocks_init(p, ALLOC_BLOCKS_AT_ONCE); return p; }
my void ensure_free_block() { if(!free_block) free_block = fresh_blocks(); }
my any **block_new(any **next) { ensure_free_block(); any **r = free_block; free_block = (any **) r[0]; r[0] = (any *) next; return r; }
my void reg_init(reg r, any **b) { r->end = b; r->allocp = (any **) &r[1]; }
reg reg_new() { any **b = block_new(NULL); reg r = (reg) &b[1]; reg_init(r, b); return r; }
void reg_free(reg r) { block((any *) r)[0] = (any *) free_block; free_block = r->end; }
my void blocks_sysfree(any **b) { if(!b) return; any **next = (any **) b[0]; munmap(b, blocksize); blocks_sysfree(next); }
my void reg_sysfree(reg r) { blocks_sysfree(r->end); }

my reg permanent_reg;
my reg reg_stack[64];
my reg *reg_sp = reg_stack; // points to tos!
my any **allocp, **end; // from currently used reg.
my void load_reg(reg r)  { allocp = r->allocp; end = r->end; }
my void store_reg(reg r) { r->allocp = allocp; r->end = end; }
void reg_push(reg r) { store_reg(*reg_sp); reg_sp++; *reg_sp = r;     load_reg(*reg_sp); }
reg reg_pop()        { store_reg(*reg_sp); reg r = *reg_sp; reg_sp--; load_reg(*reg_sp); return r; }

any *reg_alloc(int n) {
  any *res = (any *) allocp;
  allocp += n;
  if(block((any *) allocp) == end) return res;
  end = block_new(end); allocp = (any **) &end[1]; return reg_alloc(n);
}

my any reg2any(reg r) { return tag((any) r, t_reg); }
my reg any2reg(any x) { return (reg) untag_check(x, t_reg); }

any copy(any x); // FIXME: to header
my any copy_back(any x) { reg_push(reg_sp[-1]); any y = copy(x); reg_pop(); return y; }

//////////////// conses / lists ////////////////

any cons(any a, any d) { any *p = reg_alloc(2); p[0] = a; p[1] = d; return (any) p; } // no tag() needed
any far(any x) { return ((any *) x)[0]; } // fast, no typecheck
any fdr(any x) { return ((any *) x)[1]; } // likewise
any car(any x) { check(x, t_cons); return far(x); }
any cdr(any x) { check(x, t_cons); return fdr(x); }

any is_cons(any x) { return is_tagged(x, t_cons); }
#define foreach(var, lst) for(any p_ = (lst), var; is_cons(p_) && (var = far(p_), 1); p_ = fdr(p_))

//////////////// strs ////////////////

any str(any chrs) { any *p = reg_alloc(1); *p = chrs; return tag((any) p, t_str); }
any unstr(any s) { return *(any *) untag_check(s, t_str); }
my any charp2list(const char *p) { return !*p ? NIL : cons(int2any(*p), charp2list(p+1)); }
any charp2str(const char *p) { return str(charp2list(p)); }

//////////////// hash tables ////////////////

#define MAXLOAD 128 // Value between 0 and 255; 128 will cause an average of two probes.
typedef struct { unsigned size, taken_slots; any *keys, *vals; any default_value; } *hash;

hash hash_new(unsigned initsize, any default_val) {
  hash h = malloc(sizeof(*h));
  h->size = initsize; h->taken_slots = 0; h->default_value = default_val;
  h->keys = malloc(initsize*sizeof(any));
  h->vals = malloc(initsize*sizeof(any));
  for(unsigned i = 0; i != initsize; i++) h->keys[i] = HASH_SLOT_UNUSED;
  return h;
}
void hash_free(hash h) { free(h->keys); free(h->vals); free(h); }

/* Find the entry in H with KEY and provide the entry number in *POS.
   Return true if there is an entry with this key already.  If there
   is none, *POS will contain the position of the slot we can use to
   add it. */
my bool find_slot(hash h, any key, unsigned *pos) {
  int first_deleted = -1;
  *pos = key % h->size;
  while(1) {
    if(h->keys[*pos] == key) return true;
    if(h->keys[*pos] == HASH_SLOT_UNUSED)  { if(first_deleted != -1) *pos = first_deleted; return false; }
    if(h->keys[*pos] == HASH_SLOT_DELETED) { if(first_deleted == -1) first_deleted = *pos; }
    if(++(*pos) == h->size) *pos = 0;
  }
}

void hash_set(hash h, any key, any val); // FIXME: to header
my bool slot_used(any x) { return x != HASH_SLOT_UNUSED && x != HASH_SLOT_DELETED; } 
my void enlarge_table(hash h) {
  hash new = hash_new(h->size * 2 + 1, NIL);
  for(unsigned i = 0; i != h->size; i++) if(slot_used(h->keys[i])) hash_set(new, h->keys[i], h->vals[i]);
  free(h->keys); free(h->vals); h->size = new->size; h->keys = new->keys; h->vals = new->vals; free(new);
}

void hash_set(hash h, any key, any val) {
  unsigned pos;
  if(!find_slot(h, key, &pos)) {  // adding a new entry
    h->taken_slots++;
    if(((h->taken_slots << 8) / h->size) > MAXLOAD) { enlarge_table(h); find_slot(h, key, &pos); }
  }
  h->keys[pos] = key; h->vals[pos] = val;
}
any hash_get(hash h, any key) { unsigned pos; return find_slot(h, key, &pos) ? h->vals[pos] : h->default_value; }
void hash_rm(hash h, any key) { unsigned pos; if(find_slot(h, key, &pos)) { h->keys[pos] = HASH_SLOT_DELETED; h->taken_slots--; } }
#if 0 // FIXME: hash_iter
void hash_each(hash h, hash_iter fn, void *hook) {
  for(unsigned i = 0; i != h->size; i++) if(slot_used(h->keys[i])) fn(hook, h->keys[i], h->vals[i]);
}
#endif

//////////////// syms ////////////////

my hash sym_ht;
my any string_hash(const char *s, size_t *len) {  // This is the djb2 algorithm.
  int32_t hash = 5381;
  *len = 0;
  while(*s) { (*len)++; hash = ((hash << 5) + hash) + *(s++); }
  return int2any(hash);
}

my int cells4charp(int len) { return (len+1)/sizeof(any) + 1; } // how much cells we need
my char *symtext(any sym) { return (char *) untag(sym); }
my any as_sym(char *name) { return tag((any) name, t_sym); } // `name` must be interned
my any add_sym(const char *name, size_t len, any id) {
  reg_push(permanent_reg);
  char *new = (char *) reg_alloc(cells4charp(len));
  reg_pop();
  memcpy(new, name, len);
  hash_set(sym_ht, id, (any) new);
  return as_sym(new);
}
any intern(const char *name) {
  size_t len;
  any id = string_hash(name, &len);
  while(1) {
    char *candidate = (char *) hash_get(sym_ht, id);
    if(candidate == NULL) return add_sym(name, len, id);
    if(!strcmp(candidate, name)) return as_sym(candidate);
    id++;
  }
}

//////////////// print ////////////////

void print(any x) {
  switch(tag_of(x)) {
  case t_cons:
    if(is_tagged(far(x), t_sym) && is_tagged(fdr(x), t_cons) && fdr(fdr(x)) == NIL) {
      // FIXME. don't call intern() each time
      if(far(x) == intern("quote"))            { printf("'");  print(far(fdr(x))); break; }
      if(far(x) == intern("quasiquote"))       { printf("`");  print(far(fdr(x))); break; }
      if(far(x) == intern("unquote"))          { printf(",");  print(far(fdr(x))); break; }
      if(far(x) == intern("unquote-splicing")) { printf(",@"); print(far(fdr(x))); break; }
    }
    printf("(");
    bool first = true;
    do { if(first) first=false; else printf(" "); print(far(x)); x=fdr(x); } while(is_tagged(x, t_cons));
    if(x != NIL) { printf(" . "); print(x); } printf(")"); break;
  case t_sym: printf("%s", symtext(x)); break;
  case t_num: printf("%d", any2int(x)); break;
  case t_uniq:
    switch(x) {
    case NIL: printf("()"); break;
    case BTRUE: printf("\\+"); break;
    case BFALSE: printf("\\-"); break;
    default: abort(); }
    break;
  case t_str:
    printf("\"");
    foreach(c, unstr(x))
      switch(any2int(c)) { // FIXME: add more
      case '"':  printf("\\\""); break;
      case '\\': printf("\\\\"); break;
      case '\n': printf("\\n");  break;
      case '\t': printf("\\t");  break;
      default: putchar(any2int(c)); }
    printf("\""); break;
  case t_reg: printf("\\reg(%p)", (void *) x); break;
  case t_sub: printf("\\sub(%p)", (void *) x); break;
  case t_other: default: abort(); }
}

//////////////// misc ////////////////

any copy(any x) {
  switch(tag_of(x)) {
  case t_cons: return cons(copy(far(x)), copy(fdr(x)));
  //FIXME:case t_str: return str(copy(unstr(x)));
  case t_sym: case t_num: case t_uniq: return x;
  default: return x; } // FIXME: should be an error
}

void init_bone() {
  free_block = NULL;
  blocksize = sysconf(_SC_PAGESIZE); blockmask = ~(blocksize - 1); blockwords = blocksize/sizeof(any);
  permanent_reg = reg_new(); reg_stack[0] = permanent_reg; load_reg(permanent_reg);
  sym_ht = hash_new(199, 0);
}

// FIXME: doesn't belong here
int main() {
  init_bone();
  any test = NIL;
  print(test); putchar('\n');
  test = cons(int2any(1), cons(int2any(2), NIL));
  print(test); putchar('\n');
  test = cons(int2any(1), cons(int2any(2), int2any(3)));
  print(test); putchar('\n');
  test = cons(intern("quote"), cons(int2any(2), NIL));
  print(test); putchar('\n');
  test = cons(intern("lalala"), cons(BFALSE, NIL));
  print(test); putchar('\n');
  test = charp2str("\"Hello, world!\"\n");
  print(test); putchar('\n');
  test = reg2any(permanent_reg);
  print(test); putchar('\n');
  return 0;
}

