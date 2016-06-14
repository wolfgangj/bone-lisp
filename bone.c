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
#include "bone.h"

//my any L(any x) { print(x); puts(""); return x; } // for debugging
my void fail(const char *msg) { fprintf(stderr, "%s\n", msg); exit(1); }
my size_t bytes2words(size_t n) { return (n-1)/sizeof(any) + 1; }
#define x(tag, name) case tag: return name
my const char *type_name(type_tag tag) { switch(tag) {
    x(t_cons,"cons");x(t_sym,"sym");x(t_str,"str");x(t_reg,"reg");x(t_sub,"sub");x(t_num,"num");default:return "<?>"; } }
#undef x

#define HASH_SLOT_UNUSED  UNIQ(100)
#define HASH_SLOT_DELETED UNIQ(101)
#define READER_LIST_END   UNIQ(102)
#define BINDING_DEFINED   UNIQ(103)
#define BINDING_EXISTS    UNIQ(104)
#define BINDING_DECLARED  UNIQ(105)
#define VAR_UNBOUND       UNIQ(106)
bool is_nil(any x) { return x == NIL; }
bool is(any x) { return x != BFALSE; }
any to_bool(int x) { return x ? BTRUE : BFALSE; }

my void print(any x); my void backtrace(); // FIXME: to header?
my void generic_error(const char *msg, any x) { printf("%s: ", msg); print(x); printf("\n"); backtrace(); throw(); }
my void type_error(any x, type_tag t) { 
  printf("ERR: typecheck failed: (%s? ", type_name(t)); print(x); printf(")\n"); backtrace(); throw();
}
my type_tag tag_of(any x) { return x & 7; }
my bool is_tagged(any x, type_tag t) { return tag_of(x) == t; }
my void check(any x, type_tag t) { if(!is_tagged(x, t)) type_error(x, t); }
my any tag(any x, type_tag t) { return x | t; }
my any untag(any x) { return x & ~7; }
my any untag_check(any x, type_tag t) { check(x, t); return untag(x); }

my bool is_num(any x) { return is_tagged(x, t_num); }
// FIXME: these assume little-endian
int32_t any2int(any x) { check(x, t_num); return ((int32_t *) &x)[1]; }
any int2any(int32_t n) { any r = t_num; ((int32_t *) &r)[1] = n; return r; }

//////////////// regions ////////////////

#define ALLOC_BLOCKS_AT_ONCE 16
my size_t blocksize;  // in bytes
my size_t blockwords; // words per block
my any blockmask; // to get the block an `any` belongs to; is not actually an object!
my any **free_block;
// A block begins with a pointer to the previous block that belongs to the region.
// The metadata of a region (i.e. this struct) is stored in its first block.
typedef struct { any **current_block, **allocp; } *reg;

my any **block(any *x) { return (any **) (blockmask & (any) x); } // get ptr to start of block that x belongs to.
my any **blocks_alloc(int n) { return mmap(NULL, blocksize*n, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0); }
my void block_point_to_next(any **p, int i) { p[i*blockwords] = (any *) &p[(i+1)*blockwords]; }
my void blocks_init(any **p, int n) { n--; for(int i = 0; i < n; i++) block_point_to_next(p, i); p[n*blockwords] = NULL; }
my any **fresh_blocks() { any **p = blocks_alloc(ALLOC_BLOCKS_AT_ONCE); blocks_init(p, ALLOC_BLOCKS_AT_ONCE); return p; }
my void ensure_free_block() { if(!free_block) free_block = fresh_blocks(); }
my any **block_new(any **next) { ensure_free_block(); any **r = free_block; free_block = (any **) r[0]; r[0] = (any *) next; return r; }
my void reg_init(reg r, any **b) { r->current_block = b; r->allocp = (any **) &r[1]; }
my reg reg_new() { any **b = block_new(NULL); reg r = (reg) &b[1]; reg_init(r, b); return r; }
my void reg_free(reg r) { block((any *) r)[0] = (any *) free_block; free_block = r->current_block; }
my void blocks_sysfree(any **b) { if(!b) return; any **next = (any **) b[0]; munmap(b, blocksize); blocks_sysfree(next); }
my void reg_sysfree(reg r) { blocks_sysfree(r->current_block); }

my reg permanent_reg;
my reg reg_stack[64];
my reg *reg_sp = reg_stack; // points to tos!
my any **allocp, **current_block; // from currently used reg.
my void load_reg(reg r)  { allocp = r->allocp; current_block = r->current_block; }
my void store_reg(reg r) { r->allocp = allocp; r->current_block = current_block; }
my void reg_push(reg r) { store_reg(*reg_sp); reg_sp++; *reg_sp = r;     load_reg(*reg_sp); }
my reg reg_pop()        { store_reg(*reg_sp); reg r = *reg_sp; reg_sp--; load_reg(*reg_sp); return r; }
my void reg_permanent() { reg_push(permanent_reg); }
my void rollback_reg_sp(reg *p) {
  while(reg_sp != p)
    reg_free(reg_pop());
}

my any *reg_alloc(int n) {
  any *res = (any *) allocp;
  allocp += n;
  if(block((any *) allocp) == current_block)
    return res; // normal case
  current_block = block_new(current_block);
  allocp = (any **) &current_block[1];
  return reg_alloc(n);
}

my any reg2any(reg r) { return tag((any) r, t_reg); }
my reg any2reg(any x) { return (reg) untag_check(x, t_reg); }

my any copy(any x);
my any copy_back(any x) {
  reg_push(reg_sp[-1]);
  any y = copy(x);
  reg_pop();
  return y;
}

//////////////// excepions ////////////////

my struct { jmp_buf buf; reg *reg_sp; } exc_bufs[32]; // FIXME: thread-local
my int exc_num = 0;

jmp_buf *begin_try_() {
  exc_bufs[exc_num].reg_sp = reg_sp;
  return &exc_bufs[exc_num++].buf;
}

my void exc_buf_nonempty() {
  if(!exc_num)
    fail("internal error: throw/catch mismatch");
}

jmp_buf *throw_() {
  exc_buf_nonempty();
  exc_num--;
  rollback_reg_sp(exc_bufs[exc_num].reg_sp);
  return &exc_bufs[exc_num].buf;
}

void end_try_() {
  exc_buf_nonempty();
  exc_num--;
}

//////////////// conses / lists ////////////////

any cons(any a, any d) { any *p = reg_alloc(2); p[0] = a; p[1] = d; return (any) p; } // no tag() needed as t_cons==0
any far(any x) { return ((any *) x)[0]; } // fast, no typecheck
any fdr(any x) { return ((any *) x)[1]; } // likewise
any car(any x) { check(x, t_cons); return far(x); }
any cdr(any x) { check(x, t_cons); return fdr(x); }
void set_far(any cell, any x) { ((any *) cell)[0] = x; }
void set_fdr(any cell, any x) { ((any *) cell)[1] = x; }

bool is_cons(any x) { return is_tagged(x, t_cons); }
bool is_single(any x) { return is_cons(x) && is_nil(fdr(x)); }
any single(any x) { return cons(x, NIL); }

listgen listgen_new() { listgen res = { NIL, NIL }; return res; }

void listgen_add(listgen *lg, any x) {
  if(is_nil(lg->xs))
    lg->xs = lg->last = single(x);
  else {
    any new = single(x);
    set_fdr(lg->last, new);
    lg->last = new;
  }
}

my void listgen_add_list(listgen *lg, any xs) {
  foreach(x, xs)
    listgen_add(lg, x);
}

my void listgen_set_tail(listgen *lg, any x) {
  if(is_nil(lg->xs))
    lg->xs=lg->last=x;
  else
    set_fdr(lg->last, x);
}

my int len(any x) {
  int n = 0;
  foreach_cons(e, x)
    n++;
  return n;
}

my any reverse(any xs) {
  any res = NIL;
  foreach(x, xs)
    res = cons(x, res);
  return res;
}

my bool is_member(any a, any xs) {
  foreach(x, xs)
    if(x == a)
      return true;
  return false;
}

my any assoc(any obj, any xs) {
  foreach(x, xs)
    if(car(x) == obj)
      return fdr(x);
  return BFALSE;
}

my any assoc_entry(any obj, any xs) {
  foreach(x, xs)
    if(car(x) == obj)
      return x;
  return BFALSE;
}

my any cat2(any a, any b) {
  if(is_nil(a)) return b;
  listgen lg = listgen_new();
  foreach(x, a)
    listgen_add(&lg, x);
  set_fdr(lg.last, b);
  return lg.xs;
}

my any move_last_to_rest_x(any xs) {
  if(is_single(xs)) return far(xs);
  foreach_cons(pair, xs)
    if(is_single(fdr(pair))) {
      set_fdr(pair, far(fdr(pair))); break;
    }
  return xs;
}

//////////////// strs ////////////////

bool is_str(any x) { return is_tagged(x, t_str); }

my any str(any chrs) {
  any *p = reg_alloc(1);
  *p = chrs;
  return tag((any) p, t_str);
}

my any unstr(any s) { return *(any *) untag_check(s, t_str); }

my any charp2list(const char *p) { return !*p ? NIL : cons(int2any(*p), charp2list(p+1)); } // FIXME: for short strings only
any charp2str(const char *p) { return str(charp2list(p)); }
my char *list2charp(any x) {
  char *res = malloc(len(x) + 1); // FIXME: longer for UTF-8
  char *p = res;
  try {
    foreach(c, x) {
      *p = any2int(c); p++;
    }
  } catch {
    free(res);
    throw();
  }
  *p = '\0'; return res;
}
char *str2charp(any x) { return list2charp(unstr(x)); }

my bool str_eql(any s1, any s2) {
  s1=unstr(s1); s2=unstr(s2);
  foreach(chr, s1) {
    if(is_nil(s2) || chr==far(s2))
      return false;
    s2=fdr(s2);
  }
  return is_nil(s2);
}

//////////////// hash tables ////////////////

#define MAXLOAD 175 // Value between 0 and 255; 128 will cause an average of two probes.
typedef struct { unsigned size, taken_slots; any *keys, *vals; any default_value; } *hash;

my hash hash_new(unsigned initsize, any default_val) {
  hash h = malloc(sizeof(*h));
  h->size = initsize; h->taken_slots = 0; h->default_value = default_val;
  h->keys = malloc(initsize*sizeof(any)); h->vals = malloc(initsize*sizeof(any));
  for(unsigned i = 0; i != initsize; i++) h->keys[i] = HASH_SLOT_UNUSED; return h;
}
my void hash_free(hash h) { free(h->keys); free(h->vals); free(h); }

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

my void hash_set(hash h, any key, any val);
my bool slot_used(any x) { return x != HASH_SLOT_UNUSED && x != HASH_SLOT_DELETED; } 

my void enlarge_table(hash h) {
  hash new = hash_new(h->size * 2 + 1, NIL);
  for(unsigned i = 0; i != h->size; i++)
    if(slot_used(h->keys[i]))
      hash_set(new, h->keys[i], h->vals[i]);
  free(h->keys); free(h->vals); h->size = new->size; h->keys = new->keys; h->vals = new->vals; free(new);
}

my void hash_set(hash h, any key, any val) {
  unsigned pos;
  if(!find_slot(h, key, &pos)) {  // adding a new entry
    h->taken_slots++;
    if(((h->taken_slots << 8) / h->size) > MAXLOAD) {
      enlarge_table(h);
      find_slot(h, key, &pos);
    }
  }
  h->keys[pos] = key;
  h->vals[pos] = val;
}

my any hash_get(hash h, any key) {
  unsigned pos;
  return find_slot(h, key, &pos) ? h->vals[pos] : h->default_value;
}

my void hash_rm(hash h, any key) {
  unsigned pos;
  if(find_slot(h, key, &pos)) {
    h->keys[pos] = HASH_SLOT_DELETED;
    h->taken_slots--;
  }
}

#if 0 // FIXME: hash_iter
my void hash_each(hash h, hash_iter fn, void *hook) {
  for(unsigned i = 0; i != h->size; i++)
    if(slot_used(h->keys[i])) fn(hook, h->keys[i], h->vals[i]);
}
my void hash_print(hash h) { // useful for debugging
  for(unsigned i = 0; i != h->size; i++)
    if(slot_used(h->keys[i])) {
      print(h->keys[i]); putchar('='); print(h->vals[i]); putchar('\n');
    }
}
#endif

//////////////// syms ////////////////

my bool is_sym(any x) { return is_tagged(x, t_sym); }
my hash sym_ht;
my any string_hash(const char *s, size_t *len) {  // This is the djb2 algorithm.
  int32_t hash = 5381; *len = 0;
  while(*s) { (*len)++; hash = ((hash << 5) + hash) + *(s++); }
  return int2any(hash);
}
char *symtext(any sym) { return (char *) untag_check(sym, t_sym); }
my any as_sym(char *name) { return tag((any) name, t_sym); } // `name` must be interned
my any add_sym(const char *name, size_t len, any id) {
  reg_permanent();
  char *new = (char *) reg_alloc(bytes2words(len+1));
  reg_pop();
  memcpy(new, name, len);
  hash_set(sym_ht, id, (any) new);
  return as_sym(new);
}
any intern(const char *name) {
  size_t len; any id = string_hash(name, &len);
  while(1) {
    char *candidate = (char *) hash_get(sym_ht, id);
    if(candidate == NULL) return add_sym(name, len, id);
    if(!strcmp(candidate, name)) return as_sym(candidate);
    id++;
  }
}
my any intern_from_chars(any chrs) {
  char *s = list2charp(chrs);
  any res = intern(s);
  free(s);
  return res;
}
my any gensym() {
  static int gensyms = 0;
  reg_permanent(); char *new = (char *) reg_alloc(1); reg_pop();
  snprintf(new, sizeof(any), "_g%05d", gensyms++);
  return as_sym(new);
}

my any s_quote, s_quasiquote, s_unquote, s_unquote_splicing, s_lambda, s_with, s_if, s_list, s_cat, s_dot, s_do, s_arg, s_env;
#define x(name) s_ ## name = intern(#name)
my void init_syms() { x(quote);x(quasiquote);x(unquote);s_unquote_splicing=intern("unquote-splicing");
  x(lambda);x(with);x(if);x(list);x(cat);s_dot=intern(".");x(do);x(arg);x(env); }
#undef x

//////////////// subs ////////////////

typedef struct sub_code { // fields are in the order in which we access them.
  int argc; // number of required args
  int take_rest; // accepting rest args? 0=no, 1=yes
  int extra_localc; // the ones introduced by `with`
  any name; // sym for backtraces
  int size_of_env; // so that we can copy subs
  any ops[1]; // can be longer
} *sub_code;
#define sub_code_header_size (bytes2words(sizeof(struct sub_code))-1)
my sub_code make_sub_code(int argc, int take_rest, int extra_localc, int size_of_env, int code_size) {
  sub_code code = (sub_code) reg_alloc(sub_code_header_size + code_size);
#define x(f) code->f = f
  x(argc); x(take_rest); x(extra_localc); x(size_of_env);
#undef x
   code->name = BFALSE; return code;
}
my int count_locals(sub_code sc) { return sc->argc + sc->take_rest + sc->extra_localc; }

typedef struct { sub_code code; any env[0]; } *sub;
my bool is_sub(any x) { return is_tagged(x, t_sub); }
my any sub2any(sub s) { return tag((any) s, t_sub); }
my sub any2sub(any x) { return (sub) untag_check(x, t_sub); }

my any copy(any x);
my any copy_sub(any x) {
  sub s = any2sub(x);
  int envsize = s->code->size_of_env;
  any *p = reg_alloc(1+envsize);
  *p++ = (any) s->code;
  for(int i = 0; i != envsize; i++)
    *p++ = copy(s->env[i]);
  return tag((any) p, t_sub);
}

my void name_sub(sub subr, any name) { if(!is(subr->code->name)) subr->code->name = name; }

//////////////// printer ////////////////

my void print_sub_args(any x) {
  if(!is_cons(x)) { if(!is_nil(x)) { printf(". "); print(x); printf(" "); } return; }
  print(far(x)); printf(" "); print_sub_args(fdr(x));
}

my void print(any x) {
  switch(tag_of(x)) {
  case t_cons: {
    any a = far(x);
    if(is_sym(a)) {
      if(a == s_quote)            { printf("'");  print(fdr(x)); break; }
      if(a == s_quasiquote)       { printf("`");  print(fdr(x)); break; }
      if(a == s_unquote)          { printf(",");  print(fdr(x)); break; }
      if(a == s_unquote_splicing) { printf(",@"); print(fdr(x)); break; }
      if(a == s_lambda && is_cons(fdr(x)) && is_single(fdr(fdr(x))) && is_cons(far(fdr(fdr(x))))) {
	printf("| "); print_sub_args(far(fdr(x))); print(far(fdr(fdr(x)))); break;
      }
    }
    bool first = true; printf("(");
    do { if(first) first=false; else printf(" "); print(far(x)); x=fdr(x); } while(is_tagged(x, t_cons));
    if(x != NIL) { printf(" . "); print(x); } printf(")"); break;
  }
  case t_sym: printf("%s", symtext(x)); break;
  case t_num: printf("%d", any2int(x)); break;
  case t_uniq:
    switch(x) {
    case NIL: printf("()"); break;
    case BTRUE: printf("#t"); break;
    case BFALSE: printf("#f"); break;
    case ENDOFFILE: printf("#{eof}"); break;
    default: printf("#{?}"); }
    break;
  case t_str:
    printf("\"");
    foreach(c, unstr(x))
      switch(any2int(c)) {
      case '"':  printf("\\\""); break;
      case '\\': printf("\\\\"); break;
      case '\n': printf("\\n"); break;
      case '\t': printf("\\t"); break;
      default: putchar(any2int(c)); }
    printf("\""); break;
  case t_reg: printf("#reg(%p)", (void *) x); break;
  case t_sub:
    printf("#sub(id=%p name=", (void *) x);
    sub_code code = any2sub(x)->code;
    print(code->name);
    printf(" argc=%d take-rest?=", code->argc); print(code->take_rest ? BTRUE : BFALSE); printf(")");
    break;
  case t_other: default: abort(); }
}

my void say_str(any s) { foreach(chr, unstr(s)) putchar(any2int(chr)); }
my void say(any x) {
  switch(tag_of(x)) {
  case t_str: say_str(x); break;
  case t_cons:
    foreach(e, x)
      say(e);
    break;
  default: print(x);
  }
}

//////////////// read ////////////////

my void parse_error(const char *text) { printf("parse error: %s\n", text); throw(); } // FIXME

my bool allowed_chars[] = { // these can be used for syms in s-exprs
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,1,1,1,0,0,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,
  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,
  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0
}; // disallowed are the first 32 and " #'(),@:;[]`{}|
my bool is_symchar(int c) { return (c >= 0 && c < 256) ? allowed_chars[c] : c!=EOF; }

my FILE *src;
#define nextc() getc(src) // FIXME: should be a defvar
my int look() { int c = nextc(); ungetc(c, src); return c; }

my void skip_until(char end) { int c; do { c = nextc(); } while(c!=end && c!=EOF); }

my int find_token() {
  while(1) { int c = nextc();
    switch(c) {
    case ';': skip_until('\n'); break;
    case ' ': case '\t': case '\n': case '\f': case '\r': break;
    default: return c;
    }
  }
}
my int digit2int (any chr) { int dig = any2int(chr) - '0'; return (dig >= 0 && dig <= 9) ? dig : -1; }

my any chars2num(any chrs) {
  int ires = 0, pos = 0; bool is_positive = true, is_num = false; // need `is_num` to catch "", "+" and "-"
  foreach(chr, chrs) {
    int dig = digit2int(chr); pos++;
    if(dig == -1) {
      if(pos != 1) return BFALSE;
      if(any2int(chr) == '-') { is_positive = false; continue; }
      if(any2int(chr) == '+') continue;
      return BFALSE;
    }
    is_num = true; ires *= 10; ires += dig;
  }
  return !is_num ? BFALSE : int2any(is_positive ? ires : -ires);
}
my any chars_to_num_or_sym(any cs) { any num = chars2num(cs); return is(num) ? num : intern_from_chars(cs); }

my any read_sym_chars(int start_char) {
  listgen lg = listgen_new();
  listgen_add(&lg, int2any(start_char));
  int c;
  while(is_symchar(c = look()))
    listgen_add(&lg, int2any(nextc()));
  return lg.xs;
}

my any read_str() {
  listgen lg = listgen_new();
  while(1) { int c = nextc();
    if(c == '"') return str(lg.xs);
    if(c == EOF) parse_error("end of file inside of a str");
    if(c == '\\')
      switch(c = nextc()) {
      case '\\': case '"': break;
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      case EOF: parse_error("end of file after backslash in str");
      default: parse_error("invalid character after backslash in str");
      }
    listgen_add(&lg, int2any(c)); }
}
my any reader(); // for mutual recursion
my any read_list() {
  any x = reader();
  if(x == READER_LIST_END) return NIL;
  if(x == ENDOFFILE) parse_error("end of file in list (use `M-x check-parens`)");
  if(x == s_dot) {
    x = reader();
    if(reader() != READER_LIST_END)
      parse_error("invalid improper list");
    return x;
  }
  return cons(x, read_list());
}
my any short_lambda_parser(any *body) {
  any x = reader();
  if(is_cons(x)) { *body = x; return NIL; }
  if(!is_sym(x)) { parse_error("invalid lambda short form (expected sym)"); }
  if(x == s_dot) { any rest = reader(); *body = reader(); return rest; }
  return cons(x, short_lambda_parser(body));
}
my any read_lambda_short_form() {
  any body, args = short_lambda_parser(&body);
  return cons(s_lambda, cons(args, single(body)));
}
my any read_unquote() {
  any q = s_unquote;
  int c = look();
  if(c == '@') {
    nextc();
    q = s_unquote_splicing; }
  return cons(q, reader());
}
my any reader() {
  int c = find_token();
  switch(c) {
  case ')': return READER_LIST_END;
  case '(': return read_list();
  case '|': return read_lambda_short_form();
  case '\'': return cons(s_quote, reader());
  case '`': return cons(s_quasiquote, reader());
  case ',': return read_unquote();
  case '"': return read_str();
  case '#':
    switch(c = nextc()) {
    case 'f': return BFALSE;
    case 't': return BTRUE;
    case '!': skip_until('\n'); return reader(); // ignore Unix-style script header
    default: parse_error("invalid character after #");
    }
  case EOF: return ENDOFFILE;
  default: return(chars_to_num_or_sym(read_sym_chars(c)));
  }
}
my any bone_read() {
  any x = reader();
  if(x == READER_LIST_END) parse_error("unexpected closing parenthesis (use `M-x check-parens`)");
  return x;
}

//////////////// bindings ////////////////

my void add_name(hash namespace, any name, bool overwritable, any val) {
  any prev = hash_get(namespace, name);
  if(is(prev) && far(prev)==BINDING_DEFINED) generic_error("already defined", name);
  if(is_sub(val)) name_sub(any2sub(val), name);
  reg_permanent();
  hash_set(namespace, name, cons(overwritable?BINDING_EXISTS:BINDING_DEFINED, val));
  reg_pop();
}

my hash bindings; // FIXME: does it need mutex protection? -> yes, but we use it only at compile-time anyway
my any get_binding(any name) { return hash_get(bindings, name); }
my void bind(any name, bool overwritable, any subr) { add_name(bindings, name, overwritable, subr); }
my bool is_bound(any name) { return get_binding(name) != BFALSE; }

my hash macros; // FIXME: needs mutex protection, see above
my void mac_bind(any name, bool overwritable, any subr) { add_name(macros, name, overwritable, subr); }
my any get_mac(any name) { return hash_get(macros, name); }
my bool is_mac_bound(any name) { return get_mac(name) != BFALSE; }

// These are thread-local, therefore we have to look them up each time
my hash dynamics; // FIXME: dynamics should be thread local (which is why we can't use `bindings` for this)
my any get_dyn(any name) { return hash_get(dynamics, name); }
my bool is_dyn_bound(any name) { return get_dyn(name) != VAR_UNBOUND; }
my void set_dyn(any name, any x) { hash_set(dynamics, name, x); }

my void create_dyn(any name, any x) {
  if(is_dyn_bound(name))
    generic_error("dynamic var bound twice", name);
  set_dyn(name, x);
}

my any get_existing_dyn(any name) {
  any x = get_dyn(name);
  if(x==VAR_UNBOUND)
    generic_error("dynamic var unbound", name);
  return x;
}

//////////////// evaluator ////////////////

typedef enum {
  OP_CONST = 1, OP_GET_ENV, OP_GET_ARG, OP_SET_LOCAL, OP_WRAP, OP_PREPARE_CALL, OP_CALL, OP_TAILCALL, OP_ADD_ARG,
  OP_JMP_IFN, OP_JMP, OP_RET, OP_PREPARE_SUB, OP_ADD_ENV, OP_MAKE_SUB, OP_MAKE_RECURSIVE, OP_DYN
} opcode;

my any last_value;
void bone_result(any x) { last_value = x; }
my any locals_stack[1024];
my any *locals_sp;

my any *alloc_locals(int n) {
  any *res = locals_sp;
  locals_sp += n;
  return res;
}

my void drop_locals(int n) { locals_sp -= n; }

struct call_stack_entry { sub subr; any *args; int tail_calls; } call_stack[256], *call_sp;
my bool is_self_evaluating(any x) { return !(is_sym(x) || is_cons(x)); }
my void print_arg(any x) {
  if(!is_self_evaluating(x))
    printf("'");
  print(x);
}
my void backtrace() {
  printf("BACKTRACE:\n");
  for(struct call_stack_entry *e = call_sp; e > call_stack; e--) {
    printf("(");
    if(is(e->subr->code->name))
      print(e->subr->code->name);
    else
      printf("<unknown>");

    int i;
    for(i = 0; i != e->subr->code->argc; i++) {
      printf(" ");
      print_arg(e->args[i]);
    }
    if(e->subr->code->take_rest)
      foreach(x, e->args[i]) {
	printf(" "); print_arg(x);
      }
    printf(")\n");
    if(e->tail_calls)
      printf(";; hidden tail calls: %d\n", e->tail_calls);
  }
}

struct upcoming_call {
  sub to_be_called;
  int locals_cnt;
  int nonrest_args_left; any rest_constructor;
  any *args, *next_arg; // `next_arg` points into `args`.
} upcoming_calls[256], *next_call; // stack pointer

my void args_error(sub_code sc, any xs) { generic_error("wrong number of args", cons(sc->name, xs)); }
my void args_error_unspecific(sub_code sc) { args_error(sc, single(intern("..."))); }

my void add_nonrest_arg() { *(next_call->next_arg++) = last_value; }
my void add_rest_arg() {
  sub_code sc = next_call->to_be_called->code;
  if(!sc->take_rest) args_error_unspecific(sc);
  if(next_call->rest_constructor == NIL) { // first rest arg
    next_call->rest_constructor = next_call->args[sc->argc] = single(last_value);
  } else { // adding another rest arg
    any next = single(last_value); set_fdr(next_call->rest_constructor, next); next_call->rest_constructor = next;
  }
}
my void verify_argc(struct upcoming_call *the_call) {
  if(the_call->nonrest_args_left)
    args_error_unspecific(the_call->to_be_called->code);
}
my void call(sub subr, any *args, int locals_cnt) {
  sub lambda; any *lambda_envp;
  call_sp++; call_sp->subr = subr; call_sp->args = args; call_sp->tail_calls = 0;
start:;
  any *env = subr->env;
  any *ip = subr->code->ops;
  while(1)
    switch(*ip++) {
    case OP_CONST: last_value = *ip++; break;
    case OP_GET_ENV: last_value = env[any2int(*ip++)]; break;
    case OP_GET_ARG: last_value = args[any2int(*ip++)]; break; // args+locals
    case OP_SET_LOCAL: args[any2int(*ip++)] = last_value; break;
    case OP_WRAP: ((csub) *ip)(args); goto cleanup;
    case OP_PREPARE_CALL: {
      sub to_be_called = any2sub(last_value);
      sub_code sc = to_be_called->code;
      next_call++;
      next_call->to_be_called = to_be_called;
      next_call->nonrest_args_left = sc->argc;
      next_call->locals_cnt = count_locals(sc);
      next_call->next_arg = next_call->args = alloc_locals(next_call->locals_cnt);
      if(sc->take_rest) { next_call->rest_constructor = next_call->args[sc->argc] = NIL; }
      break;
    }
    case OP_CALL: {
      struct upcoming_call *the_call = next_call--;
      verify_argc(the_call);
      call(the_call->to_be_called, the_call->args, the_call->locals_cnt);
      break;
    }
    case OP_TAILCALL: {
      struct upcoming_call *the_call = next_call--;
      verify_argc(the_call);
      locals_cnt = the_call->locals_cnt;
      for(int i = 0; i < locals_cnt; i++)
	args[i] = the_call->args[i];
      locals_sp = &args[locals_cnt];
      subr = the_call->to_be_called;
      call_sp->subr = subr;
      call_sp->args = args;
      call_sp->tail_calls++;
      goto start;
    }
    case OP_ADD_ARG:
      if(next_call->nonrest_args_left) {
	next_call->nonrest_args_left--;
	add_nonrest_arg();
      } else
	add_rest_arg();
      break;
    case OP_JMP_IFN: if(is(last_value)) { ip++; break; } // else fall through
    case OP_JMP: ip += any2int(*ip); break;
    case OP_RET: goto cleanup;
    case OP_PREPARE_SUB: {
      sub_code lc = (sub_code) *ip++;
      lambda = (sub) reg_alloc(1+lc->size_of_env);
      lambda->code = lc;
      lambda_envp = lambda->env;
      break;
    }
    case OP_ADD_ENV: *(lambda_envp++) = last_value; break;
    case OP_MAKE_SUB: last_value = sub2any(lambda); break;
    case OP_MAKE_RECURSIVE: any2sub(last_value)->env[any2int(ip[-2])] = last_value; break; // follows after OP_SET_LOCAL
    case OP_DYN: last_value = get_existing_dyn(*ip++); break;
    default: printf("unknown vm instruction\n"); abort(); // FIXME
  }
cleanup:
  call_sp--;
  drop_locals(locals_cnt); 
}

my void apply(any s, any xs) {
  sub subr = any2sub(s); sub_code sc = subr->code; int argc = sc->argc, pos = 0;
  int locals_cnt = count_locals(sc); any *args = alloc_locals(locals_cnt); listgen lg;
  foreach(x, xs) {
    if(pos <  argc) { args[pos] = x; pos++; continue; } // non-rest arg
    if(pos == argc) {
      // starting rest args
      if(!sc->take_rest) args_error(sc, xs);
      lg = listgen_new();
      listgen_add(&lg, x);
      args[pos] = lg.xs;
      pos++;
      continue;
    }
    // adding another rest arg
    listgen_add(&lg, x);
    pos++;
  }
  if(pos < argc) args_error(sc, xs);
  if(pos == argc) args[argc] = NIL;
  call(subr, args, locals_cnt);
}
my void call0(any subr) { apply(subr, NIL); }
my void call1(any subr, any x) { apply(subr, single(x)); }

//////////////// compiler ////////////////

my any mac_expand_1(any x) {
  if(!is_cons(x) || far(x)==s_quote) return x;
  if(is_sym(far(x))) {
    any mac = get_mac(far(x));
    if(is(mac)) {
      apply(fdr(mac), fdr(x));
      return last_value;
    }
  }
  bool changed = false; listgen lg = listgen_new(); any lst = x;
  if(far(x)==s_lambda) {
    listgen_add(&lg, s_lambda);
    listgen_add(&lg, car(fdr(x)));
    lst = fdr(fdr(x));
  } 
  foreach(e, lst) {
    any new = mac_expand_1(e);
    if(new!=e)
      changed=true;
    listgen_add(&lg, new);
  }
  return changed ? lg.xs : x;
}
my any mac_expand(any x) {
  any res; while(1) {
    res = mac_expand_1(x);
    if(res==x)
      return res;
    x = res;
  }
}

typedef struct { any dst; int pos; int max_locals; int curr_locals; int extra_offset; } compile_state;
my int extra_pos(compile_state *s) { return s->curr_locals + s->extra_offset - 1; }

my void emit(any x, compile_state *state) {
  any next = single(x);
  set_fdr(state->dst, next);
  state->dst = next;
  state->pos++;
}

my void compile_expr(any e, any env, bool tail_context, compile_state *state); // decl for mutual recursion
my void compile_if(any e, any env, bool tail_context, compile_state *state) {
  compile_expr(car(e), env, false, state);
  e = fdr(e);
  emit(OP_JMP_IFN, state);
  emit(0, state);
  compile_state to_else_jmp = *state;

  compile_expr(car(e), env, tail_context, state);
  emit(OP_JMP, state);
  emit(0, state);
  e = fdr(e);
  set_far(to_else_jmp.dst, int2any(state->pos + 1 - to_else_jmp.pos));
  compile_state after_then_jmp = *state;

  compile_expr(car(e), env, tail_context, state);
  set_far(after_then_jmp.dst, int2any(state->pos + 1 - after_then_jmp.pos));
}

my any lambda_ignore_list(any old, any args) {
  listgen lg = listgen_new();
  if(is_sym(args))
    listgen_add(&lg, args);  // only rest arg
  else
    foreach_cons(x, args) {
      listgen_add(&lg, far(x));
      if(!is_cons(fdr(x)) && !is_nil(fdr(x)))
	listgen_add(&lg, fdr(x));
    }

  if(is_nil(lg.last))
    return old;
  set_fdr(lg.last, old);
  return lg.xs;
}

my void found_local(any local, listgen *lg, int *cnt) {
  if(!is(assoc(far(local), lg->xs))) {
    (*cnt)++;
    listgen_add(lg, local);
  }
}

// `locals` is of the form ((foo arg . 0) (bar arg . 1) (baz env . 0))
my void collect_locals_rec(any code, any locals, any ignore, int *cnt, listgen *lg) {
  foreach(x, code)
    switch(tag_of(x)) {
    case t_sym: {
      any local = assoc_entry(x, locals);
      if(is(local) && !is_member(x, ignore)) {
	found_local(local, lg, cnt);
      }
      break;
    }
    case t_cons:
      if(far(x)==s_quote) continue;
      if(far(x)==s_with) { collect_locals_rec(cdr(fdr(x)), locals, cons(car(fdr(x)), ignore), cnt, lg); continue; }
      if(far(x)==s_lambda) { collect_locals_rec(cdr(fdr(x)), locals, lambda_ignore_list(ignore, car(fdr(x))), cnt, lg); continue; }
      collect_locals_rec(x, locals, ignore, cnt, lg);
      break;
    default:;
    }
}

my any collect_locals(any code, any locals, any ignore, int *cnt) {
  listgen res = listgen_new();
  collect_locals_rec(code, locals, ignore, cnt, &res);
  return res.xs;
}
my any add_local(any env, any name, any kind, int num) { return cons(cons(name, cons(kind, int2any(num))), env); }
my any locals_for_lambda(any env, any args) {
  any res = NIL; int cnt = 0;
  foreach(x, env) res = add_local(res, far(x), s_env, cnt++);
  cnt = 0;
  foreach(x, args) res = add_local(res, x, s_arg, cnt++);
  return res;
}

my any flatten_rest_x(any xs, int *len, int *take_rest) { // stores len w/o rest in `*len`.
  if(is_sym(xs)) { *take_rest = 1; return single(xs); } // only rest args
  foreach_cons(x, xs) {
    (*len)++;
    any tail = fdr(x);
    if(is_sym(tail)) {
      set_fdr(x, single(tail));
      *take_rest = 1;
      return xs;
    }
  }
  *take_rest = 0;
  return xs;
}

my sub_code compile2sub_code(any expr, any env, int argc, int take_rest, int env_size);
my void compile_lambda(any args, any body, any env, compile_state *state) {
  int argc = 0, take_rest;
  args = flatten_rest_x(args, &argc, &take_rest);
  int collected_env_len = 0;
  any collected_env = collect_locals(cons(s_do, body), env, args, &collected_env_len);
  any env_of_sub = locals_for_lambda(collected_env, args);
  if(is_nil(body)) generic_error("body of lambda expression is empty", body);
  sub_code sc = compile2sub_code(cons(s_do, body), env_of_sub, argc, take_rest, collected_env_len);
  emit(OP_PREPARE_SUB, state);
  emit((any) sc, state);

  foreach(x, collected_env) {
    any env_or_arg = far(fdr(x));
    any pos = fdr(fdr(x));

    emit(env_or_arg==s_arg ? OP_GET_ARG : OP_GET_ENV, state);
    emit(pos, state);
    emit(OP_ADD_ENV, state);
  }
  emit(OP_MAKE_SUB, state);
}

my void compile_do(any body, any env, bool tail_context, compile_state *state) {
  foreach_cons(x, body)
    compile_expr(far(x), env, is_nil(fdr(x)) && tail_context, state);
}

my bool arglist_contains(any args, any name) {
  if(is_nil(args)) return false;
  if(is_sym(args)) return args==name;
  if(car(args)==name) return true;
  return arglist_contains(fdr(args), name);
}

my bool refers_to(any expr, any name) {
  if(is_sym(expr)) return expr==name;
  if(!is_cons(expr)) return false;
  if(far(expr)==s_quote) return false;
  if(far(expr)==s_with) {
    if(car(fdr(expr))==name)
      return false;
    return refers_to(fdr(fdr(expr)), name);
  }
  if(far(expr)==s_lambda) {
    if(arglist_contains(car(fdr(expr)), name))
      return false;
    return refers_to(fdr(fdr(expr)), name);
  }

  foreach(x, expr)
    if(refers_to(x, name))
      return true;
  return false;
}

my void compile_with(any name, any expr, any body, any env, bool tail_context, compile_state *state) {
  state->curr_locals++; if(state->curr_locals > state->max_locals) state->max_locals = state->curr_locals;
  env = add_local(env, name, s_arg, extra_pos(state)); compile_expr(expr, env, false, state);
  emit(OP_SET_LOCAL, state); emit(int2any(extra_pos(state)), state);
  if(refers_to(expr, name)) emit(OP_MAKE_RECURSIVE, state); compile_do(body, env, tail_context, state); state->curr_locals--;
}
my void compile_expr(any e, any env, bool tail_context, compile_state *state) {
  switch(tag_of(e)) {
  case t_num: case t_uniq: case t_str: case t_sub: case t_other: case t_reg:
    emit(OP_CONST, state);
    emit(e, state);
    break;
  case t_cons: {
    any first = far(e); any rest = fdr(e);
    if(first==s_quote) { emit(OP_CONST, state); emit(rest, state); break; } // FIXME: copy() to permanent?
    if(first==s_do) { compile_do(rest, env, tail_context, state); break; }
    if(first==s_if) { compile_if(rest, env, tail_context, state); break; }
    if(first==s_lambda) { compile_lambda(car(rest), cdr(rest), env, state); break; }
    if(first==s_with) { compile_with(car(rest), car(cdr(rest)), cdr(cdr(rest)), env, tail_context, state); break; }
    compile_expr(first, env, false, state); emit(OP_PREPARE_CALL, state);
    foreach(arg, rest) { compile_expr(arg, env, false, state); emit(OP_ADD_ARG, state); }
    emit(tail_context ? OP_TAILCALL : OP_CALL, state); break;
    }
  case t_sym: {
    any local = assoc(e, env);
    if(is(local)) {
      emit(far(local) == s_arg ? OP_GET_ARG : OP_GET_ENV, state);
      emit(fdr(local), state);
      break;
    }
    any global = get_binding(e);
    if(is_cons(global)) {
      emit(OP_CONST, state);
      emit(fdr(global), state);
      break;
    }
    any dyn = get_dyn(e);
    if(dyn!=VAR_UNBOUND) {
      emit(OP_DYN, state);
      emit(e, state);
      break;
    }
    generic_error("unbound sym", e);
    break;
  }
  }
}
my any compile2list(any expr, any env, int extra_offset, int *extra_locals) {
  any res = single(BFALSE);
  compile_state state = {res,0,0,0,extra_offset};
  compile_expr(expr, env, true, &state);
  emit(OP_RET, &state);
  *extra_locals = state.max_locals;
  return fdr(res);
}
my sub_code compile2sub_code(any expr, any env, int argc, int take_rest, int env_size) { // result is in permanent region.
  int extra; any raw = compile2list(expr, env, argc+take_rest, &extra);
  reg_permanent();
  sub_code code = make_sub_code(argc, take_rest, extra, env_size, len(raw));
  reg_pop();
  any *p = code->ops; foreach(x, raw) *p++ = x; return code;
}
my sub_code compile_toplevel_expr(any e) {
  sub_code res = compile2sub_code(mac_expand(e), NIL, 0, 0, 0);
  name_sub((sub) &res, intern("<top>"));
  return res;
}
my void eval_toplevel_expr(any e) { sub_code code = compile_toplevel_expr(e); call0(sub2any((sub) &code)); }

//////////////// quasiquote ////////////////

my any quasiquote(any x);
my any qq_list(any x) {
  if(!is_cons(x)) return cons(s_quote, single(x));
  if(far(x)==s_unquote) return cons(s_list, single(fdr(x)));
  if(far(x)==s_unquote_splicing) return fdr(x);
  if(far(x)==s_quasiquote) return qq_list(quasiquote(fdr(x)));
  return cons(s_list, single(cons(s_cat, cons(qq_list(far(x)), single(quasiquote(fdr(x)))))));
}
my any qq_id(any x) { return !is_sym(x) ? x : cons(s_quote, x); }
my any quasiquote(any x) {
  if(!is_cons(x)) return qq_id(x);
  if(far(x)==s_unquote) return fdr(x);
  if(far(x)==s_unquote_splicing) generic_error("invalid quasiquote form", x);
  if(far(x)==s_quasiquote) return quasiquote(quasiquote(fdr(x)));
  return cons(s_cat, cons(qq_list(far(x)), single(quasiquote(fdr(x)))));
}

//////////////// library ////////////////

DEFSUB(fastplus) { last_value = int2any(any2int(args[0]) + any2int(args[1])); }
DEFSUB(fullplus) {
  int ires = 0;
  foreach(n, args[0])
    ires += any2int(n);
  last_value = int2any(ires);
}
DEFSUB(cons) { last_value = cons(args[0], args[1]); }
DEFSUB(print) { print(args[0]); last_value = single(args[0]); }
DEFSUB(apply) { apply(args[0], move_last_to_rest_x(args[1])); }
DEFSUB(id) { last_value = args[0]; }
DEFSUB(nilp) { last_value = to_bool(args[0] == NIL); }
DEFSUB(eqp) { last_value = to_bool(args[0] == args[1]); }
DEFSUB(not) { last_value = to_bool(args[0] == BFALSE); }
DEFSUB(car) { last_value = car(args[0]); }
DEFSUB(cdr) { last_value = cdr(args[0]); }
DEFSUB(consp) { last_value = to_bool(is_tagged(args[0], t_cons)); }
DEFSUB(symp)  { last_value = to_bool(is_tagged(args[0], t_sym)); }
DEFSUB(subp)  { last_value = to_bool(is_tagged(args[0], t_sub)); }
DEFSUB(nump)  { last_value = to_bool(is_tagged(args[0], t_num)); }
DEFSUB(strp)  { last_value = to_bool(is_tagged(args[0], t_str)); }
DEFSUB(str) { last_value = str(args[0]); }
DEFSUB(unstr) { last_value = unstr(args[0]); }
DEFSUB(len) { last_value = int2any(len(args[0])); }
DEFSUB(assoc) { last_value = assoc(args[0], args[1]); }
DEFSUB(intern) { last_value = intern_from_chars(unstr(args[0])); }
DEFSUB(copy) { last_value = copy(args[0]); }
DEFSUB(say) { foreach(x, args[0]) say(x); last_value = args[0]; }
DEFSUB(fastminus) { last_value = int2any(any2int(args[0]) - any2int(args[1])); }
DEFSUB(fullminus) {
  int res = any2int(args[0]);
  foreach(x, args[1])
    res -= any2int(x);
  last_value = int2any(res);
}
DEFSUB(fast_num_eqp)  { last_value = to_bool(any2int(args[0]) == any2int(args[1])); }
DEFSUB(fast_num_neqp) { last_value = to_bool(any2int(args[0]) != any2int(args[1])); }
DEFSUB(fast_num_gtp)  { last_value = to_bool(any2int(args[0]) >  any2int(args[1])); }
DEFSUB(fast_num_ltp)  { last_value = to_bool(any2int(args[0]) <  any2int(args[1])); }
DEFSUB(fast_num_geqp) { last_value = to_bool(any2int(args[0]) >= any2int(args[1])); }
DEFSUB(fast_num_leqp) { last_value = to_bool(any2int(args[0]) <= any2int(args[1])); }
DEFSUB(each) {
  check(args[1], t_sub);
  foreach(x, args[0])
    call1(args[1], x);
}
DEFSUB(fastmult) { last_value = int2any(any2int(args[0]) * any2int(args[1])); }
DEFSUB(fullmult) {
  int ires = 1;
  foreach(n, args[0])
    ires *= any2int(n);
  last_value = int2any(ires);
}
DEFSUB(fastdiv) { last_value = int2any(any2int(args[0]) / any2int(args[1])); }
DEFSUB(fulldiv) { CSUB_fullmult(&args[1]); last_value = int2any(any2int(args[0]) / any2int(last_value)); }
DEFSUB(listp) { last_value = to_bool(is_cons(args[0]) || is_nil(args[0])); }
DEFSUB(cat2) { last_value = cat2(args[0], args[1]); }
DEFSUB(in_reg) {
  reg_push(reg_new());
  call0(args[0]);
  last_value = copy_back(last_value);
  reg_free(reg_pop());
}
DEFSUB(bind) { bind(args[0], is(args[1]), args[2]); }
DEFSUB(assoc_entry) { last_value = assoc_entry(args[0], args[1]); }
DEFSUB(str_eql) { last_value = to_bool(str_eql(args[0], args[1])); }
DEFSUB(str_neql) { last_value = to_bool(!str_eql(args[0], args[1])); }
DEFSUB(list_star) { last_value = move_last_to_rest_x(args[0]); }
DEFSUB(memberp) { last_value = to_bool(is_member(args[0], args[1])); }
DEFSUB(reverse) { last_value = reverse(args[0]); }
DEFSUB(mod) { last_value = int2any(any2int(args[0]) % any2int(args[1])); }
DEFSUB(full_num_eqp) { last_value = BTRUE; if(is_nil(args[0])) return; int32_t n = any2int(far(args[0]));
  foreach(x, fdr(args[0])) if(n != any2int(x)) { last_value = BFALSE; return; } }
DEFSUB(full_num_gtp) { last_value = BTRUE; if(is_nil(args[0])) return; int32_t n = any2int(far(args[0]));
  foreach(x, fdr(args[0])) { int32_t m = any2int(x); if(n <= m) { last_value = BFALSE; return; } n = m; } }
DEFSUB(full_num_ltp) { last_value = BTRUE; if(is_nil(args[0])) return; int32_t n = any2int(far(args[0]));
  foreach(x, fdr(args[0])) { int32_t m = any2int(x); if(n >= m) { last_value = BFALSE; return; } n = m; } }
DEFSUB(full_num_geqp) { last_value = BTRUE; if(is_nil(args[0])) return; int32_t n = any2int(far(args[0]));
  foreach(x, fdr(args[0])) { int32_t m = any2int(x); if(n < m) { last_value = BFALSE; return; } n = m; } }
DEFSUB(full_num_leqp) { last_value = BTRUE; if(is_nil(args[0])) return; int32_t n = any2int(far(args[0]));
  foreach(x, fdr(args[0])) { int32_t m = any2int(x); if(n > m) { last_value = BFALSE; return; } n = m; } }
DEFSUB(bit_not) { last_value = int2any(~any2int(args[0])); }
DEFSUB(bit_and) { last_value = int2any(any2int(args[0])&any2int(args[1])); }
DEFSUB(bit_or)  { last_value = int2any(any2int(args[0])|any2int(args[1])); }
DEFSUB(bit_xor) { last_value = int2any(any2int(args[0])^any2int(args[1])); }
DEFSUB(quasiquote) { last_value = quasiquote(args[0]); }
DEFSUB(mac_expand_1) { last_value = mac_expand_1(args[0]); }
DEFSUB(mac_bind) { mac_bind(args[0], is(args[1]), args[2]); }
DEFSUB(mac_expand) { last_value = mac_expand(args[0]); }
DEFSUB(boundp) { last_value = to_bool(is_bound(args[0])); }
DEFSUB(mac_bound_p) { last_value = to_bool(is_mac_bound(args[0])); }
DEFSUB(eval) { eval_toplevel_expr(args[0]); }
DEFSUB(gensym) { last_value = gensym(); }
DEFSUB(map) {
  any s=args[0];
  check(s, t_sub);
  listgen lg=listgen_new();
  foreach(x, args[1]) {
    call1(s, x);
    listgen_add(&lg, last_value);
  }
  last_value=lg.xs;
}
DEFSUB(filter) {
  any s = args[0];
  listgen lg = listgen_new();
  foreach(x, args[1]) {
    call1(s, x);
    if(is(last_value))
      listgen_add(&lg, x);
  }
  last_value=lg.xs;
}
DEFSUB(full_cat) {
  listgen lg = listgen_new();
  foreach_cons(c, args[0])
    if(is_cons(c) && is_nil(fdr(c))) {
      listgen_set_tail(&lg, far(c));
      break;
    } else
      listgen_add_list(&lg, far(c));
  last_value=lg.xs;
}
DEFSUB(refers_to) { last_value = to_bool(refers_to(args[0], args[1])); }
DEFSUB(load) { bone_load(symtext(args[0])); }
DEFSUB(var_bind) { create_dyn(args[0], args[1]); }
DEFSUB(with_var) {
  bool failed = false;
  any old = get_existing_dyn(args[0]);
  set_dyn(args[0], args[1]);
  try { call0(args[2]); } catch { failed = true; }
  set_dyn(args[0], old);
  if(failed) throw();
}
DEFSUB(var_bound_p) { last_value = to_bool(is_dyn_bound(args[0])); }
DEFSUB(var_bang) { set_dyn(args[0], args[1]); }
DEFSUB(reg_loop) {
  reg_push(reg_new());
  call0(args[0]);

  while(1) {
    reg old = reg_pop();
    reg_push(reg_new());
    any sub_args = copy(last_value);
    reg_free(old);
    apply(args[1], sub_args);
    if(!is(car(last_value)))
      break;
    last_value = fdr(last_value);
  }
  last_value = copy_back(fdr(last_value));
  reg_free(reg_pop());
}

my any make_csub(csub cptr, int argc, int take_rest) {
  sub_code code = make_sub_code(argc, take_rest, 0, 0, 2);
  code->ops[0] = OP_WRAP;
  code->ops[1] = (any) cptr;
  sub subr = (sub) reg_alloc(1);
  subr->code = code;
  return sub2any(subr);
}
void bone_register_csub(csub cptr, const char *name, int argc, int take_rest) {
  bind(intern(name), false, make_csub(cptr, argc, take_rest));
}
my void register_cmac(csub cptr, const char *name, int argc, int take_rest) {
  mac_bind(intern(name), false, make_csub(cptr, argc, take_rest));
}
my void init_csubs() {
  bone_register_csub(CSUB_fastplus, "_fast+", 2, 0);
  bone_register_csub(CSUB_fullplus, "_full+", 0, 1);
  bone_register_csub(CSUB_cons, "cons", 2, 0);
  bone_register_csub(CSUB_print, "print", 1, 0);
  bone_register_csub(CSUB_apply, "apply", 1, 1);
  bone_register_csub(CSUB_id, "id", 1, 0); bone_register_csub(CSUB_id, "list", 0, 1);
  bone_register_csub(CSUB_nilp, "nil?", 1, 0);
  bone_register_csub(CSUB_eqp, "eq?", 2, 0);
  bone_register_csub(CSUB_not, "not", 1, 0);
  bone_register_csub(CSUB_car, "car", 1, 0);
  bone_register_csub(CSUB_cdr, "cdr", 1, 0);
  bone_register_csub(CSUB_consp, "cons?", 1, 0);
  bone_register_csub(CSUB_symp, "sym?", 1, 0);
  bone_register_csub(CSUB_subp, "sub?", 1, 0);
  bone_register_csub(CSUB_nump, "num?", 1, 0);
  bone_register_csub(CSUB_strp, "str?", 1, 0);
  bone_register_csub(CSUB_str, "str", 1, 0);
  bone_register_csub(CSUB_unstr, "unstr", 1, 0);
  bone_register_csub(CSUB_len, "len", 1, 0);
  bone_register_csub(CSUB_assoc, "assoc?", 2, 0);
  bone_register_csub(CSUB_intern, "intern", 1, 0);
  bone_register_csub(CSUB_copy, "copy", 1, 0);
  bone_register_csub(CSUB_say, "say", 0, 1);
  bone_register_csub(CSUB_fastminus, "_fast-", 2, 0);
  bone_register_csub(CSUB_fullminus, "_full-", 1, 1);
  bone_register_csub(CSUB_fast_num_eqp, "_fast=?", 2, 0);
  bone_register_csub(CSUB_fast_num_neqp, "<>?", 2, 0);
  bone_register_csub(CSUB_fast_num_gtp, "_fast>?", 2, 0);
  bone_register_csub(CSUB_fast_num_ltp, "_fast<?", 2, 0);
  bone_register_csub(CSUB_fast_num_geqp, "_fast>=?", 2, 0);
  bone_register_csub(CSUB_fast_num_leqp, "_fast<=?", 2, 0);
  bone_register_csub(CSUB_each, "each", 2, 0);
  bone_register_csub(CSUB_fastmult, "_fast*", 2, 0);
  bone_register_csub(CSUB_fullmult, "_full*", 0, 1);
  bone_register_csub(CSUB_fastdiv, "_fast/", 2, 0);
  bone_register_csub(CSUB_fulldiv, "_full/", 1, 1);
  bone_register_csub(CSUB_listp, "list?", 1, 0);
  bone_register_csub(CSUB_cat2, "_fast-cat", 2, 0);
  bone_register_csub(CSUB_in_reg, "_in-reg", 1, 0);
  bone_register_csub(CSUB_bind, "_bind", 3, 0);
  bone_register_csub(CSUB_assoc_entry, "assoc-entry?", 2, 0);
  bone_register_csub(CSUB_str_eql, "str=?", 2, 0);
  bone_register_csub(CSUB_str_neql, "str<>?", 2, 0);
  bone_register_csub(CSUB_list_star, "list*", 0, 1);
  bone_register_csub(CSUB_memberp, "member?", 2, 0);
  bone_register_csub(CSUB_reverse, "reverse", 1, 0);
  bone_register_csub(CSUB_mod, "mod", 2, 0);
  bone_register_csub(CSUB_full_num_eqp, "_full=?", 0, 1);
  bone_register_csub(CSUB_full_num_gtp, "_full>?", 0, 1);
  bone_register_csub(CSUB_full_num_ltp, "_full<?", 0, 1);
  bone_register_csub(CSUB_full_num_geqp, "_full>=?", 0, 1);
  bone_register_csub(CSUB_full_num_leqp, "_full<=?", 0, 1);
  bone_register_csub(CSUB_bit_not, "bit-not", 1, 0);
  bone_register_csub(CSUB_bit_and, "bit-and", 2, 0);
  bone_register_csub(CSUB_bit_or, "bit-or", 2, 0);
  bone_register_csub(CSUB_bit_xor, "bit-xor", 2, 0);
  register_cmac(CSUB_quasiquote, "quasiquote", 0, 1);
  bone_register_csub(CSUB_mac_expand_1, "mac-expand-1", 1, 0);
  bone_register_csub(CSUB_mac_bind, "_mac-bind", 3, 0);
  bone_register_csub(CSUB_mac_expand, "mac-expand", 1, 0);
  bone_register_csub(CSUB_boundp, "bound?", 1, 0);
  bone_register_csub(CSUB_mac_bound_p, "mac-bound?", 1, 0);
  bone_register_csub(CSUB_eval, "eval", 1, 0);
  bone_register_csub(CSUB_gensym, "gensym", 0, 0);
  bone_register_csub(CSUB_map, "map", 2, 0);
  bone_register_csub(CSUB_filter, "filter", 2, 0);
  bone_register_csub(CSUB_full_cat, "_full-cat", 0, 1);
  bone_register_csub(CSUB_refers_to, "_refers-to?", 2, 0);
  bone_register_csub(CSUB_load, "_load", 1, 0);
  bone_register_csub(CSUB_var_bind, "_var-bind", 2, 0);
  bone_register_csub(CSUB_with_var, "_with-var", 3, 0);
  bone_register_csub(CSUB_var_bound_p, "var-bound?", 1, 0);
  bone_register_csub(CSUB_var_bang, "_var!", 2, 0);
  bone_register_csub(CSUB_reg_loop, "_reg-loop", 2, 0);
}

//////////////// misc ////////////////

my any copy(any x) {
  switch(tag_of(x)) {
  case t_cons: return cons(copy(far(x)), copy(fdr(x))); // FIXME: optimize
  case t_str: return str(copy(unstr(x)));
  case t_sym: case t_num: case t_uniq: return x;
  case t_sub: return copy_sub(x);
  default: return x; } // FIXME: should be an error
}

my void bone_init_thread() {
  call_sp = call_stack; call_stack->subr = NULL; call_stack->tail_calls = 0; // FIXME: all needed?
  locals_sp = locals_stack;
  next_call = upcoming_calls;
}

my any add_info_entry(const char *name, int n, any prev) { return cons(cons(intern(name), single(int2any(n))), prev); } 
void bone_init(int argc, char **argv) {
  blocksize = sysconf(_SC_PAGESIZE); blockmask = ~(blocksize - 1); blockwords = blocksize/sizeof(any);
  free_block = fresh_blocks();
  permanent_reg = reg_new(); reg_stack[0] = permanent_reg; load_reg(permanent_reg);
  sym_ht = hash_new(997, (any) NULL); init_syms();
  bindings = hash_new(997, BFALSE); macros = hash_new(397, BFALSE); init_csubs(); dynamics = hash_new(97, VAR_UNBOUND);
  { any lisp_info = add_info_entry("major-version", BONE_MAJOR, NIL);
    lisp_info = add_info_entry("minor-version", BONE_MINOR, lisp_info);
    lisp_info = add_info_entry("patch-version", BONE_PATCH, lisp_info);
    set_dyn(intern("_*lisp-info*"), lisp_info); }
  src = stdin;
  any args = NIL; while(argc--) args = cons(charp2str(argv[argc]), args); set_dyn(intern("*program-args*"), args); 
  bone_init_thread();
}

my char *mod2file(const char *mod) {
  char *res = malloc(strlen(mod) + 4);
  strcat(strcpy(res, mod), ".bn");
  return res;
}
void bone_load(const char *mod) {
  char *fn = mod2file(mod);
  FILE *old = src;
  src = fopen(fn, "r");
  free(fn);
  if(!src) {
    src = old;
    generic_error("could not open module", intern(mod));
  }

  bool fail = false; any e;
  try {
    while((e = bone_read()) != ENDOFFILE)
      eval_toplevel_expr(e);
  } catch { fail = true; }
  fclose(src);
  src = old;
  if(fail) throw();
}

void bone_repl() {
  set_dyn(intern("$"), BFALSE);
  set_dyn(intern("$$"), BFALSE);

  int line = 0;
  while(1) {
    printf("\n@%d: ", line++);
    try {
      any e = bone_read();
      if (e == ENDOFFILE) break;
      eval_toplevel_expr(e);
      print(last_value);
      set_dyn(intern("$$"), get_dyn(intern("$")));
      set_dyn(intern("$"), last_value);
    } catch { call_sp = call_stack; }
  }
  printf("\n");
}
