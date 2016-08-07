/* bone.h -- The Bone Lisp header file.
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

#ifndef BONE_H
#define BONE_H

#define BONE_MAJOR 0
#define BONE_MINOR 6
#define BONE_PATCH 0
#define BONE_VERSION_EXTRA "-pre"

#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>

#define STRINGIFY1(x) #x
#define STRINGIFY(x) STRINGIFY1(x)
#define BONE_VERSION STRINGIFY(BONE_MAJOR) "." STRINGIFY(BONE_MINOR) "." STRINGIFY(BONE_PATCH) BONE_VERSION_EXTRA

#define my static
typedef uint64_t any; // we only support 64 bit currently
typedef void (*csub)(any *);
typedef enum { t_cons = 0, t_sym = 1, t_uniq = 2, t_str = 3, /*t_unused = 4,*/ t_sub = 5, t_num = 6, t_other = 7 } type_tag;
typedef enum { t_other_src, t_other_dst } type_other_tag;
typedef enum { t_num_int, t_num_float } type_num_tag;
#define BONE_INT_MIN -576460752303423488  /* -(2^59)  */
#define BONE_INT_MAX  576460752303423487  /* 2^59 - 1 */
#define UNIQ(n) (t_uniq | (010*(n)))
#define NIL       UNIQ(0)
#define BTRUE     UNIQ(1)
#define BFALSE    UNIQ(2)
#define ENDOFFILE UNIQ(3)

void bone_init(int argc, char **argv);
void bone_load(const char *file);
void bone_repl();
void bone_result(any x);
void bone_register_csub(csub cptr, const char *name, int argc, int take_rest);

#define DEFSUB(name) my void CSUB_ ## name(any *args)

bool is_nil(any x);
bool is(any x);
any to_bool(bool x);

type_other_tag get_other_type(any x);
void check(any x, type_tag t);

int64_t any2int(any x);
any int2any(int64_t n);

any cons(any a, any d);
my any precons(any a);
any far(any x);
any fdr(any x);
any car(any x);
any cdr(any x);
void set_far(any cell, any x);
void set_fdr(any cell, any x);
bool is_cons(any x);
bool is_single(any x);
any single(any x);
int64_t len(any x);
any list2(any a, any b);
any list3(any a, any b, any c);
#define foreach(var, lst) for(any p_ = (lst), var; is_cons(p_) && (var = far(p_), 1); p_ = fdr(p_))
#define foreach_cons(var, lst) for(any var = (lst); is_cons(var); var = fdr(var))

typedef struct { any xs, last; } listgen;
listgen listgen_new();
void listgen_add(listgen *lg, any x);

void call0(any subr);
void call1(any subr, any x);
void call2(any subr, any x, any y);

bool is_str(any x);
any charp2str(const char *p);
char *str2charp(any x); // created w/ malloc()

any intern(const char *name);
char *symtext(any sym);

any fp2src(FILE *fp, any name);
any fp2dst(FILE *fp, any name);
FILE *src2fp(any x);
FILE *dst2fp(any x);

jmp_buf *begin_try_();
jmp_buf *throw_();
void end_try_();

#define try if(!setjmp(*begin_try_())) {
#define throw() longjmp(*throw_(), 1)
#define catch end_try_(); } else

void bone_info_entry(const char *name, int n);

#endif /* BONE_H */
