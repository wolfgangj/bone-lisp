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

#include <stdint.h>
#include <stdbool.h>

#define BONE_VERSION "0.1"

#define my static
typedef uint64_t any; // we only support 64 bit currently
typedef void (*csub)(any *);

typedef enum { t_cons = 0, t_sym = 1, t_uniq = 2, t_str = 3, t_reg = 4, t_sub = 5, t_num = 6, t_other = 7 } type_tag;
#define UNIQ(n) (t_uniq | (010*(n)))
#define NIL       UNIQ(0)
#define BTRUE     UNIQ(1)
#define BFALSE    UNIQ(2)
#define ENDOFFILE UNIQ(3)

void bone_init();
void bone_load(const char *file);
void bone_repl();
void bone_result(any x);
void bone_register_csub(csub cptr, const char *name, int argc, int take_rest);

#define DEFSUB(name) my void CSUB_ ## name(any *args)

bool is_nil(any x);
bool is(any x);
any to_bool(int x);

int32_t any2int(any x);
any int2any(int32_t n);

bool is_str(any x);
any charp2str(const char *p);
char *str2charp(any x); // created w/ malloc()

char *symtext(any sym);

#endif /* BONE_H */
