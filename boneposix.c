/* boneposix.c -- POSIX bindings for Bone Lisp.
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

#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "bone.h"

DEFSUB(errno) { bone_result(int2any(errno)); }
DEFSUB(errname) { switch(errno) {
#define x(n) case n: bone_result(intern(#n)); break
    x(EDOM); x(EILSEQ); x(ERANGE); // C99 + POSIX

    x(E2BIG); x(EACCES); x(EADDRINUSE); x(EADDRNOTAVAIL); x(EAFNOSUPPORT); x(EAGAIN); x(EALREADY); x(EBADF); x(EBADMSG); x(EBUSY);
    x(ECANCELED); x(ECHILD); x(ECONNABORTED); x(ECONNREFUSED); x(ECONNRESET); x(EDEADLK); x(EDESTADDRREQ); x(EDQUOT); x(EEXIST);
    x(EFAULT);x(EFBIG); x(EHOSTUNREACH); x(EIDRM); x(EINPROGRESS); x(EINTR); x(EINVAL); x(EIO); x(EISCONN); x(EISDIR); x(ELOOP);
    x(EMFILE); x(EMLINK); x(EMSGSIZE); x(EMULTIHOP); x(ENAMETOOLONG); x(ENETDOWN); x(ENETRESET); x(ENETUNREACH); x(ENFILE); x(ENOBUFS);
    x(ENODATA); x(ENODEV); x(ENOENT); x(ENOEXEC); x(ENOLCK); x(ENOLINK); x(ENOMEM); x(ENOMSG); x(ENOPROTOOPT); x(ENOSPC); x(ENOSR);
    x(ENOSTR); x(ENOSYS); x(ENOTCONN); x(ENOTDIR); x(ENOTEMPTY); x(ENOTSOCK); x(ENOTSUP); x(ENOTTY); x(ENXIO); x(EOVERFLOW); x(EPERM);
    x(EPIPE); x(EPROTO); x(EPROTONOSUPPORT); x(EPROTOTYPE); x(EROFS); x(ESPIPE); x(ESRCH); x(ESTALE); x(ETIME); x(ETIMEDOUT);
    x(ETXTBSY); x(EXDEV); // all of these are POSIX
#ifndef __linux__
    x(EOPNOTSUPP); // same as ENOTSUPP
    x(EWOULDBLOCK); // same as EDOM
#else
    x(EBADE); x(EBADFD); x(EBADR); x(EBADRQC); x(EBADSLT); x(ECHRNG); x(ECOMM); x(EHOSTDOWN); x(EISNAM); x(EKEYEXPIRED);
    x(EKEYREJECTED); x(EKEYREVOKED); x(EL2HLT); x(EL2NSYNC); x(EL3HLT); x(EL3RST); x(ELIBACC); x(ELIBBAD); x(ELIBMAX); x(ELIBSCN);
    x(ELIBEXEC); x(EMEDIUMTYPE); x(ENOKEY); x(ENOMEDIUM); x(ENONET); x(ENOPKG); x(ENOTBLK); x(ENOTUNIQ); x(EPFNOSUPPORT); x(EREMCHG);
    x(EREMOTE); x(EREMOTEIO); x(ERESTART); x(ESHUTDOWN); x(ESOCKTNOSUPPORT); x(ESTRPIPE); x(EUCLEAN); x(EUNATCH); x(EUSERS); x(EXFULL);
    //x(EDEADLOCK); // same as EDEADLK
#endif
#undef x
    default: bone_result(BFALSE); }
}

DEFSUB(getpid) { bone_result(int2any(getpid())); }
DEFSUB(getuid) { bone_result(int2any(getuid())); }
DEFSUB(geteuid) { bone_result(int2any(geteuid())); }
DEFSUB(getgid) { bone_result(int2any(getgid())); }
DEFSUB(getegid) { bone_result(int2any(getegid())); }
my void getenv_any(char *name) { char *res = getenv(name); bone_result(res ? charp2str(res) : BFALSE); }
my void getenv_str(any x) { char *name = str2charp(x); getenv_any(name); free(name); }
my void getenv_sym(any x) { getenv_any(symtext(x)); }
DEFSUB(getenv) { if(is_str(args[0])) getenv_str(args[0]); else getenv_sym(args[0]); }
my void setenv_any(char *name, char *val, any ow) { bone_result(to_bool(!setenv(name, val, is(ow)))); }
my void setenv_str(any x, char *val, any ow) { char *name = str2charp(x); setenv_any(name, val, ow); free(name); }
my void setenv_sym(any x, char *val, any ow) { setenv_any(symtext(x), val, ow); }
DEFSUB(setenv) { char *val = str2charp(args[1]);
  if(is_str(args[0])) setenv_str(args[0], val, args[2]); else setenv_sym(args[0], val, args[2]); free(val); }

void bone_posix_init() {
  bone_register_csub(CSUB_errno, "errno", 0, 0);
  bone_register_csub(CSUB_errname, "errname", 0, 0);
  bone_register_csub(CSUB_getpid, "getpid", 0, 0);
  bone_register_csub(CSUB_getuid, "getuid", 0, 0);
  bone_register_csub(CSUB_geteuid, "geteuid", 0, 0);
  bone_register_csub(CSUB_getgid, "getgid", 0, 0);
  bone_register_csub(CSUB_getegid, "getegid", 0, 0);
  bone_register_csub(CSUB_getenv, "getenv?", 1, 0);
  bone_register_csub(CSUB_setenv, "setenv?", 3, 0); // FIXME: last arg optional
}
