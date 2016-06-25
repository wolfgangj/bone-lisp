# Release information

## 0.3.0

* Support for I/O:
  Read from `*src*`.
  Print to `*dst*`.
  Also define `*stdin*`, `*stdout*` and `*stderr*`.
  Provide macros:
  `with-file-dst`
  `with-file-src`
  `with-stderr`
* You can specify a script file on the command line;
  you can also use Unix `#!` notation to specify the interpreter in the file.
  (If a file starts with `#`, the first line is ignored.)
* Added the documentation generator `gendoc.bn`.
* The reader macro `#eval` allows read-time evaluation.
* New builtin subs and macs:
  `awhen`
  `drop`
  `dropr`
  `dst?`
  `eof?`
  `num->str`
  `sort`
  `src?`
  `str-dropr`
  `str-empty?`
  `str-prefix?`
  `str-take`
  `str-taker`
  `sym->str`
  `take`
  `taker`
* New POSIX bindings:
  `sys.dst-close?`
  `sys.dst-open?`
  `sys.random`
  `sys.src-close?`
  `sys.src-open?`
* New standard library module `std/random` containing:
  `random-choice`
* New standard library module `std/bases` containing the reader macros:
  `#bin`
  `#oct`
  `#hex`
  `#in-base`
* New standard library module `std/alist` containing:
  `simplify-alist`
* Moved these subs from prelude to new standard library module `std/math`:
  `max`
  `min`
  `percent`
* Module `std/math` additionally contains:
  `abs`
  `difference`
  `even?`
  `iota`
  `mean`
  `odd?`
  `sum`
* Read access to dynamically scoped variables is faster as it avoids the hash table lookup.
  Write access is also faster as it only does a single lookup.
* Show line numbers on parse errors.

## 0.2.0

* Dynamic scoping is now possible via `defvar` and `with-var`.
* Reader macros can be registered with `defreader` or `myreader`.
* You can code character constants with the reader macro `#chr`.
* Errors now just bring you back to the REPL like they should.
* The previous values on the REPL are available via `$` and `$$`.
* You can access the command line args via `*program-args*`.
* Load code from source files with `(use file1 file2 ...)`;
  the file extension `.bn` will be added automatically.
  Reload files with `(reload foo)`.
* Possibly endless loops that won't run out of memory can be implemented with `reg-loop`.
* You can do recursive definitions not only with `defsub`, but also with the other binding constructs:
  `mysub`, `internsub`, `defmac`, `mymac` and `internmac`
* All of the above binding constructs allow to avoid the implicit lambda (as in Scheme).
  So you can say: `(mysub atom? (compose not cons?))`
* New library subs:
  `0?`
  `>0?`
  `<0?`
  `assocar?`
  `car?`
  `cat-lists`
  `cdr?`
  `compose`
  `chr-look`
  `chr-read`
  `equal?`
  `err`
  `fold`
  `foldr`
  `lisp-info`
  `max`
  `min`
  `percent`
  `read`
  `reader-bound?`
  `unfold`
  `unfoldr`
  `var-bound?`
  `version`
* Changed argument order of `nth` and `nth-cons`.
* Replaced `str-skip` with `str-drop`.

## 0.1.0

* Initial release
