# Release information

## 0.5.0

* Support for floating point numbers.
* Basic Unicode support:
  I/O is done in UTF-8.
* New builtin subs/macros:
  `acond`
  `caar?`
  `caar*`
  `cadr?`
  `cadr*`
  `case`
  `cdar?`
  `cdar*`
  `cddr?`
  `cddr*`
  `last`
  `read-line`
  `str*`
  `str-ascii-lower`
  `str-pad`
  `str-padr`
  `str-pos?`
  `str-select`
  `with-dst`
  `with-gensyms`
  `with-src`
  `with-stderr`
  `with-stdin`
  `with-stdout`
* New standard library module `std/prog-args` for parsing program arguments.
  It contains the subs:
  `parse-prog-args`
  `say-prog-args-help`
* New standard library module `std/log` for logging.
  It contains the subs/macros:
  `log`
  `with-log`
* `(lisp-info 'posix)` returns `0` if POSIX module is used (`#f` otherwise).
* New POSIX bindings:
  `call`
  `exec`
  `str-now`
  `sys.ctime?`
  `sys.execvp?`
  `system`
  `wait-for`
* All internal structures are resized dynamically, so there are no arbitrary limits anymore.
* The interpreter takes up less permanent memory space.
* The documentation generator understands basic command line options now.

## 0.4.0

* New standard library module `std/tap` which implements incomplete, but TAP-conformant testing.
  It contains the subs/macros:
  `test`
  `test-error`
  `test-plan`
  `test-plan-end`
* Show origin of anonymous subs in backtrace.
* `if` allows an else-branch of arbitrary size;
  when the `else` branch is empty but would be taken, `#f` is returned.
* Extended fixnum range from 32 to 60 bits.
* Allow mutual recursion: You can `declare` a binding before you define it.
* The stack size is now increased dynamically, so there is no arbitrary limit anymore.
* New reader macro in `std/alist`:
  `#=>` allows convenient notation for association lists.
* Introduced a (limited) destructuring binding construct: `destructure`;
  it can only destructure proper lists currently (without nesting).
* New builtin subs:
  `all?`
  `any?`
  `car*`
  `cdr*`
  `chr-skip`
  `dup`
  `find?`
  `flatten`
  `partial`
  `str-join`
* New POSIX bindings:
  `gettimeofday`
  `sys.gettimeofday?`
  `timeofday-diff`
* New module `std/bench` with subs:
  `measure-time`
  `say-time`
* New subs in `std/math`:
  `count`
  `iota`
  `iota*`
* `mapx` was removed - it was more confusing than helpful.
* `each` got the order of args reversed to be consistent with everything else.

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
