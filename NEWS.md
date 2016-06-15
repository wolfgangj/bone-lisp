# Release information

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
