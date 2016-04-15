![Bone Lisp](logo.png)

# The Bone Lisp programming language

    (defsub (len xs)
      "Calculate the length of the list `xs`."
      (with loop | remaining n
                 (if (nil? remaining)
                     n
                   (loop (cdr remaining) (++ n)))
        (loop xs 0)))

## What?

[ Note: This software is currently in pre-alpha status. ]

Bone is - or rather, will be - an (embeddable or stand-alone)
interpreter for a simple lexically scoped (with optional dynamic
scoping) Lisp-1.  It is based on immutable values, does full tail-call
elimination and uses explicit regions for memory management (instead
of garbage collection).  Currently, cons cells are the universal data
structure (no arrays, records or hash tables).

It is inspired by Pico Lisp, R5RS Scheme, Forth, Common Lisp, Erlang
and Ruby.

It is currently written for 64 bit systems.  And it requires
little-endian, though both issues are not that hard to fix if desired.

## Why?

Garbage collection becomes extremely complex internally if you want to
do it well (i.e. avoid to stop the world), but programming with
`malloc()` and `free()` is just too error-prone.  Using explicit
regions is both very simple and very fast, but how far can one get
with it?  I want to find out, so I am developing this interpreter.  I
could have tried to do this with a more innovative language design as
well, but:

* most serious programmers already have some knowledge of Lisp
* building a Lisp is not too hard
* it is well known how to build software in Lisp
* I personally enjoy to use Lisp a lot
* it allows me to concentrate on my main goals

Bone Lisp could become useful for real-time systems (e.g. as a
scripting language for games), some kinds of multi-threaded servers
and embedded systems.

## Who?

It is being developed by
Wolfgang Jaehrling (wolfgang at conseptizer dot org)

## Links

Somewhat related:
* [Carp](https://github.com/eriksvedang/Carp) is "a statically typed lisp, without a GC"
* [newLISP](http://www.newlisp.org/) uses "One Reference Only" memory menagement
* [MLKit](http://www.elsman.com/mlkit/) uses region inference (and a GC)
* [Linear Lisp](http://home.pipeline.com/~hbaker1/LinearLisp.html) produces no garbage

Bone Lisp is influenced by:
* [PicoLisp](http://picolisp.com/) is a pragmatic but simple Lisp
* [R5RS Scheme](http://www.schemers.org/Documents/Standards/R5RS/) is a beautiful Lisp dialect
* [Forth](https://en.wikipedia.org/wiki/Forth_%28programming_language%29) is a lesson in simplicity
* [Common Lisp](https://common-lisp.net/) is a full-featured traditional Lisp
* [Erlang](http://www.erlang.org/) is a functional language for building scalable real-time systems
* [Ruby](https://www.ruby-lang.org/) is a scripting language with great usability
