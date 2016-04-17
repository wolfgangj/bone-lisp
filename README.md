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

*Note: This software is currently in pre-alpha status.*

Bone is an interpreter for a lexically scoped Lisp-1.
(Dynamic scoping will be added as an option.)
It is based on immutable values and does tail-call elimination.
The special feature that distinguishes it from other Lisps is: 
It uses explicit regions for memory management (instead of garbage collection).
Currently, cons cells are the universal data structure;
I have not decided yet whether I will add arrays, records and hash tables.

It is inspired by Pico Lisp, R5RS Scheme, Forth, Common Lisp, Erlang and Ruby.

It is currently written for 64 bit systems.
It requires little-endian, though both issues are not that hard to fix if desired.
It runs on GNU/Linux and possibly on other Unices.

## Why?

Garbage collection becomes extremely complex internally if you want to do it well (i.e. avoid to stop the world).
But programming with `malloc()` and `free()` is just too error-prone.
Using explicit regions is both very simple and very fast, but how far can one get with it?
I want to find out, so I am developing this interpreter.
I could have tried to do this with a more innovative language design as well, but:

* building a Lisp is not too hard
* it is well known how to build software in Lisp
* I personally enjoy to use Lisp a lot
* it allows me to concentrate on my main goals

Bone Lisp could maybe become useful for soft real-time systems (e.g. as a scripting language for games), some kinds of multi-threaded servers and embedded systems.

## Who?

It is being developed by
Wolfgang Jaehrling (wolfgang at conseptizer dot org)

## Links

Bone Lisp is influenced by:
* [PicoLisp](http://picolisp.com/) is a pragmatic but simple Lisp
* [R5RS Scheme](http://www.schemers.org/Documents/Standards/R5RS/) is a beautiful Lisp dialect
* [Forth](https://en.wikipedia.org/wiki/Forth_%28programming_language%29) is a deep lesson in simplicity
* [Common Lisp](https://common-lisp.net/) is a full-featured traditional Lisp
* [Erlang](http://www.erlang.org/) is a functional language for building scalable real-time systems
* [Ruby](https://www.ruby-lang.org/) is a scripting language with great usability

Somewhat related projects:
* [Pre-Scheme](https://en.wikipedia.org/wiki/PreScheme) is a GC-free (LIFO) subset of Scheme
* [Carp](https://github.com/eriksvedang/Carp) is "a statically typed lisp, without a GC"
* [newLISP](http://www.newlisp.org/) uses "One Reference Only" memory menagement
* [MLKit](http://www.elsman.com/mlkit/) uses region inference (and a GC)
* [Linear Lisp](http://home.pipeline.com/~hbaker1/LinearLisp.html) produces no garbage
* [Dale](https://github.com/tomhrr/dale) is basically C in S-Exprs (but with macros)
* [ThinLisp](http://www.thinlisp.org/) is a subset of Common Lisp that can be used without GC
