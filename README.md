![Bone Lisp](logo.png)

# The Bone Lisp programming language

    (defsub (len xs)
      "Calculate the length of the list `xs`."
      (with loop | remain n
                 (if (nil? remain)
                     n
                   (loop (cdr remain) (++ n)))
        (loop xs 0)))

## What?

*Note: This software is currently in pre-alpha status.*

Bone is an interpreter for a lexically scoped Lisp-1.
(Dynamic scoping will be added as an option.)
It is based on immutable values and does tail-call elimination.
The special feature that distinguishes it from other Lisps is the semi-automatic memory management: 
It uses explicit regions instead of garbage collection.
Currently, cons cells are the universal data structure;
I have not decided yet whether I will add arrays, records and hash tables.

It is inspired by Pico Lisp, R5RS Scheme, Forth, Common Lisp, Erlang and Ruby.

It is currently written for 64 bit systems.
It also requires little-endian, though both issues are not that hard to fix if desired.
It runs on GNU/Linux and possibly on other Unices.

## Why?

Garbage collection becomes extremely complex internally if you want to support multi-threading, avoid pause-times and handle large heaps well.
But programming with `malloc()` and `free()` is just too error-prone.
Using explicit regions is both very simple and very fast, but how far can one get with it?
I want to find out, so I am developing this interpreter.

Bone Lisp could maybe become useful for soft real-time systems (e.g. as a scripting language for games), some kinds of multi-threaded servers and embedded systems.

## Getting started

To make embedding as easy as possible, the whole interpreter is in a single C file.
I normally compile it with:

    $ gcc -std=gnu99 -Wall -W -Wextra -Wno-unused -g bone.c -o bone

## Who?

It is being developed by
Wolfgang Jaehrling (wolfgang at conseptizer dot org)

# Quick Intro

Bone Lisp doesn't try to be an overly innovative Lisp (like e.g. Clojure).
However, we change one important piece of terminology:
We speak about subroutines, not about functions (and reserve the term `function` for pure functions without side-effects).
Usually, we abbreviate this to subs.

To the common syntactic sugar (like `'x` for quoting) we only add a shortcut for anonymous subs:

    | a b c (foo)   ; => (lambda (a b c) (foo))

Rest arguments work like they do in Scheme:

    (lambda args foo)
    (lambda (a b c . args) foo)

Booleans and the empty list work almost like they do in Scheme:
The empty list is written as `()` and is self-evaluating.
While we still call it "nil", it is not the symbol `nil` (which isn't special in any way).
Only the value `#f` is false.
The cannonical value for `true` is `#t`.

The names of predicates end with a question mark (e.g. `nil?`).
Subs which may return a useful value or `#f` (false) also follow this convention (eg. `assoq?`).
This helps to prevent forgetting about the possbility of returning `#f`.

Most library names are taken from Scheme and Common Lisp.
Often, we provide several names for the same thing (like Ruby does).
For example, `len`, `length` and `size` are the same.
See `core.bn` for docstrings describing the builtins.
(In the future we'll have a program that extracts the docstrings and generates a markdown file from them.)

Currently, we have no macros yet, so you'll need to use some internal subroutines.
A subroutine can be defined with `_bind`:

    (_bind 'sum | xs (apply + xs))
    (sum '(1 2 3))  ; => 6

The use of regions is also available only via an internal sub:

    (w/new-reg (lambda () foo))

The given thunk will be evaluated with objects allocated in a new region.
On return, the return value will be copied to the previous region.
Then, the region will be freed.

## License

This is Free Software distributed under the terms of the ISC license.
(A very simple non-copyleft license.)
See the file LICENSE for details.

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
* [newLISP](http://www.newlisp.org/) uses "One Reference Only" memory management
* [MLKit](http://www.elsman.com/mlkit/) uses region inference (and a GC)
* [Linear Lisp](http://home.pipeline.com/~hbaker1/LinearLisp.html) produces no garbage
* [Dale](https://github.com/tomhrr/dale) is basically C in S-Exprs (but with macros)
* [ThinLisp](http://www.thinlisp.org/) is a subset of Common Lisp that can be used without GC
