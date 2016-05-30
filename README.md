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
It is based on immutable values and does tail-call elimination.
The special feature that distinguishes it from other Lisps is the semi-automatic memory management: 
It uses explicit regions instead of garbage collection.

It is inspired by Pico Lisp, R5RS Scheme, Forth, Common Lisp, Erlang and Ruby.

It is currently written for 64 bit systems.
It also requires little-endian, though this can be easiely fixed if desired.
It runs on GNU/Linux and should also work on other Unices.

## Why?

Garbage collection becomes extremely complex internally if you want to support multi-threading, avoid pause-times and handle large heaps well.
But doing manual memory management is time-consuming and usually also error-prone.
Explicit regions are both very simple and very fast, but how far can one get with them?
I want to find out, so I am developing this interpreter.

Bone Lisp could maybe become useful for soft real-time systems (e.g. as a scripting language for games), some kinds of multi-threaded servers and embedded systems.

## Status

### What it does

* Lexical scoping & closures
* Explicit regions memory management
* Tail call elimination
* Lists, strings, fixnums, symbols

### What it does not (yet)

* Macros (highest priority)
* Reader macros
* Multithreading
* Keywords
* Optional dynamic scoping
* I/O streams
* Floating point numbers
* Unicode
* Records / structures
* POSIX bindings

### What it does not (unsure whether it ever will)

These are open for discussion, but I currently have no plans for these.

* Arrays
* Hash tables
* Exceptions
* Module system
* Bignums

### What it does not (and won't)

I have no interest at all in adding these features to Bone Lisp.

* Garbage collection
  (obviously, since the whole point of Bone Lisp is to avoid it)
* Continuations
  (I don't think they make sense with explicit regions)
* Being compatible to other Lisp dialects
* Object oriented programming
  (creating a good object system is hard and thus takes a lot of time I'd rather spend on other features)

## Getting started

To make embedding as easy as possible, the whole interpreter is in a single C file.
I normally compile it with:

    $ gcc -std=gnu99 -Wall -W -Wextra -Wno-unused -g bone.c -o bone

## Who?

It is being developed by
Wolfgang Jaehrling (wolfgang at conseptizer dot org)

## Quick Intro

Bone Lisp doesn't try to be an overly innovative Lisp (like e.g. Clojure), nor does it try hard to be compatible with tradition.
I hope you'll like the few things Bone does different than traditional Lisps.

One important piece of terminology is changed:
Keeping with the times, we reserve the term "function" for pure functions without side-effects.
Since Bone Lisp allows some side-effects (like I/O), we mostly speak about using subroutines in our code.
Usually, we abbreviate "subroutine" as "sub", like it is done in modern BASIC dialects.

To the usual syntactic sugar (like `'x` for quoting) we only add a shortcut for anonymous subs with a single expression in the body:

    | a b c (foo)   ; => (lambda (a b c) (foo))

Rest arguments work like they do in Scheme:

    (lambda args foo)
    (lambda (a b c . args) foo)

Booleans and the empty list work almost like they do in Scheme:

* The empty list is written as `()` and is self-evaluating (whereas in Scheme it can't be evaluated).
* While we still call the empty list "nil", it is not the symbol `nil` (which isn't special in any way).
* You cannot take the `car` and `cdr` of the empty list.
* Only the value `#f` is false.
* The cannonical value for true is `#t`.

The names of predicates end with a question mark (e.g. `nil?`).
Subs which may return a useful value or `#f` (false) also follow this convention (eg. `assoc?`).
This helps to prevent forgetting about the possbility of returning `#f`.

Most names in the library are taken from Scheme and Common Lisp.
Often, we provide several names for the same thing (like Ruby does).
For example, `len`, `length` and `size` are the same.
See `core.bn` for docstrings describing the builtins.
(In the future we'll have a program that extracts the docstrings and generates a markdown file from them.)

Currently, we have no macros yet, so you'll need to use some internal subroutines.
A subroutine can be defined with `_bind`:

    (_bind 'sum | xs (apply + xs))
    (sum '(1 2 3))  ; => 6

Note that the environment is hyperstatic (as in Forth):
If you redefine something, the subs previously defined will continue to use the old definition.

The use of regions is also available only via an internal sub:

    (_w/new-reg (lambda () foo))

The given thunk will be evaluated with objects allocated in a new region.
The return value will be copied to the previous region.
Finally, the new region will be freed.

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

Somewhat related Free Software projects:
* [Pre-Scheme](https://en.wikipedia.org/wiki/PreScheme) is a GC-free (LIFO) subset of Scheme
* [Carp](https://github.com/eriksvedang/Carp) is "a statically typed lisp, without a GC"
* [newLISP](http://www.newlisp.org/) uses "One Reference Only" memory management
* [MLKit](http://www.elsman.com/mlkit/) uses region inference (and a GC)
* [Linear Lisp](http://home.pipeline.com/~hbaker1/LinearLisp.html) produces no garbage
* [Dale](https://github.com/tomhrr/dale) is basically C in S-Exprs (but with macros)
* [ThinLisp](http://www.thinlisp.org/) is a subset of Common Lisp that can be used without GC
