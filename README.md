The Bone Lisp dialect
=====================

> "They who would give up essential Freedom,  
> to purchase a little Type-Safety,  
> deserve neither Freedom nor Type-Safety."  
> (Benjamin Franklisp)

What?
-----

[ Note: This software is currently in pre-alpha status. ]

Bone is - or rather, will be - an (embeddable or stand-alone)
interpreter for a simple lexically scoped (with optional dynamic
scoping) Lisp-1 based on immutable values, featuring tail call
elimination and using explicit regions for memory management (instead
of garbage collection).  Currently, cons cells are the universal data
structure (no arrays, records or hash tables).

It is inspired by Common Lisp, Pico Lisp, Scheme, Forth and Erlang.

It is currently written for 64 bit systems.  And it requires
little-endian, though both issues are not that hard to fix if desired.

Why?
----

Garbage collection becomes extremely complex internally if you want to
do it well (i.e. avoid to stop the world), but programming with
malloc/free is just too error-prone.  Using explicit regions is both
very simple and very fast, but how far can one get with it?  I want to
find out, so I am developing this interpreter.  I could have tried to
do this with a more innovative language design as well, but:

* most serious programmers already have some knowledge of Lisp
* building a Lisp is not too hard
* it is well known how to build software in Lisp
* I personally enjoy to use Lisp a lot
* an existing language allows me to concentrate on my main goals

For these reasons, it made sense to keep the language rather
traditional and stick with a Lisp.

Bone Lisp could become useful for real-time systems (e.g. as a
scripting language for games), some kinds of multi-threaded servers
and embedded systems.

Who?
----

It is being developed by
Wolfgang Jaehrling (wolfgang at conseptizer dot org)
