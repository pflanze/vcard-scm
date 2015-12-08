# Explanations for [vcard.scm](vcard.scm)

Since the libraries (lib directory) used by the project is not really
documented (yet), here is an introduction on how to read the code in
[vcard.scm](vcard.scm).

This is the core part of
[vcard-scm](https://github.com/pflanze/vcard-scm), and defines the
data structures representing a
[VCard](https://en.wikipedia.org/wiki/VCard) file and the formatting
of those to strings. It's still somewhat incomplete, but unlike any
other library for the same purpose that I've seen it does enforce
correctness of the produced output through (run-time) type checking
(modulo bugs). Also, for what it does it is very compact compared to
the other libraries I looked at. It achieves this through compile time
code generation using macros, a very tight syntax for generating
classes and class hierarchies, and higher order functions to build
type checkers. This means that the code will also be pretty dense,
i.e. demanding, reading. If you feel like you're progressing slowly,
take into account that you're reading a file that would typically be
perhaps 5 times bigger if written without such abstractions.

It makes use of lots of libraries that I wrote on top of Gambit
Scheme; they are included as a submodule (see installation
instructions in the [README](README.md)), but as mentioned they are
mostly undocumented, so let's go through the special syntax used in
this file instead.

First, let me point out that the Gambit Scheme repl uses `>` as the
prompt. Thus a typical session would look like:

    > (+ 1 2)
    3

Since this can be simply handled first-class as 3 S-expressions, I've
implemented a `TEST` form that accepts inputs of the above form and
will store it as test cases:

    (TEST
     > (+ 1 2)
     3
     > (+ 1 1)
     3)

The tests are stored separate from the compiled binaries during
compilation (more precisely, macro expansion time), and are loaded and
run when running the `run-tests` procedure. If the above is in a file
example.scm, then you would see the following happen in a repl
session:

    > (load "example")
    > (run-tests "example")
    Testing file example.scm
    TEST form no. 0
    *** WARNING IN "/home/chris/vcard-scm/example.scm"@33.4 -- TEST failure, got: 2
    1 success(es), 1 failure(s)

`def` is a shorthand for `define`, but also supports optional run-time
type checks of its arguments by way of giving it a Scheme vector
holding 2 slots (predicate and variable name) instead of just the
variable name:

    > (def (next #(integer? x)) (+ x 1))
    > (next 3)
    4
    > (next 3.4)
    *** ERROR IN next, (console)@19.1 -- x does not match integer?: 3.4

`typed-lambda` is a `lambda` that also allows for type checking,
analogous to def.

`defenum` defines a predicate function knowing about all values an
enumeration contains, and binds the enumerator names to symbols of the
same name:

    > (defenum fruit apple banana pear)
    > apple
    apple
    > (fruit? apple)
    #t
    > (fruit? 'apple)
    #t
    > (fruit? 'foo)
    #f

`defmacro` is an implementation of unhygienic macros. Two special
features it offers are retention of source location information (macro
arguments are the s-expressions in question wrapped to also contain
location information, which is mostly transparent except where a
Scheme builtin is handling it, in which case the actual s-expression
value has to be rid of the source information first, which the
`source-code` function and the `with-source*` forms do), and macro
expansion by way of prefixing a macro call with `expansion#`:

    > (defmacro (foom x y) `(let ,x ,y))
    > (foom (fun) 4)
    *** ERROR IN (console)@28.1 -- Ill-formed special form: let
    > (expansion#foom (fun) 4)
    (let (fun) 4)

`both` and `all-of` (n-ary variant of both), and `maybe` are function
combinators for building predicates from predicates.

The various `...-of` functions are building predicates for collection
types of a contained type. 

`def.` is a variant of `def` that is object oriented: it defines a
method on its first argument based on the type specified as the part
of the function name before the dot; example:

    (def. (gmtime.vcard-date v)
      (vcard-date (.year v)
                  (.month v)
                  (.mday v)))

This defines a `.vcard-date` method that is selected if `v` matches
the predicate `gmtime?`. Its body creates a `vcard-date` object
(defined elsewhere in the file with `subclass vcard-date`) with 3
values, which are derived from v by way of method calls. I'm using the
term "method" here as those dispatch on the type of their first
argument only, and are all named with a leading dot to make that fact
clear; other than that those are simply functions just like
multimethods from CLOS (common lisp object system).

(`def.` in the example above *also* defines a normal function named
`gmtime.vcard-date` that requires its argument to be a gmtime; calling
it fully qualified like `(gmtime.vcard-date foo)` means that the code
really expects the type of `foo` to match gmtime, which can be faster
and more explicitly states the intention. In this example instead of
`(.year v)` I could have written `(gmtime.year v)`, etc.)

This file contains no side-effecting statements with the exception of
one `push!` statement at compile time: `insert-result-of` is run
during compile time and the result of the evaluation is inserted as
code into the compiled file. That form is used here to generate
classes from a list of descriptions of the elements of a
VCard. `compile-time` is forcing the evaluation of its body to compile
time, which means contained definitions can be used by the macro
expander while running on the remainder of the file. `class` and
`subclass` are helper syntax to define class hierarchies, and are
using the same object system that `def.` uses.

    (class foo
           (struct a b)
           (method (add v)
                   (+ (.a v) (.b v))))

is equivalent to:

    (defstruct foo a b)
    (def. (foo.add v)
          (+ (.a v) (.b v)))

`struct` or `defstruct` define the object fields, plus a type
predicate function `foo?`, a constructor function `foo` which takes 2
arguments of any type in this case, and `foo.a` and `foo.b` accessor
methods so that e.g.

    (.a (foo 2 3))

returns `2`. The struct definitions are following the same rules as
`typed-lambda` mentioned above, and allows for DSSSL style argument
qualifiers (`#!optional` means the arguments are optional, `#!key`
means that the subsequent arguments are passed as key/value pairs):

    > (defstruct myds #!key a b)
    > (myds b: 10)
    #(myds #f 10)

`class` can contain nested `subclass` forms, in which case the
predicate function checking the class type will also accept objects of
the subclasses:

    (class animal
           (subclass dog
                     (struct #(string? name))
                     (method (desc d)
                             (string-append "dog " (.name d))))
           (subclass human
                     (struct #(string? given-name)
                             #(string? family-name))
                     (method (desc h)
                             (string-append "human "
                                            (.given-name h)
                                            " "
                                            (.family-name h)))))

    (def animals (list (dog "fifi") (human "Max" "Havel")))

    (TEST
     > (map animal? animals)
     (#t #t)
     > (map dog? animals)
     (#t #f)
     > (map human? animals)
     (#f #t)
     > (map .desc animals)
     ("dog fifi" "human Max Havel"))

`C` is a macro to "cut", i.e. apply a function call partially;

    (C <= 0 _ 60)

is equivalent to

    (lambda (x) (<= 0 x 60))


## Synopsis

I found it interesting to develop all the syntactical features going
into this. It's a bit pointless when other languages like Clojure or
Haskell could be used instead and such and other features produced as
a shared effort can be profited from. Scheme is still the ultimate
substrate to develop new experimental or specialized languages on,
though, there might be cases where this is still useful. It would also
be quite straight-forward to write a translator from vcard.scm to
human readable code in various other programming languages (like C#,
Java, Perl, etc.).
