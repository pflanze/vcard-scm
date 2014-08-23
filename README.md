# vcard-scm: VCard files from Scheme

This is currently just a VCard generator, not parser. It attempts to
support OpenPGP keys well.

* `vcard.scm`: the definition of the VCard file format.

* `vcard-easy.scm`: an abstraction on top of the bare VCard structures
  that takes care of proper (as far as this author can say) embedding
  of data so that it has the best chance to actually be shown to the
  recipient. (Suggestions for improvements welcome!)

* `qrencode.scm`: generate QR codes from strings by using the qrencode
  command line tool.

* `examples.scm`: how to make use of all of this.


## Installation of dependencies on Debian

    $ sudo apt-get install git gambc build-essential qrencode 

## Installation of dependencies  on other systems

Debian derivates should work as above, otherwise you'll have to find
corresponding packages
([Gambit-C](https://en.wikipedia.org/wiki/Gambit_(scheme_implementation))
Scheme system, C compiler,
[qrencode](http://fukuchi.org/works/qrencode/)). 

NOTE that some of the libraries from chj-schemelib might only work on
Linux (you'll get compilation or test failures in the next
section). Contact [me](http://christianjaeger.ch/contact.html) if
that's the case.


## Installation of vcard-scm

    $ git clone https://github.com/pflanze/vcard-scm
    $ cd vcard-scm
    $ git submodule init; git submodule update
    $ gsc

This will (when run for the first time) compile all of the used
libraries in lib/, and hence be slow. Ignore the warnings. Enter the
following at the "`>` " prompt to run the test suite:

    (run-tests)

It should say "0 failures" at the end. To quit the Scheme repl:

    ,q


## Usage directly

To look at the vcard generated by the examples, enter the Scheme repl
again:

    $ gsc
    > (display (example2 #f)) ;; false for private, 
    > (display (example2 #t)) ;; true for public

To write it out as .vcd and QR code files in the current working
directory:

    > (printcard "example2" (example2 #f))


## Usage from own repo

If you want to keep track of your vcard definition(s) in a separate
Git repository, do the following:

    $ cd ..
    $ git init myrepo
    $ cd myrepo
    $ ln -s ../vcard-scm/.gambcini
    $ mkdir .gambc
    $ cat > .gambc/load.scm # then paste the following:

    (parameterize ((current-directory "../vcard-scm"))
		  (load ".gambc/load.scm"))

    (load "my.scm")

hit ctl-d, 

    $ touch my.scm
    $ git add . ; git commit -m "start"
    $ gsc

open my.scm, add your definition, then

    > (lo) ;; this reloads things
    > (printcard "my" my)

## Enjoy..

..and send me email to the address in the card written by

    > (display (cjcard))
    > (printcard "cjcard" (cjcard))

and tell me whether you like it :)

