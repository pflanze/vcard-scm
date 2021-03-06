# vcard-scm: vCard files from Scheme

This is currently just a vCard generator, not a parser.

* `vcard.scm`: the definition of the vCard file format.

* `vcard-easy.scm`: an abstraction that simplifies the data format of
  vcard a bit, and more importantly, tries to fill it into a vcard
  container in a way that makes it most likely for users of today's
  apps to see in the right way. It also tries to embed OpenPGP
  fingerprints in the best way possible.

  (Suggestions for improvements welcome, this depends on your
  feedback, as I don't have access to every app out there!)

* `qrencode.scm`: generate QR codes from strings by using the qrencode
  command line tool.

* `examples.scm`: how to make use of the above.


## Installation of dependencies on Debian

    $ sudo apt-get install git gambc build-essential qrencode imagemagick

imagemagick is only used for `showQR`, also you can omit qrencode if
you don't want to generate QR codes.

## Installation of dependencies  on other systems

Debian derivates should work as above, otherwise you'll have to find
corresponding packages
([Gambit-C](https://en.wikipedia.org/wiki/Gambit_(scheme_implementation))
Scheme system, C compiler,
[qrencode](http://fukuchi.org/works/qrencode/)). As said above,
qrencode and imagemagick are optional.

NOTE that some of the libraries from chj-schemelib might only work on
Linux (you'll get compilation or test failures in the next section if
that's the case). Contact [me](http://christianjaeger.ch/contact.html)
if this happens.


## Installation of vcard-scm

    $ git clone https://github.com/pflanze/vcard-scm
    $ cd vcard-scm
    $ git submodule init; git submodule update
    $ ./run

This will (when run for the first time) compile all of the used
libraries in lib/, and hence take a few minutes. Ignore the
warnings. Once the "`>` " prompt appears, enter the following to run
the test suite:

    (run-tests)

It should say "0 failures" at the end. To quit the Scheme repl:

    ,q


## Usage directly

To look at the vcard generated by the examples, enter the Scheme repl
again:

    $ ./run
    > (display (example2 #f)) ;; false for private, 
    > (display (example2 #t)) ;; true for public
    > (showQR (example2 #f)) ;; show QR code on screen
    > (showQR (example2 #f) "-s" "3") ;; man qrencode: 3 pixels per dot

To write it out as .vcd and QR code files in the current working
directory:

    > (printcard "example2" (example2 #f))

(or use the more lowlevel procedures from qrencode.scm like: )

    > (print-QR-eps-file "examp.eps" (example2 #f))


## Usage from own repo

If you want to keep track of your vcard definition(s) in a separate
Git repository, do the following:

    $ cd ..
    $ git init myvcard
    $ cd myvcard
    $ ln -s ../vcard-scm/.gambcini ../vcard-scm/run .
    $ mkdir .gambc
    $ cat > .gambc/load.scm # then paste the following:

    (parameterize ((current-directory "../vcard-scm"))
		  (load ".gambc/load.scm"))

    (load "me.scm")

hit ctl-d, 

    $ touch me.scm
    $ git add . ; git commit -m "start"
    $ ./run

open me.scm, add your definition, then

    > (lo) ;; reload modified files
    > (printcard "me" (me)) ;; pass whatever arguments `me` was defined for

## Enjoy..

..and send me email to the address in the card show by

    > (display (cjcard))
    > (showQR (cjcard))

and tell me whether you like it!

