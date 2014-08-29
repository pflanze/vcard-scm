;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A representation of and formatters for vCard objects, according to
;; http://tools.ietf.org/html/rfc2426 and
;; http://tools.ietf.org/html/rfc2425

;; XXX TODO:
;;
;; - what is the model for data types (and hence escaping) really,
;;   e.g. is this correct? ROLE:president\, CTO
;; 
;; - handle more special chars other than \n and \t? how to encode
;;   unicode in the output?

(require lib lib/easy lib/test lib/unixtime lib/tree-util)


(defenum vcard-version v21 v3 v4)

;; macro so that it can (evilly and unhygienically) refer to a local
;; variable:
(defmacro (*vcard-version*)
  ;; only v3 is really defined yet, as aside of the cursory wikipedia
  ;; page I've studied RFC2426 only
  `vcard-v3)

;; turn (*vcard-version*) into a string
(def vcard-v21 "2.1")
(def vcard-v3 "3.0")
(def vcard-v4 "4.0")

(defmacro (*vcard-allow-without-FN*)
  ;; whether to allow vcards without an FN field even though the
  ;; version given by *vcard-version* officially makes it required.
  `#f)


;; So much guessing, really. RFC2426 is long and still unclear (ok
;; perhaps this is in RFC2425?)
(def vcard-word?
     ;; no requirement for upcase, although the examples for CLASS are such?
     (both nonempty-string?
	   (string-of char-alphanumeric?)))

(def improper-list-of-maybe-string?
     (improper-list-of (maybe string?)))

;; escaping using backslash
(def (vcard-escape-chars #(string? chars))
     (let ((escape? (char-one-of chars)))
       (typed-lambda (#(string? v))
		(list->string
		 (string-fold-right (lambda (c r)
				      (let ((r* (cons c r)))
					(if (escape? c)
					    (cons #\\ r*)
					    r*)))
				    '()
				    v)))))
(TEST
 > (map (vcard-escape-chars ";,") '("" "foo, \\bar{};baz\n"))
 ;; note that this is not the way escaping is done in the actual vcard
 ;; code
 ("" "foo\\, \\bar{}\\;baz\n"))


(def (maybe-string.vcard-string/escapes chars)
     (let ((escape (vcard-escape-chars chars)))
       (lambda (v)
	 (if v
	     (escape v)
	     ""))))

;; escaping levels

;; level 1 is used when joining multiple segments within a value
;; (text-list, see RFC2425)
(def maybe-string.vcard-string-1
     (maybe-string.vcard-string/escapes ",\\"))

;; level 2 is used when joining multiple values (input is really a
;; vcard-string-1)
(def maybe-string.vcard-string-2
     (maybe-string.vcard-string/escapes ";"))

;; both level 1+2 at the same time (input is really a vcard-string-1)
(def maybe-string.vcard-string-1+2
     (maybe-string.vcard-string/escapes ",\\;"))

;; level 3 would seem to make sense to be used when joining the value
;; part to the field name and parameters (maybe type wouldn't make
;; sense here). But using it on the value part would actually be
;; wrong. (Input is really a vcard-string-2.)
(def string.vcard-string-3
     (maybe-string.vcard-string/escapes ":"))


(def. (improper-list-of-maybe-string.comma-separated-vcard-string v)
  (strings-join (improper*-map maybe-string.vcard-string-1+2 v)
		","))

(TEST
 > (.comma-separated-vcard-string '())
 ""
 > (.comma-separated-vcard-string #f)
 ""
 > (.comma-separated-vcard-string '("" #f))
 ","
 > (.comma-separated-vcard-string '(#f "Foo,bar"))
 ",Foo\\,bar"
 > (.comma-separated-vcard-string '(#f "Foo;bar:1\\2"))
 ",Foo\\;bar:1\\\\2")


(class rfc2425-text-list
       ;; RFC2426: The text components are separated by the SEMI-COLON
       ;; character (ASCII decimal 59). Where it makes semantic sense,
       ;; individual text components can include multiple text values
       ;; (e.g., a "street" component with multiple lines) separated
       ;; by the COMMA character (ASCII decimal 44).

       ;; Also see RFC2425, from which the name text-list has been
       ;; taken.

       (method (vcard-string v)
	       (strings-join
		(map improper-list-of-maybe-string.comma-separated-vcard-string
		     ;; HACK. add proper base struct method?
		     (cdr (vector->list v)))
		";"))

       (subclass vcard-name
		 (struct
		  #!key
		  #(improper-list-of-maybe-string? family-name)
		  #(improper-list-of-maybe-string? given-name)
		  #(improper-list-of-maybe-string? additional-names)
		  #(improper-list-of-maybe-string? honorific)
		  #(improper-list-of-maybe-string? prefixes)
		  #(improper-list-of-maybe-string? honorific-suffixes)))

       (subclass vcard-address
		 (struct
		  #!key
		  #(improper-list-of-maybe-string? post-office-box)
		  #(improper-list-of-maybe-string? extended-address)
		  #(improper-list-of-maybe-string? street-address)
		  #(improper-list-of-maybe-string? locality)
		  ;; ^ (e.g., city)
		  #(improper-list-of-maybe-string? region)
		  ;; ^ (e.g., state or province)
		  #(improper-list-of-maybe-string? postal-code)
		  #(improper-list-of-maybe-string? country-name))))

(TEST
 > (.vcard-string (vcard-address
		   extended-address: "Foo house"
		   street-address: '("14 Baz street" "Left, then right corner")
		   locality: "Lundin"
		   region: "Ungland"
		   postal-code: "3SC 5CX"
		   country-name: "Ukay"))
 ";Foo house;14 Baz street,Left\\, then right corner;Lundin;Ungland;3SC 5CX;Ukay")



(defenum vcard-address-type
  dom	 ;; a domestic delivery address
  intl	 ;; an international delivery address
  postal ;; a postal delivery address
  parcel ;; a parcel delivery address
  home	 ;; a delivery address for a residence
  work	 ;; delivery address for a place of work
  pref	 ;; to indicate the preferred delivery address when more than
  ;; one address is specified.

  ;; (BTW These type parameter values can be specified as a parameter
  ;;  list (i.e., "TYPE=dom;TYPE=postal") )
  )

(defenum vcard-phonenumber-type
  ;; 'where':
  home	;; associated with a residence
  work	;; associated with a place of work
  pref	;; preferred-use telephone number
  ;; 'what':
  msg	;; the telephone number has voice messaging support
  voice ;; voice telephone number (default for TEL)
  fax	;; facsimile telephone number
  cell	;; cellular telephone number
  video ;; video conferencing telephone number
  pager ;; paging device telephone number
  bbs	;; bulletin board system telephone number
  modem ;; MODEM connected telephone number
  car	;; car-phone telephone (name conflict with |car| procedure is
  ;; resolved in favor of the existing procedure)
  isdn ;; ISDN service telephone number
  pcs  ;; personal communication services telephone number (what
  ;; does that mean?
  ;; https://en.wikipedia.org/wiki/Personal_Communications_Service)
  )

(defenum vcard-email-type
  internet ;; default
  x400
  pref
  ;;XX hmm sigh:
  ;; Another IANA registered address type can also be
  ;; specified. A non-standard value
  ;; can also be specified.
  )

(def vcard-phonenumber-string? string?) ;; ok?

(def (bare-email-address-string? v)
     (and (string? v)
	  ;; XX not 100% correct, esp. since non-internet adresses are
	  ;; supported..
	  (string-contains? v "@")
	  (not (string-contains? v " "))))

(def (vcard-utc-offset-string? v)
     (and (string? v)
	  ;; regexes would be good here..
	  (= (string-length v) 6)
	  ((string-chars-match (char-one-of "+-")
			       char-digit?
			       char-digit?
			       (char=?/ #\:)
			       char-digit?
			       char-digit?) v)))

(TEST
 > (map vcard-utc-offset-string?
	'(x "" "+03:03" "+03:03 " "+03:034" "+0303" "03:03"))
 (#f #f #t #f #f #f #f))

(def vcard-geo-degrees?
     (all-of number?
	     real?
	     inexact?
	     (C <= -180 _ 180)))

(TEST
 > (every (applying (lambda (v r)
		      (equal? (vcard-geo-degrees? v) r)))
	  '((0 #f)
	    (-170. #t)
	    (-180. #t)
	    (-180.1 #f)
	    (180.1 #f)
	    (180. #t)))
 #t)


(class vcard-binary
       (subclass vcard-binary-u8vector
		 (struct #(u8vector? value))
		 (method (vcard-string v)
			 (u8vector.base64-string (.value v))))
       (subclass vcard-binary-base64string
		 (struct #(string? value))
		 (method (vcard-string v)
			 ;; according to
			 ;; http://stackoverflow.com/questions/19167455/cant-add-a-base64-encoded-image-to-vcard
			 ;; base64 string might have to be in one
			 ;; line?
			 (chain  (.value v)
				 (.replace-substrings "\n" "")
				 (.replace-substrings " " "")))))

(class vcard-uri
       (struct #(string? value))
       (method vcard-string (compose maybe-string.vcard-string-1+2
				     vcard-uri.value)))

(defenum vcard-encoding
  b ;; from RFC2426
  BASE64 ;; Sigh, this is what people are using? (XX Is this in RFC2425?)
  )

(defenum vcard-image-format
  ;; "The TYPE parameter values MUST be one of the IANA registered
  ;; image formats or a non-standard image format." hm?
  JPEG
  PNG ;; XX ok?
  ;; XX ... ?
  )

(defenum vcard-VALUE
  ;; totally undocumented in RFC 2426 except shown in example of
  ;; "3.5.3 LOGO Type Definition" and "3.5.4 AGENT Type Definition"?
  ;; (XX Is this in RFC2425?)
  uri)

(defenum vcard-pgp-type
  ;; as seen in https://en.wikipedia.org/wiki/Vcard
  ;; (XX Is this in RFC2425? Or is nobody following the RFCs really?)
  PGP)

(def integer-year?
     (all-of integer?
	     exact?
	     (C <= 1900 _ 2100)))
(def integer-month?
     (all-of integer?
	     exact?
	     (C <= 1 _ 12)))
(def integer-day?
     (all-of integer?
	     exact?
	     (C <= 1 _ 31)))
(def integer-hour?
     (all-of integer?
	     exact?
	     (C <= 0 _ 23)))
(def integer-minute?
     (all-of integer?
	     exact?
	     (C <= 0 _ 59)))
(def integer-second?
     (all-of integer?
	     exact?
	     ;; allow for leap seconds. correct?
	     (C <= 0 _ 60)))

(class vcard-time
       (struct #(integer-hour? hour)
	       #(integer-minute? minute)
	       #(integer-second? second))
       
       (method (vcard-string v)
	       (let* ((vals (cdr (vector->list v))))
		 (strings-join
		  (map (C number->padded-string 2 _) vals)
		  ":"))))

(class vcard-date-perhaps-time
       (subclass vcard-date
		 (struct #(integer-year? year)
			 #(integer-month? month)
			 #(integer-day? day))
		        
		 (method (vcard-string v)
			 (let* ((vals (cdr (vector->list v))))
			   (strings-join
			    (map number->padded-string
				 '(4 2 2)
				 vals)
			    "-"))))
       (subclass vcard-date-time
		 (struct constructor-name: _vcard-date-time
			 #(vcard-date? date)
			 #(vcard-time? time))
		 ;; zone? Nah, assume GMT

		 (def (vcard-date-time y m d H M S)
		      (_vcard-date-time (vcard-date y m d)
					(vcard-time H M S)))
		 (method (vcard-string v)
			 (string-append (.vcard-string (.date v))
					"T"
					(.vcard-string (.time v))
					"Z"))))

(TEST
 > (.vcard-string (vcard-date 1994 3 4))
 "1994-03-04"
 > (.vcard-string (vcard-date-time 1994 3 4 12 0 30))
 "1994-03-04T12:00:30Z")

(def. (gmtime.vcard-date v)
  (vcard-date (.year v)
	      (.month v)
	      (.mday v)))

(def. (gmtime.vcard-date-time v)
  (vcard-date-time (.year v)
		   (.month v)
		   (.mday v)
		   (.hour v)
		   (.min v)
		   (.sec v)))


;; ------------------------------------------------------
;; stringification of more types (basic Scheme types)

;; NOTE: these are all for the first level, Scheme types to the first
;; level of vcard strings. XX Hence change these names to
;; .vcard-string-1 (changing all of their call sites)?

(def. string.vcard-string maybe-string.vcard-string-1)
;; ^ XX or .vcard-string-1+2 ?
(def. number.vcard-string number->string)
;; ^ always ok? (Never using "," or ";" or "\\")
(def. (pair.vcard-string v)
  (let-pair ((a v*) v)
	    (assert (not (pair? a))) ;; pair.vcard-string are for the
				     ;; lowest level only, no nesting
				     ;; allowed there
	    (let ((astr (.vcard-string a)))
	      (if (null? v*)
		  astr
		  ;; XX O(n^2); and could just do (strings-join
		  ;; (improper-map* .vcard-string v) ","), man!
		  (string-append astr "," (.vcard-string v*))))))
(def. (null.vcard-string v)
  ;;(XX correct re joining and all ? sgh; just give #f instead and
  ;;check for that in pair.vcard-string instead for null?)-- why was I
  ;;worried really?
  "")

(def. symbol.vcard-string
  (compose maybe-string.vcard-string-1 symbol.string))
;; XX or .vcard-string-1+2 ?

(def. (false.vcard-string v)
  ;;(XX ok? re joinoing --what was my worry exactly?)
  "")

(def (.maybe-vcard-string v)
     (if v
	 (.vcard-string v)
	 #f))

(TEST
 > (.vcard-string "b,2;3\\,4")
 "b\\,2;3\\\\\\,4"
 > (.vcard-string #f)
 ""
 > (.maybe-vcard-string #f)
 #f
 > (.maybe-vcard-string '|f,g|)
 "f\\,g")


(def (vcard-parameters->string names maybe-str-s)
     ;; maybe-str-s is a list of vcard-strings
     (strings-join
      (filter identity
	      (map (lambda (name maybe-str)
		     (and maybe-str
			  (string-append
			   (symbol->string name) "="
			   (maybe-string.vcard-string-2 maybe-str))))
		   names maybe-str-s))
      ;;XX correct?:
      ";"))

(TEST
 > (vcard-parameters->string '(A B) '("a" "b"))
 "A=a;B=b"
 > (vcard-parameters->string '(A B) '("a" ""))
 "A=a;B="
 > (vcard-parameters->string '(A B) '("a" #f))
 "A=a"
 ;; > (vcard-parameters->string '(A B) '("a" ("b1" "b,2"
 ;; "b;3\\,3.2"))) that's not valid; conversion to vcard-string-1
 ;; needs to be done by caller; like:
 > (vcard-parameters->string
    '(A B)
    (map .vcard-string '("a" ("b1" "b,2" "b;3\\,3.2"))))
 "A=a;B=b1,b\\,2,b\\;3\\\\\\,3.2"
 > (vcard-parameters->string '(A B)
			     (map .vcard-string '("a" ("b1" #f))))
 "A=a;B=b1,")


;; There's no vcard-values->string, see values-vcard-string (and
;; typed+format.ref-code/class) methods instead.

;; ------------------------------------------------------
;; Field definitions

(compile-time

 ;; representation for a syntax that holds both type predicate and a
 ;; formatter (pretty-printer to string)

 (class typed+format
	(struct #((maybe location?) maybe-location)
		maybe-predicate
		#((perhaps-source-of symbol?) variable)
		maybe-formatter)

	(method (perhaps-typed v)
		(let-typed+format ((maybe-loc maybe-pred var maybe-format) v)
				  (possibly-sourcify
				   (if maybe-pred
				       (vector maybe-pred var)
				       var)
				   maybe-loc)))

	(def (perhaps-typed.var->ref-for classname varname)
	     (lambda (parameter)
	       `(,(source.symbol-append
		   classname
		   "."
		   (perhaps-typed.var parameter))
		 ,varname)))

	(def (typed+format.ref-code/class class V)
	     (lambda (v)
	       `(,(or (.maybe-formatter v)
		      `.maybe-vcard-string)
		 (,(source.symbol-append
		    class
		    "."
		    (.variable v))
		  ,V)))))
 
 (def (typed+format:parse v)
      (let ((v* (source-code v))
	    (t+f (C typed+format (maybe-source-location v)
		    _ _ _)))
	(if (vector? v*)
	    (let ((len (vector.length v*)))
	      (case len
		((2) (t+f (vector.ref v* 0)
			  (vector.ref v* 1)
			  #f))
		((3) (t+f (vector.ref v* 0)
			  (vector.ref v* 1)
			  (vector.ref v* 2)))
		(else
		 (source-error v "expecting either symbol or vector   "))))
	    (t+f #f v #f)))))


;; using CRLF seems essential, at least "QR Droid" won't handle
;; continuation lines properly with just LF; hence it's also not
;; possible to copy-paste output at least when feeding to 'qrencode'
;; from the Debian qrencode package, instead have to write to a file.
(def vcard-eol "\r\n")

(insert-result-of

 (defenum vcard-featured r y n)
 ;; r = required, y = possible, n = not available

 ;; predicates for individual members (XX add to defenum ?)
 (def n? (lambda (x) (eq? x 'n)))
 (def r? (lambda (x) (eq? x 'r)))
 
 (let* ((required-classes '())
	(classdefs
	 (map

	  (comp
	   (applying
	    (typed-lambda
	     (#((source-of symbol?) name)
	       #((source-of vcard-featured?) vcard-v21)
	       #((source-of vcard-featured?) vcard-v3)
	       #((source-of vcard-featured?) vcard-v4)
	       #((source*-of (improper-list-of string?)) desc)
	       #((source-of (improper-list-of vector?)) parameterS) ;; code
	       #((source-of (improper-list-of vector?)) valueS) ;; code
	       )
	     (with-source*
	      (name)

	      (if (and (r? (source-code (*vcard-version*)))
		       (not (and (*vcard-allow-without-FN*)
				 (eq? name* 'FN))))
		  (push! required-classes name))
	    
	      (if (n? (*vcard-version*))
		  `(begin)
		  (with-gensyms
		   (V)
		   (let ((values-tf
			  (source.improper*-map typed+format:parse valueS))
			 (parameters-tf
			  (source.improper*-map typed+format:parse parameterS)))
		     (quasiquote-source
		      (subclass ,name
				(struct
				 ,@(map .perhaps-typed values-tf)
				 ;; DSSSL keywords can only come at the
				 ;; end (otherwise would have to hack
				 ;; something with #!rest)
				 ,@(if* parameterS
					(quasiquote-source-list
					 #!key
					 ,@(map .perhaps-typed parameters-tf))
					'()))

				(method
				 (parameters-vcard-string ,V)
				 ,(if* parameterS
				       (quasiquote-source
					(vcard-parameters->string
					 ',(map .variable parameters-tf)
					 (list
					  ,@(map (typed+format.ref-code/class
						  name V)
						 parameters-tf))))
				       (quasiquote-source
					"")))
				(method
				 (values-vcard-string ,V)
				 ,(let ((es
					 (map (typed+format.ref-code/class
					       name V)
					      values-tf)))
				    (if ((length-is 1) es)
					(car es)
					(quasiquote-source
					 (strings-join
					  (list ,@es)
					  ;; XX always correct?  Makes
					  ;; sense, comma for within
					  ;; fields
					  ";")))))
			       
				;; (Could hack instead by taking the
				;; name of the struct)
				(method (name v)
					',name)
				(method (name-string v)
					,(symbol->string name*))))))))))
	   source-code)

	  (quote-source-list
	   ;; Name
	   ;; 2.1
	   ;; 3.0
	   ;; 4.0
	   ;; Description
	   ;; ParameterS with predicates
	   ;; PredicateS

	   ;; 3.1 Identification Types
	   (FN
	    y r r
	    "The formatted name string associated with the vCard object."
	    ()
	    #(string? value))

	   (N
	    r r y
	    ("A structured representation of the name of the person, "
	     "place or thing associated with the vCard object.")
	    ()
	    #(vcard-name? value))

	   (NICKNAME
	    n y y
	    ("One or more descriptive/familiar names for the object "
	     "represented by this vCard.")
	    ()
	    #(improper-list-of-maybe-string?
	      values
	      improper-list-of-maybe-string.comma-separated-vcard-string))

	   (ADR
	    y y y
	    ("A structured representation of the physical delivery "
	     "address for the vCard object.")
	    ;; Type encoding: 8bit
	    (#((maybe (improper-list-of vcard-address-type?)) TYPE))
	    ;; (XX check that only sensible combinations are used? Or
	    ;; is actually anything ok?)  "The default is
	    ;; "TYPE=intl,postal,parcel,work"."
	    #(vcard-address? value))

	   (LABEL
	    y y n
	    ("Represents the actual text that should be put on the "
	     "mailing label when delivering a physical package to "
	     "the person/object associated with the vCard (related to "
	     "the ADR property).")
	    (#((maybe (improper-list-of vcard-address-type?)) TYPE))
	    ;; ^ same as ADR, correct?
	    #(string? value))

	   ;; 3.3 Telecommunications Addressing Types
	   (TEL
	    y y y
	    ("The canonical number string for a telephone number for "
	     "telephony communication with the vCard object.")
	    (#((maybe (improper-list-of vcard-phonenumber-type?)) TYPE))
	    #(vcard-phonenumber-string? value))

	   (EMAIL
	    y y y
	    ("The address for electronic mail communication with the "
	     "vCard object.")
	    (#((maybe (improper-list-of vcard-email-type?)) TYPE))
	    #(bare-email-address-string? value))

	   (MAILER
	    y y n
	    "Type of email program used."
	    ()
	    #(string? value))
	
	   ;; 3.4 Geographical Types
	   (TZ
	    y y y
	    "The time zone of the vCard object."
	    ()
	    #(vcard-utc-offset-string? value)
	    ;; XX sigh the spec (rfc2426) is pathetic: "Type value:
	    ;; The default is a single utc-offset value. It can also
	    ;; be reset to a single text value." but then the text
	    ;; example comes with 3 fields. -> Just don't support that
	    ;; altogether.
	    )

	   (GEO
	    y y y
	    "Specifies a latitude and longitude."
	    ()
	    (
	     #(vcard-geo-degrees? lat)
	     #(vcard-geo-degrees? lon)))

	   ;; 3.5 Organizational Types
	   (TITLE
	    y y y
	    ("Specifies the job title, functional position or function "
	     "of the individual associated with the vCard object within "
	     "an organization.")
	    ()
	    #(string? value)
	    ;; XX "Type special notes: This type is based on the
	    ;; X.520 Title attribute."
	    )
	    
	   (ROLE
	    y y y
	    ("The role, occupation, or business category of the vCard "
	     "object within an organization.")
	    ()
	    #(string? value)
	    ;; XX "This type is based on the X.520 Business Category
	    ;; explanatory attribute."
	    )

	   (LOGO
	    y y y
	    ("An image or graphic of the logo of the organization that "
	     "is associated with the individual to which the vCard "
	     "belongs. It may point to an external URL or may be "
	     "embedded in the vCard as a Base64 encoded block of text.")
	    (
	     #((maybe vcard-image-format?) TYPE)
	     #((maybe vcard-encoding?) ENCODING)
	     #((maybe vcard-VALUE?) VALUE)) ;; Sigh, only shown in
	    ;; example, not in
	    ;; documentation.
	    #((either vcard-uri? vcard-binary?) value))

	   (AGENT
	    y y n
	    ("Information about another person who will act on behalf "
	     "of the vCard object. Typically this would be an area "
	     "administrator, assistant, or secretary for the individual. "
	     "Can be either a URL or an embedded vCard.")
	    (#((maybe vcard-VALUE?) VALUE))
	    ;; XX and check that VALUE is uri if value is a vcard-uri
	    #((either vcard-uri? vcard?) value))

	   (ORG
	    y y y
	    ("The name and optionally the unit(s) of the organization "
	     "associated with the vCard object. This property is based "
	     "on the X.520 Organization Name attribute and the X.520 "
	     "Organization Unit attribute.")
	    ()
	    #((improper-list-of string?) value))

	   ;; 3.6 Explanatory Types
	   (CATEGORIES
	    y y y
	    ("A list of \"tags\" that can be used to describe the "
	     "object represented by this vCard.")
	    ()
	    ;; hmm using the same 'tuple' again like in
	    ;; NICKNAME, "should this get a meta name?"
	    ;; Disadvantage of predicates vs. types.
	    #(improper-list-of-maybe-string?
	      values
	      improper-list-of-maybe-string.comma-separated-vcard-string))

	   (NOTE
	    y y y
	    ("Specifies supplemental information or a comment that is "
	     "associated with the vCard.")
	    ()
	    #(string? value))

	   (PRODID
	    n y y
	    "The identifier for the product that created the vCard object."
	    ()
	    #(string? value))

	   (REV
	    y y y
	    "A timestamp for the last time the vCard was updated."
	    ()
	    #(vcard-date-perhaps-time? value))

	   (SORT-STRING
	    y y n
	    ("Defines a string that should be used when an application "
	     "sorts this vCard in some way.")
	    ()
	    #(string? value))

	   (SOUND
	    y y y
	    ("By default, if this property is not grouped with other "
	     "properties it specifies the pronunciation of the FN "
	     "property of the vCard object. It may point to an "
	     "external URL or may be embedded in the vCard as a Base64 "
	     "encoded block of text.")
	    #((maybe vcard-encoding?) ENCODING)
	    #((either vcard-uri? vcard-binary?) value))

	   (UID
	    y y y
	    ("Specifies a value that represents a persistent, "
	     "globally unique identifier associated with the object.")
	    ()
	    #(string? value))

	   (URL
	    y y y
	    ("A URL pointing to a website that represents the person "
	     "in some way.")
	    ()
	    #(vcard-uri? value))

	   ;;(VERSION r r r "The version of the vCard
	   ;; specification. In versions 3.0 and 4.0, this must come
	   ;; right after the BEGIN property.")  implicit. XX: allow
	   ;; it to be given to actually choose the version,
	   ;; influencing the type?

	   ;; 3.7 Security Types
	   (CLASS
	    n y n
	    "Describes the sensitivity of the information in the vCard."
	    ()
	    ;; ex. "CLASS:PUBLIC CLASS:PRIVATE CLASS:CONFIDENTIAL"
	    #(vcard-word? value))
	    
	   (KEY
	    y y y
	    ("The public encryption key associated with the vCard "
	     "object. It may point to an external URL, may be plain "
	     "text, or may be embedded in the vCard as a Base64 "
	     "encoded block of text.")
	    (#((maybe vcard-pgp-type?) TYPE) ;; not in v21, and
	      ;; not mentioned in
	      ;; rfc2426, see
	      ;; below.
	      #((maybe vcard-encoding?) ENCODING))
	    ;; either binary or text, but in both cases meant to
	    ;; be key data, *not* a uri! Sigh! (rfc2426) *BUT*
	    ;; wikipedia claims that all versions of vcard could
	    ;; carry a url here, duh?, well, perhaps iff TYPE=PGP
	    ;; is given, which is v3/v4 specific.
	    #((either vcard-binary?
		      string?
		      vcard-uri?)
	      value)
	    ;; XX again, check for consistency in constructor or
	    ;; so, TYPE=PGP requiring vcard-uri? maybe? Not clear
	    ;; at all though.
	    )

	   ;; 3.8 Extended Types

	   ;; The types defined by this document can be extended with
	   ;; private types using the non-standard, private values
	   ;; mechanism defined in [RFC 2045]. Non-standard, private
	   ;; types with a name starting with "X-" may be defined
	   ;; bilaterally between two cooperating agents without
	   ;; outside registration or standardization.

	   ;;From wikipedia, other versions than rfc2426:

	   ;; 	(ANNIVERSARY n n y "Defines the person's anniversary.")
	   ;; 	(BDAY y y y "Date of birth of the individual associated
	   ;;    with the vCard.")
	   ;; 	;; (BEGIN r r r) ;; ?;; (END)
	   ;; 	(CALADRURI n n y "A URL to use for sending a scheduling
	   ;;    request to the person's calendar.")
	   ;; 	(CALURI n n y "A URL to the person's calendar.")
	   ;; 	(CLIENTPIDMAP n n y "Used for synchronizing different 
	   ;;    revisions of the same vCard.")
	   ;; 	(FBURL n n y "Defines a URL that shows when the person 
	   ;;    is \"free\" or \"busy\" on their calendar.")
	   ;; 	(GENDER n n y "Defines the person's gender.")
	   ;; 	(IMPP n y y "Defines an instant messenger handle.")
	   ;; 	     (#(maybe-vcard-encoding? ENCODING))
	   ;; 	     )
	   ;; 	(KIND n n y "Defines the type of entity that this vCard 
	   ;;    represents, such as an individual or organization.")
	   ;; 	(LANG n n y "Defines a language that the person speaks.")
	   ;; 	(MEMBER n n y "Defines a member that is part of the group 
	   ;;    that this vCard represents. Acceptable values include:
	   ;; a \"mailto:\" URL containing an email address
	   ;; a UID which references the member's own vCard
	   ;; The KIND property must be set to \"group\" in order to use 
	   ;;    this property.")
	   ;; 	(NAME n y n "Provides a textual representation of the 
	   ;;    SOURCE property.")

	   (PHOTO
	    y y y
	    ("An image or photograph of the individual associated with "
	     "the vCard. It may point to an external URL or may be "
	     "embedded in the vCard as a Base64 encoded block of text.")
	    (
	     #((maybe vcard-image-format?) TYPE)
	     #((maybe vcard-encoding?) ENCODING)
	     #((maybe vcard-VALUE?) VALUE) ;; ?
	     )
	    ;; Note that
	    ;; http://stackoverflow.com/questions/19167455/cant-add-a-base64-encoded-image-to-vcard
	    ;; says that iOS does not support uris, just
	    ;; embedded
	    #((either vcard-uri? vcard-binary?) value))
	   ;; ^ heh vs LOGO

	   ;; 	(PROFILE y y n "States that the vCard is a vCard.")
	   ;; 	;; ^ ehr
	   ;; 	(RELATED n n y "Another entity that the person is related 
	   ;;    to. Acceptable values include:
	   ;; a \"mailto:\" URL containing an email address
	   ;; a UID which references the person's own vCard")
	   ;; 	(SOURCE y y y "A URL that can be used to get the latest 
	   ;;    version of this vCard.") ;; h
	   ;; 	;; ^^    RFC2426: "If the SOURCE type is present, then its 
	   ;;  ;;       value provides information how to find the source 
	   ;;  ;;       for the vCard."
	   ;; 	;;  source != latest version ????
	   ;; 	;; goldhamster everything
	   ;; 	(XML n n y "Any XML data that is attached to the vCard. 
	   ;;    This is used if the vCard was encoded in XML (xCard 
	   ;;    standard) and the XML document contained elements which 
	   ;;    are not part of the xCard standard.")

	   ;; "A handful of separate specifications define additional 
	   ;;    vCard properties."
	   ;; well, ignoring those

	    
	   ;; "vCard extensions"
	   ;; todo?

	   ;; Invention by the Monkeysign authors and Christian Jaeger:
	   (X-OPENPGPFPR
	    y y y
	    ("An OpenPGP key fingerprint, preferably without spaces. "
	     "VERSION specifies the OpenPGP major version the key was "
	     "generated for; it is optional but might help against "
	     "downgrading attacks when it turns out that keys for older "
	     "versions can be generated with identical fingerprints.")
	    #((maybe natural?) V)
	    #(string? value))))))

   (no-pp-through-source ;; remove the no- to see the generated code
    (quasiquote-source
     (begin
       (class
	vcard-field

	(method (vcard-string v)
		(string-append
		 (string.vcard-string-3
		  ;; is escaping ":" the usual way with \\ correct?
		  ;; Some form of escaping seems to be unavoidable
		  ;; here, except if types preclude any parameter
		  ;; value to carry a colon; but then this won't hurt
		  ;; either.
		  (string-append
		   (.name-string v)
		   (let ((str (.parameters-vcard-string v)))
		     (if (string-empty? str)
			 str
			 (string-append ";" str)))))
		 ":"
		 ;; oddly, do *not* escape ":" here!
		 (.values-vcard-string v)))

	(method (vcard-formatted-string v)
		;; additionally does line [wrapping and] newline
		;; [etc.] escaping
		(string-append
		 (chain (.vcard-string v)
			(.replace-substrings
			 "\n"
			 (string-append "\\n" vcard-eol " "))
			(.replace-substrings "\t" "\\t"))
		 vcard-eol))
   
	,@classdefs)
    
       (def valid-list-of-vcard-field?
	    ;; XX HACK *again*, if I want to *tell* what's actually
	    ;; wrong, need something better/new. *Same* as parsing,
	    ;; really. Thus using exceptions instead of false.
	    (both (list-of vcard-field?)
		  (lambda (vs)
		    (let ((vnames (map .name vs)))
		      (for-each (lambda (classname)
				  (or (any (C eq? _ classname)
					   vnames)
				      ;; [XX and again, how to tell
				      ;; the context.]
				      (error "missing an instance of:"
					     classname)))
				',required-classes)
		      #t)))))))))


(class vcard
       (struct #(valid-list-of-vcard-field? fields))
       ;; toplevel stringify:
       (method (vcard-string v)
	       (strings-append `("BEGIN:VCARD" ,vcard-eol
				 ,(string-append
				   "VERSION:" (*vcard-version*) vcard-eol)
				 ,@(map .vcard-formatted-string (.fields v))
				 "END:VCARD" ,vcard-eol))))

(def (VCARD . fields)
     ;; flatten nested lists and filter away #f values
     (vcard (flatten* fields)))

(TEST
 > (.vcard-string (TEL "+1-213-555-1234" TYPE: '(work voice pref msg)))
 "TEL;TYPE=work,voice,pref,msg:+1-213-555-1234"
 > (.vcard-string (TEL "+1-213-555-1234"))
 "TEL:+1-213-555-1234"
 > (.vcard-string (EMAIL "jqpublic@xyz.dom1.com"))
 "EMAIL:jqpublic@xyz.dom1.com"
 > (.vcard-string (EMAIL "jqpublic@xyz.dom1.com" TYPE: '(internet)))
 "EMAIL;TYPE=internet:jqpublic@xyz.dom1.com"
 > (.vcard-string (GEO -2.3 4.4445))
 "GEO:-2.3;4.4445"
 > (.vcard-string (NICKNAME '("foo" "bar")))
 "NICKNAME:foo,bar"
 > (.vcard-string (NICKNAME '("foo" #f "bar")))
 "NICKNAME:foo,,bar" ;; well?
 > (.vcard-string (CATEGORIES '("TRAVEL AGENT" "Foo,bar;baz:1\\2")))
 "CATEGORIES:TRAVEL AGENT,Foo\\,bar\\;baz:1\\\\2"
 > (.vcard-string (REV (vcard-date 2014 12 10)))
 "REV:2014-12-10"
 > (.vcard-string (KEY (vcard-uri "http://foo.com/bar.pgp?a=1,b=2;c=3")
		       TYPE: 'PGP))
 "KEY;TYPE=PGP:http://foo.com/bar.pgp?a=1\\,b=2\\;c=3"
 
 > (def adr1 (ADR (vcard-address street-address: "123 Main Street"
				 locality: "Any Town"
				 region: "CA"
				 postal-code: "91921-1234")
		  TYPE: '(dom home postal parcel)))
 > (%try-error (VCARD adr1))
 #(error "missing an instance of:" N)
 > (equal?
    (%try-error
     (VCARD (N (vcard-name family-name: "Cutter" given-name: "John"))))
    (if (*vcard-allow-without-FN*)
	'#(vcard (#(N #(vcard-name "Cutter" "John" #f #f #f #f))))
	'#(error "missing an instance of:" FN)))
 #t
 > (.vcard-string
    (VCARD adr1
	   (FN "John Cutter")
	   (N (vcard-name family-name: "Cutter" given-name: "John"))))
 "BEGIN:VCARD\r\nVERSION:3.0\r\nADR;TYPE=dom,home,postal,parcel:;;123 Main Street;Any Town;CA;91921-1234;\r\nFN:John Cutter\r\nN:Cutter;John;;;;\r\nEND:VCARD\r\n" 
 ;; BEGIN:VCARD
 ;; VERSION:3.0
 ;; ADR;TYPE=dom,home,postal,parcel:;;123 Main Street;Any Town;CA;91921-1234;
 ;; FN:John Cutter
 ;; N:Cutter;John;;;;
 ;; END:VCARD
 )

