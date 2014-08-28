;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require vcard
	 lib/unixtime
	 test)


;; How to generate URL from fingerprint.
(defenum openpgp-source
  ;; https query to MIT keyserver (default):
  keyserver
  ;; Append fingerprint to (or url-personal url-work) [check output
  ;; for details] (the reason to include the fingerprint in the URL is
  ;; so that the getter has the fingerprint; perhaps some can use this
  ;; (perhaps by human inspection) for verification):
  url
  ;; What would make most sense but does not conform to RFC and was
  ;; not seen anywhere by author, probably won't work:
  bare-fingerprint)

(defenum tel-preferred
  home
  mobile
  work)

(defenum address-preferred
  home
  work)

;; vcard-easy-struct captures the vcard-easy-string arguments
(defstruct vcard-easy-struct
  #!key
  #((maybe unixtime?) unixtime)
  ;; turn off non-essential output that can't otherwise be omitted
  ;; by user:
  small? 
  public?
  #(string? family-name)
  #(string? given-name)
  #(improper-list-of-maybe-string? additional-names)
  #(improper-list-of-maybe-string? honorific)
  #(improper-list-of-maybe-string? prefixes)
  #(improper-list-of-maybe-string? honorific-suffixes)
  #(improper-list-of-maybe-string? nickname)
  ;; A version 3 vcard requires FN, generate it by default. This
  ;; can experimentally be turned off. Note that one of the "QR
  ;; Droid" apps exits with an error if this is off! And it
  ;; doesn't actually help any of the apps feed first and last
  ;; names correctly to the contact app anyway (perhaps really an
  ;; Android problem?).
  (#(boolean? generate-FN?) #t)
  tel-home
  tel-mobile
  tel-work
  #((maybe tel-preferred?) tel-preferred)
  #((maybe vcard-address?) address-home)
  #((maybe vcard-address?) address-work)
  #((maybe address-preferred?) address-preferred)
  ;; whether to add a copy of the addresses in the NOTE field as
  ;; plain text, for the (many? most?) clients that don't handle
  ;; addresses nicely:
  (#(boolean? note-address-home?) #t)
  (#(boolean? note-address-work?) #t)
  email
  tz
  title
  role
  #((maybe LOGO?) logo) ;; look for LOGO in vcard.scm and examples.scm
  #((maybe PHOTO?) photo) ;; look for PHOTO in vcard.scm and examples.scm
  org
  ;; A note that will be prepended (with a newline) to some other
  ;; auto-generated parts:
  note
  uid
  url-personal
  url-work
  ;; OpenPGP fingerprint, preferably with spaces:
  openpgp-fingerprint
  (#((either string? openpgp-source?) openpgp-source) 'keyserver))


(def. (vcard-easy-struct.vcard-name v)
  (vcard-name family-name: (.family-name v)
	      given-name: (.given-name v)
	      additional-names: (.additional-names v)
	      honorific: (.honorific v)
	      prefixes: (.prefixes v)
	      honorific-suffixes: (.honorific-suffixes v)))

(def. (vcard-easy-struct.url v)
  (or (.url-personal v) (.url-work v)))

(def. (vcard-easy-struct.openpgp-fingerprint-nospaces v)
  (let. ((openpgp-fingerprint) v)
	(and openpgp-fingerprint
	     (string.replace-substrings openpgp-fingerprint " " ""))))

(def. (vcard-easy-struct.maybe-KEY v)
  (let. ((small? openpgp-source openpgp-fingerprint url) v)
	;; I haven't seen KEY used by the apps I tested with, so only
	;; generate when big is fine.
	(and
	 (not small?)
	 (if (string? openpgp-source)
	     (KEY (vcard-uri openpgp-source) TYPE: 'PGP)
	     (and openpgp-fingerprint
		  (xcase
		   openpgp-source
		   ((keyserver)
		    (KEY (vcard-uri
			  (string-append
			   "https://pgp.mit.edu/pks/lookup?op=get&search=0x"
			   openpgp-fingerprint-nospaces))
			 TYPE: 'PGP))
		   ((url)
		    (if url
			(KEY (vcard-uri (string-append
					 url
					 "pgpkey-"
					 (string.replace-substrings
					  openpgp-fingerprint
					  " " "-")
					 ".asc"))
			     TYPE: 'PGP)
			(error (string-append
				"asking openpgp-source 'url, but neither"
				" url-personal nor url-work given"))))
		   ((bare-fingerprint)
		    (warn* (string-append
			    "embedding fingerprint directly: it is unknown"
			    " to the author of this library whether this"
			    " will work anywhere. Alternatives recommended."))
		    (KEY openpgp-fingerprint-nospaces
			 ;; give that same type?
			 TYPE: 'PGP))))))))



;; convert arguments to vcard string
(def (vcard-easy-string . arguments)
     (let ((v (apply vcard-easy-struct arguments)))
       (let.
	;; bind some fields to variables:
	((address-preferred
	  openpgp-fingerprint tel-home tel-mobile tel-work
	  email tz title role logo photo org note url-personal url-work
	  note-address-home? note-address-work? address-home address-work
	  small? public? unixtime uid openpgp-source
	  ;; and the result of some methods, too:
	  vcard-name url)
	 v)

	(let ((perhaps
	       (lambda (constr . vals)
		 (and (every identity vals)
		      (apply constr vals))))

	      (order-preferred-address-home+work
	       (lambda (h w)
		 (if address-preferred
		     (xcase address-preferred
			    ((home) (list h w))
			    ((work) (list w h)))
		     (list h w))))

	      (address-prefer (prefer/ address-preferred? address-preferred))

	      (tel-prefer (prefer/ tel-preferred? (.tel-preferred v))))
     
	  (string-append
	   (.vcard-string
	    (VCARD

	     (and (.generate-FN? v)
		  (FN (.FN-string vcard-name)))

	     (N vcard-name)

	     (perhaps NICKNAME (.nickname v))

	     ;; Somehow both "QR Droid" and "Scan" will squash all of the
	     ;; address into the "Street" field of the contact app; not my
	     ;; fault I guess. Also, both apps will only use the first
	     ;; address and ignore the second, thus look at
	     ;; address-preferred to output the preferred one first. (Also,
	     ;; add the addresses to the NOTE field, too, by default.)
	     (order-preferred-address-home+work
	      (perhaps ADR address-home TYPE: (address-prefer 'home '(home <pref>)))
	      (perhaps ADR address-work TYPE: (address-prefer 'work '(work <pref>))))
	     ;; dom vs. intl, postal.. ? Well this is called 'easy', aka simple.

	     ;; The "QR Droid" app on Android completely ignores the TYPE,
	     ;; and strictly uses the encountered TEL entries as Home,
	     ;; Mobile, Work in order. Hence, do not use |perhaps| here,
	     ;; instead output "-" entries (the empty string wouldn't do
	     ;; either).  (The "Scan" Android app does not have this
	     ;; problem and actually respects the TYPE values. But note
	     ;; that this app will add the number as "Work" if both 'home
	     ;; and 'work are specified; thus keep them purely separate
	     ;; here.)
	     (and (or tel-home tel-mobile tel-work)
		  (TEL (or tel-home "-")
		       TYPE: (tel-prefer 'home '(home <pref> msg voice))))
	     (and (or tel-mobile tel-work)
		  (TEL (or tel-mobile "-")
		       TYPE: (tel-prefer 'mobile '(cell <pref> pcs voice))))
	     (and (or tel-work)
		  (TEL tel-work
		       TYPE: (tel-prefer 'work '(work <pref> msg voice))))
	     ;; ^ omit the msg pcs voice stuff in small mode?
      
	     (perhaps EMAIL email)
	     ;; can't say work etc. here; TYPE is just for internet vs. others

	     (perhaps TZ tz) ;; hmm what about summer time??

	     (perhaps TITLE title) ;; used by both "Scan" and "QR Droid"
	     (perhaps ROLE role)   ;; not used by either app

	     logo
	     photo

	     (perhaps ORG org)

	     (perhaps NOTE (maybe-strings-join
			    (flatten*
			     (list
			      note
			      (maybe-string-append "OpenPGP ID/fingerprint: "
						   openpgp-fingerprint)
			      (maybe-string-append "Personal: " url-personal)
			      (maybe-string-append "Work: " url-work)

			      (order-preferred-address-home+work
			       (and note-address-home? address-home
				    (string-append "Home address:\n"
						   (.pretty-string address-home)))
			       (and note-address-work? address-work
				    (string-append "Work address:\n"
						   (.pretty-string address-work))))))
			    "\n"))

	     (and (not small?)
		  (PRODID "https://github.com/pflanze/vcard-scm"))

	     (REV ((if (and public? (not small?))
		       .vcard-date-time
		       .vcard-date)
		   (if unixtime
		       (unixtime.gmtime unixtime)
		       (current-gmtime))))

	     ;;(SOUND )

	     (perhaps UID uid)
	     ;; uid is perhaps used by some apps for updating previously
	     ;; stored contacts (avoiding duplicates)?

	     (and url
		  (URL (vcard-uri url)))

	     ;; Do apps actually respect this setting? OK to omit it in
	     ;; |small?| case?
	     (and (not small?)
		  (CLASS (If public? "PUBLIC" "PRIVATE")))

	     (.maybe-KEY v)))

	   ;; *after* the VCARD, add field for Monkeysign (untested!)
	   ;; (omit in small mode?)
	   (if openpgp-fingerprint
	       (string-append
		vcard-eol
		"OPENPGP4FPR:" (.openpgp-fingerprint-nospaces v) vcard-eol)
	       ""))))))



;; definitions used in the above:

(def. (vcard-name.FN-string v)
  (strings-join
   (flatten*
    (map (lambda (f) (f v))
	 (list
	  ;; .prefixes ;; can be multiple
	  .honorific ;; e.g. Dr.
	  .given-name
	  ;; .additional-names
	  .family-name
	  .honorific-suffixes ;; ?
	  )))
   " "))

(def. (vcard-address.pretty-string v)
  (strings-join
   (map (C string-append " " _)
	(flatten*
	 (map (lambda (f)
		(maybe-strings-join
		 (flatten* (f v))
		 ;;XX ok?:
		 ", "))
	      (list .post-office-box
		    .extended-address
		    .street-address
		    .locality
		    .region
		    .postal-code
		    .country-name))))
   "\n"))

(def (prefer/ type? a)
     (assert ((maybe type?) a))
     (typed-lambda (#((maybe type?) b) vals)
	      (if (eq? a b)
		  (map (lambda (v) (if (eq? v '<pref>) 'pref v)) vals)
		  (delete '<pref> vals))))


(TEST
 > (%try-error (vcard-easy-string given-name: "C" ))
 #(error "family-name does not match string?:" #f)
 > (.given-name (vcard-easy-struct family-name: "foo" given-name: "bar"))
 "bar"
 > (.note-address-home?
    (vcard-easy-struct family-name: "foo" given-name: "bar"))
 #t
 > (.note-address-home?
    (vcard-easy-struct family-name: "foo" given-name: "bar"
		       note-address-home?: #f))
 #f)

