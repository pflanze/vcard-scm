
(require vcard-easy qrencode)

(def example1
     ;; this is using the minimal set of values possible
     (vcard-easy-string given-name: "Peter"
			family-name: "Pan"))


(def small? #f) ;; generate reduced output

(def (example2 public? #!optional unixtime)
     (def (private val)
	  (if public? #f val))

     (vcard-easy-string
      public?: public?
      small?: small? ;; to strip non-essential automatic stuff
		     ;; (including a reference to this project; it
		     ;; would be nice if instead you could make a
		     ;; reference in another place, thank you!)
      unixtime: unixtime
      ;; ^ unixtime is used for REV (revision); when false, the
      ;;   current time is taken instead
      given-name: "Peter"
      additional-names: '("Nathan" "Rupert")
      family-name: "Pan"
      prefixes: '("Jr." "M.D." "A.C.P.") ;; example from RFC2426
      honorific: "Dr." ;; or "Mr." etc.
      ;; honorific-suffixes:  ;; ?
      nickname: "ppan"
      ;;tel-home: (private "+41321234567")
      tel-mobile: (private "+447787654321")
      tel-work: "+442321876543"
      tel-preferred: 'work
      email: "pp@ppan.ch"
      tz: (and (not small?) "+01:00")
      ;; ^ Note: I don't know how summer time should be handled. Seems
      ;;   outdated.

      note: "I like swimming."
      
      ;; For the addresses, I'm requiring vcard-address objects; see
      ;; "subclass vcard-address" in vcard.scm for all the fields
      ;; these support.
      address-home: (and (not small?)
			 (vcard-address
			  street-address: '("Asbeshaus" "Foostrasse 77")
			  locality: "Chur"
			  postal-code: "1234"
			  country-name: "Switzerland"))
      address-work: (vcard-address
		     street-address: '("12 Foo House" "Bar street")
		     locality: "London"
		     postal-code: "SE01 345"
		     country-name: "United Kingdom")
      ;; if small, turn of copying of address to NOTE field:
      note-address-work?: (not small?)
      address-preferred: 'work

      title: (private "president")
      role: (and (not small?) (private "president, CTO"))
      org: (private "Pan AG")

      ;; uid is any kind of string that practically guarantees global
      ;; uniqueness, best use a random string (e.g. run "passwdgen 16"
      ;; using the script from my "chj-bin" repository on Github)
      uid: "w344dfv973c2ut5365znxu52jd"
      url-personal: "http://ppan.ch/"
      url-work: "http://foobar.com/"
      openpgp-fingerprint: "DEC1 DEAF FACE DDEC ADED  EEDA DDED CAFE FEDB EEF0"
      ;; generate source url from given url (url-personal in this
      ;; case); the default is to use the MIT keyserver:
      openpgp-source: 'url
      
      ;; photo: (PHOTO
      ;; 	      (vcard-binary-base64string
      ;; 	       "iVBORw0KGgoAAAANSUhEUgAAAGEAAABZAQMAAAAU4dggAAAABlBMVEUAAAD///+l2Z/dAAABBElE
      ;; QVQ4y83SMW7DMAwFUAYGoq26QXiFjhkC8Gg2kCGjj9CrOFPGXsGAhq4WsrCAoJ/BkiMZracGqCa9
      ;; QdQnQUJxOnqJON+10lhpqHSp1Ff6qMQ/ioDhcKRZgQT0lqUnG+0+y+MSZJT0zuGmmLJucB6a9Ym7
      ;; R8jq4V2pb4eY1dGu1tdv6gG3qfDMUv5eJ6tTT7BLR7TTE0ezz5onkQVqMRxmvX4L/khKAgCSZSo1
      ;; pXCt5CtpWXOlsNJoWY7oCE4R0BmW90XmzEJJGnliMVmhVRab5BWB06wVV0VcRI1HZJEk64Gn2lLA
      ;; fUN+U7GQrhSSnAKh1UKRp0JozqU6U2q0uea/2ZcHyjqZfoqEIQQAAAAASUVORK5CYII=")
      ;; 	      ENCODING: 'BASE64 ;; no change with 'b 
      ;; 	      TYPE: 'PNG)

      ;; ^ didn't show up on the ~4 Android apps I've tested with,
      ;;   neither LOGO nor PHOTO.

      ;; logo: (LOGO (vcard-uri "http://leafpair.com/logo-tiny.png")
      ;; 		  TYPE: 'PNG
      ;; 		  VALUE: 'uri)
      ;; photo: (PHOTO (vcard-uri "http://leafpair.com/logo-tiny.png")
      ;; 		    TYPE: 'PNG
      ;; 		    VALUE: 'uri)

      ;; ^ neither do these lead to any kind of reaction
      ))


(def (printcard-QR basepath str . opts)
     (let ((path+ (C string-append basepath _)))
       (apply print-QR-eps-file (path+ ".eps") str opts)
       (apply print-QR-png-file (path+ ".png") str opts)))

(def (printcard basepath str . opts)
     (print-file (string-append basepath ".vcd") str)
     (apply printcard-QR basepath str opts))


(TEST
 > (print-file "example2.vcd" (example2 #f 0))
 > (backtick "md5sum" "example2.vcd")
 "ef270fefecf3d7d41a251eaed7c823f8  example2.vcd")

