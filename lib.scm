
(def (string-chars-match . preds)
     (lambda (v)
       (stream-every identity
		     (stream-map (lambda (pred c)
				   (pred c))
				 preds
				 (string->list v)))))


(def (maybe-string-append . maybe-string-s)
     (and (every identity maybe-string-s)
	  (strings-append maybe-string-s)))

(def (maybe-strings-join maybe-str-s str)
     (let ((strs (filter identity maybe-str-s)))
       (if* strs (strings-join strs str)
	    #f)))

;; ^ XX names should be changed to reflect whether upon encountering
;; #f they exit with #f or ignore the value.

