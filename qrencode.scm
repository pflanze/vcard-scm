
;; using the qrencode command line tool (from the package of the same
;; name in Debian)

(require cj-env)

;; move to another lib?
(def (print-file filename str)
     (with-output-to-file filename (C display str)))


(def (print-QR/ . opts)
     (lambda (filepath str)
       ;; echo str | qrencode .. -o filepath
       (let ((p (open-output-process (list path: "qrencode"
					   arguments: `(,@opts "-o" ,filepath)))))
	 (parameterize ((current-output-port p))
		       (display str))
	 (close-port p)
	 (assert (zero? (process-status p))))))

(def print-QR-eps-file (print-QR/ "-t" "EPS"))

(def print-QR-png-file (print-QR/ "-t" "PNG" "-s" "6"))

