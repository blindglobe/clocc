;;; File: <geo.lisp - 1998-03-10 Tue 13:26:49 EST sds@mute.eaglets.com>
;;;
;;; Geo.lisp - geographical data processing

(eval-when (load compile eval) (sds-require 'util) (sds-require 'url))

;;;
;;; {{{ Georgaphic Coordinates
;;;

(defun parse-geo-coord (st)
  "Return the number parsed from latitude or longitude (dd:mm:ss[nsew])
read from the stream."
  (let* ((sig 1) (cc (+ (read st) (/ (read st) 60.0d0)))
	 (lt (read st)) se nn)
    (if (numberp lt) (setq se lt nn 0 lt (string (read st)))
	(setf (values se nn)
	      (parse-integer (setq lt (string lt)) :junk-allowed t)))
    (unless se (setq se 0)) ;; (error "Cannot parse the seconds from `~a'" lt))
    (setq sig (cond ((or (char-equal (elt lt nn) #\n)
			 (char-equal (elt lt nn) #\e)) 1)
		    ((or (char-equal (elt lt nn) #\s)
			 (char-equal (elt lt nn) #\w)) -1)
		    (t (error "Wrong sign designation: `~a'. ~
Must be one of [N]orth, [S]outh, [E]ast or [W]est." (elt lt nn)))))
    (* sig (+ cc (/ se 3600d0)))))

(defun geo-location (str &optional (num 2) &key (start 0))
  "Return the latitude and longitude as two numbers from a string of
type \"48:51:00N 2:20:00E\". The optional second argument says how many
coordinates to read (default 2). If it is 1, return just one number, if
more than 1, return the list."
  (declare (simple-string str) (fixnum num start))
  (setq str (nsubstitute #\Space #\: (string str))
	str (nsubstitute #\Space #\, (string str)))
  (with-input-from-string (st str :start start)
    (if (= num 1) (parse-geo-coord st)
	(do ((ii 0 (1+ ii)) res) ((= ii num) (nreverse res))
	  (push (parse-geo-coord st) res)))))

(defun print-geo-coords (obj &optional (ar1 t) (ar2 t))
  "Print the geographic coordinates to the stream.
If the first two arguments are numbers, print them to the third argument.
Otherwise, the print the slots LAT and LON of the first argument to the
second argument."
  (let (lat lon str)
    (if (and (realp obj) (realp ar1)) (setq lat obj lon ar1 str ar2)
	(setq lat (slot-value obj 'lat) lon (slot-value obj 'lon) str ar1))
    (format str "~9,5f ~a  ~9,5f ~a" lat (if (minusp lat) #\S #\N) lon
	    (if (minusp lon) #\W #\E))))

;;;
;;; }}}{{{ Geo-Data
;;;

(defstruct (geo-data (:print-function print-geod) (:conc-name geod-))
  (name "??" :type string)	; the name of the place
  (pop 0 :type (real 0 *))	; population
  (lat 0.0 :type double-float)	; latitude
  (lon 0.0 :type double-float)	; longitude
  (zip nil :type list))		; list of zip codes.

(defun print-geod (gd &optional stream depth)
  "Print the geo-data."
  (declare (type geo-data depth))
  (if *print-readably* (funcall (print-readably geo-data) gd stream depth)
      (let ((st (or stream (make-string-output-stream))))
	(format st "Place: ~a~%Population: ~12:d;~30tLocation: "
		(geod-name gd) (geod-pop gd))
	(print-geo-coords gd st)
	(format st "~%Zip Code~p:~{ ~d~}~%" (length (geod-zip gd))
		(geod-zip gd))
	(unless stream (get-output-stream-string st)))))

(defvar *census-gazetteer-url* (make-url :prot "http" :host "www.census.gov"
					 :path "/cgi-bin/gazetteer?")
  "*The URL to use to get the cite information.")

(defun cite-info (&key (url *census-gazetteer-url*) city state zip (out t))
  "Get the cite info from the U.S. Gazetteer.
Print the results to the stream OUT (defaults to T, discard if NIL)
and return a list of geo-data."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (unless (or city state zip)
    (error "You must specify at least one of :city, :state or :zip~%"))
  (flet ((prep (str) (if str (substitute #\+ #\Space (string str)) "")))
    (setf (url-path url)
	  (format nil "~acity=~a&state=~a&zip=~a" (url-path url) (prep city)
		  (prep state) (or zip ""))))
  (with-open-url (sock url *readtable* t)
    (do () ((search "<ul>" (read-line sock))))
    (do ((str "") res gd (ii 1 (1+ ii)))
	((or (search "</ul>" str)
	     (search "</ul>" (setq str (read-line sock))))
	 (nreverse res))
      ;; name
      (setq gd (make-geo-data :name (strip-html-markup str))
	    str (read-line sock))
      ;; population
      (setf (geod-pop gd) (parse-integer str :junk-allowed t :start
					 (1+ (position #\: str))))
      ;; location
      (setq str (nsubstitute #\Space #\: (read-line sock))
	    str (nsubstitute #\Space #\, str)
	    str (nsubstitute #\Space #\< str))
      (with-input-from-string (st str)
	(read st)
	(setf (geod-lat gd) (* (read st) (if (eq 'n (read st)) 1 -1))
	      (geod-lon gd) (* (read st) (if (eq 'w (read st)) 1 -1))))
      ;; zip
      (setq str (read-line sock))
      (setf (geod-zip gd)
	    (if (search "Zip Code" str)
		(with-input-from-string (st str :start (1+ (position #\: str)))
		  (do (rr re) ((null (setq rr (read st nil nil)))
			       (nreverse re))
		    (when (numberp rr) (push rr re))))
		(list zip)))
      (read-line sock) (setq str (read-line sock)) (push gd res)
      (when out (format out "~%~:d. " ii) (print-geod gd out)))))

(defvar *weather-url* (make-url :prot "telnet" :port 3000
				:host "mammatus.sprl.umich.edu")
  "*The url for the weather information.")

(defun weather-report (code &key (out t) (url *weather-url*))
  "Print the weather report for CODE to OUT."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (setf (url-path url) (format nil "/~a//x" code))
  (with-open-url (sock url nil)
    (do (rr)
	((eq *eof* (setq rr (read-line sock nil *eof*))))
      (format out "~a~%" rr))))

;;;
;;; Countries
;;;

(defstruct (country (:print-function print-country))
  (name "" :type simple-string)	; name
  (fips nil :type symbol)	; FIPS PUB 10-4 code (US Dept of State)
  (iso2 nil :type symbol)	; ISO 3166: Codes for the Representation
  (iso3 nil :type symbol)	; of Names of Countries. 2- and 3- letters
  (isod 0 :type fixnum)		; ISO 3166: number
  (inet nil :type symbol)	; Internet Domain
  (incl nil :type (or null country)) ; Included in
  (note nil :type (or null simple-string)) ; ISO Note
  (area 0.0 :type double-float)	; Area in sq km
  (frnt 0.0 :type double-float)	; fontier length
  (lat 0.0 :type double-float)	; Latitude
  (lon 0.0 :type double-float)	; Longitude
  (pop 0 :type fixnum)		; population
  (gdp 0.0 :type double-float)	; GDP, in $$
  (cap nil :type (or null simple-string))) ; capital

(defun print-country (ntn &optional (str t) (depth 1))
  "Print the COUNTRY structure."
  (declare (type country ntn) (fixnum depth))
  (if *print-readably* (funcall (print-readably country) ntn str depth)
      (let ((st (or str (make-string-output-stream))))
	(format st "~a~@[ (~a)~] [~a ~a] [ISO: ~a ~a ~d]~@[ part of ~a~]
Location: "
		(country-name ntn) (country-cap ntn) (country-fips ntn)
		(country-inet ntn) (country-iso2 ntn) (country-iso3 ntn)
		(country-isod ntn) (and (country-incl ntn)
					(country-name (country-incl ntn))))
	(print-geo-coords ntn st)
	(format st "~%Population: ~:d~30tArea: ~f~50tGDP: $~f~%~@[[~a]~%~]"
		(country-pop ntn) (country-area ntn) (country-gdp ntn)
		(country-note ntn))
	(unless str (get-output-stream-string st)))))

(defvar *geo-code-url*
  (url "http://www.odci.gov/cia/publications/nsolo/wfb-appf.htm")
  "*The URL with the table of the codes.")
(defvar *geo-info-template*
  "http://www.odci.gov/cia/publications/nsolo/factbook/~a.htm"
  "*The string for generating the URL for getting information on a country.")
(defvar *country-list* nil "*The list of known countries.")
(defvar *country-file* (merge-pathnames "country.dat" *lisp-dir*)
  "The file with the country data.")
(declaim (simple-string *geo-info-template*) (list *country-list*)
	 (type url *geo-code-url*) (pathname *country-list*))

(defsubst find-country (slot value &key
			(test (cond ((member slot '(name cap note)) #'search)
				    ((member slot '(isod pop)) #'=)
				    (#'eq))))
  "Get the COUNTRY struct corresponding to the given SLOT VALUE.
Returns the list of all countries satisfying the condition."
  (declare (symbol slot))
  (mapcan (lambda (nn)
	    (when (funcall test value (slot-value nn slot)) (list nn)))
	  *country-list*))

(defun save-restore-country-list (&optional (save t))
  "Save or restore `*country-list*'."
  (if save (write-to-file *country-list* *country-file*)
      (setq *country-list* (read-from-file *country-file*))))

(defsubst str-core (rr)
  "Get the substring of the string from the first `>' to the last `<'."
  (declare (simple-string rr))
  (let ((cc (subseq rr (1+ (position #\> rr)) (position #\< rr :from-end t))))
    (cond ((string= "-" cc) "NIL") ((string= "<BR>" cc) nil) (cc))))

(defun fetch-country-list ()
  "Initialize `*country-list*' from `*geo-code-url*'."
  (format t "Reading `~a'" *geo-code-url*)
  (with-open-url (st *geo-code-url* *readtable* t)
    ;; (with-open-file (st "/home/sds/lisp/wfb-appf.htm" :direction :input)
    (do () ((search "Entity" (read-line st) :test #'char=)))
    (do () ((search "</TR>" (read-line st) :test #'char=)))
    (do (rr nn res)
	((search "</TABLE>" (setq rr (read-line st))
		 :test #'char=) (setq *country-list* (nreverse res)))
      (princ ".")
      (setq nn (make-country
		:name (str-core (read-line st))
		:fips (intern (str-core (read-line st)))
		:iso2 (intern (str-core (read-line st)))
		:iso3 (intern (str-core (read-line st)))
		:isod (or (read-from-string (str-core (read-line st))) 0)
		:inet (intern (str-core (read-line st)))
		:note (str-core (read-line st))))
      (read-line st) (push nn res)))
  (format t "~d countries.~%" (length *country-list*))
  (dolist (nn *country-list*)
    (when (country-note nn)
      (format t "~a: ~a~%" (country-name nn) (country-note nn))
      (do* ((iiw " includes with ")
	    (pos (search iiw (country-note nn))
		 (1+ (or (position #\Space new) (1- (length new)))))
	    (new (if pos (subseq (country-note nn) (+ (length iiw) pos)))
		 (subseq new pos))
	    (ll (find-country 'name new) (find-country 'name new)))
	   ((or (null new) (zerop (length new)) (= 1 (length ll)))
	    (if (= 1 (length ll))
		(format t "~5tIncdiding into --> ~a~%"
			(country-name (setf (country-incl nn) (car ll))))
		(if pos (format t "~10t *** Not found!~%"))))))))

(defun dump-country (ntn &rest opts)
  "Dump the URL for the country."
  (declare (type country ntn))
  (apply #'dump-url (url (format nil *geo-info-template* (country-fips ntn)))
	 opts))

(defmacro dump-find-country ((&rest find-args) (&rest dump-args &key (out t)
						&allow-other-keys))
  "Dump all the URLs for all the relevant countries."
  (remf dump-args :out)
  (dolist (cc (apply #'find-country find-args))
    (let ((st (if (or (and (symbolp out) (fboundp out)) (functionp out))
		  (funcall out cc) out)))
      (format st "~70~~%~a~70~~%" cc)
      (apply #'dump-country cc :out st dump-args))))

(defun update-country (cc)
  "Get the data from the WWW and update the structure."
  (declare (type country cc))
  (with-open-url (st (url (format nil *geo-info-template* (country-fips cc))))
    (do (ln pos (loc "<B>Location:</B>"))
	((setq pos (search loc (setq ln (read-line st))))
	 (setq loc (geo-location ln 2 :start pos))
	 (setf (country-lon cc) (car loc) (country-lat cc) (cadr loc))))
    ))

(provide "geo")
;;; geo.lisp ends here
