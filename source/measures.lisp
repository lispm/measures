;;;-*- Package: MEASURES; Syntax: Common-Lisp -*-

;;;; ************************** MEASURES *******************************
;;;
;;;   Module         :  MEASURES
;;;   Abstract       :  Funktionen zum Rechnen mit dimensionierten Zahlen

;;;; ************************* Identification ***************************
;;;
;;;   Author  :  Roman Cunis		of   :  Univ. Hamburg


;;;; Directives

(pushnew :measures *features*)

;;;; **************** Interface-Commands *************************

(in-package :measures)

(provide 'measures)

(defvar *measures-version* "2.3, 12-APR-2017")

(defparameter *ms-package* (find-package "MEASURES"))

(export '(*measures-version*
	  measuring-p measure dim-number dim-measure measure-named
	  measure-p dim-number-p make-dim-number
	  read-dim-number parse-dim-number
	  define-unit-format with-unit-format print-converted
	  define-unit delete-unit defmeasure delete-measure
          declare-unit-char install-dim-number-reader
	  dim+ dim- dim* dim/ dim< dim<= dim= dim/= dim>= dim>
	  dim-value dim-max dim-min dim-sqrt dim-expt dim-zerop
	  dim-eql dim-equal dim-equalp dim-same-units-p
	  *step-skip-leading-zeros* *step-skip-trailing-zeros*
	  *ratios-only* pprint-measure get-derived-measures))

(export '(ms-base-unit ms-scale ms-prim-id ms-output-format ms-name))

;;;; *TMP-UNIT-OUTPUT-FORMAT*
;;;; Fuer jede Groesse kann ein spezielles Ausgabe-Format definiert werden.
;;;; Dies bedeutet, dass im Moment der Ausgabe die Einheit einer
;;;; dimensionierten Zahl entsprechend gewaehlt wird:
;;;;   :BASE -- Basis-Einheit
;;;;   :UNIT <Einheit> -- die angegebene Einheit
;;;;   :CURRENT -- keine Umwandlung
;;;;   :BEST-FIT -- diejenige Einheit, bei der am wenigsten Stellen vor dem
;;;;   Komma bzw. Nullen nach dem Komma benoetigt werden.
;;;;   :STEP <Einheit-1> ... <Einheit-n> -- Stufenweise unter Verwendung der
;;;;   Einheiten-i, so dass fuer die ersten n-1 Einheiten nur ganzzahlige
;;;;   Werte ausgegeben werden.
;;;; Die moeglichen Ausgabe-Formate sind in *VALID-FORMAT-KEYS* aufgelistet.
;;;; *TMP-UNIT-OUTPUT-FORMAT* ordnet jeder Groesse (Namenssymbol) ein Format in
;;;; einer Assoc-Liste zu, ein T-Eintrag wird als Default verwendet, sonst ist
;;;; :CURRENT Default (z.B. ((LENGTH :BASE) (T :BEST-FIT)) ).

(defvar *tmp-unit-output-format*)


(defclass measure ()
  ((base-unit     :type symbol :accessor ms-base-unit :initarg :base-unit)
   (scale         :initform nil :type list :accessor ms-scale :initarg :scale)
   (prim-id       :type rational :accessor ms-prim-id :initarg :prim-id)
   (output-format :accessor ms-output-format :initarg :output-format)
   (regular       :accessor ms-regular :initarg :regular)
   (name          :accessor ms-name :initarg :name)))

(defun measure-p (ms) (typep ms 'measure))

(defun measure-name (ms) (ms-name ms))

(defun measure-named (mn &key (error-p t))
  (cond ((find mn (list-all-measures) :key #'ms-name))
	(error-p (error "Not a measure: ~A." mn))
	(t nil)))

(defmethod print-object ((ms measure) stream)
  (format stream "#<Measure ~A (~A)>" (ms-name ms) (ms-base-unit ms)))

(defun pprint-measure (ms0 &optional (stream *standard-output*))
  (let ((ms (cond ((measure-p ms0) ms0)
		  ((typep ms0 '(or symbol string))
		   (measure-named ms0))
		  (t (error "Not a measure: ~S." ms0))))) 
    (format stream
	    "~&~
	    {{<Measure ~A>~%~
	    ~5TBase-Unit: ~S~%~
	    ~5TScale: ~S~%~
	    ~5TPrim-Id: ~S~%~
	    ~5TOutput-Format: ~@[~S~] }}~%"
	    (ms-name ms) (ms-base-unit ms)
	    (ms-scale ms) (ms-prim-id ms) (ms-output-format ms))
    nil))

(defstruct (dim-number (:conc-name %dim-)
                       (:print-function print-dim-number)
                       (:constructor %make-dim-number))
  value prim-id)

(defstruct (xdim-number (:include dim-number)
			(:conc-name %dim-)
			(:print-function print-dim-number)
			(:constructor %make-xdim-number))
  pref-unit)


(defvar *prim-numbers* 
    '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(defvar *metric-prefixes* '((#\m 1/1000) (#\c 1/100) (#\d 1/10) (#\k 1000)))

(defvar *scientific-prefixes*
        '((#\p 1/1000000000000) (#\n 1/1000000000) (#\u 1/1000000)
	  (#\m 1/1000) (#\k 1000) (#\M 1000000) (#\G 1000000000)))

(defvar *read-measures* ':dispatched)
;; alternativ: :on-demand  :dispatched  :integrated

(defvar *print-formatted-dim* t)

;;; Feinsteuerung der Ausgabe im :STEP-Format (s. STEPWISE-DIM-LIST)

(defvar *step-skip-leading-zeros* t)
(defvar *step-skip-trailing-zeros* t)


;;;; Wenn *RATIOS-ONLY* T ist, werden ALLE Dim-Zahlen als Ratios (absolute
;;;  Genauigkeit, kleinerer Bereich) dargestellt.
;;;  Moegliche Werte: T -- Dim-Numbers, :ALL -- Alle Zahlen werden Ratios,
;;;                   NIL -- Ratios nur bei "/"-Notation

;;;-exported-
(defvar *ratios-only* nil
  "All Dim-Numbers (when T) and Numbers (when :ALL) will be read as Ratios.")

;;;; MEASURE-STATE
;;;; Global-State-Frame zur Buendelung aller fuer den Benutzer wichtigen
;;;; globalen Variablen.


(defmacro measure-of (unit) `(get ,unit 'measure))

(defun base-unit-p (unit) (eq (ms-base-unit (measure-of unit)) unit))

(defun unit-p (unit)
  (and (etypecase unit
	  (symbol (measure-of unit))
	  (string (and (setq unit (find-symbol unit *ms-package*))
		       (measure-of unit))))
       t))
	
(defun primitive-p (ms)
  (and (member (ms-prim-id ms) *prim-numbers*) t))

(defun dim-value0 (n)
  (etypecase n
    (number n)
    (dim-number (%dim-value n))))

(defun dim-prim-id (n)
  (etypecase n
    (number 1)
    (dim-number (%dim-prim-id n))))

(defun dim-measure (n)
  (prim-id-to-measure (dim-prim-id n)))

(defun make-dim-number (&key (value 1) (unit nil u-p) (pid nil p-p))
  (cond ((eq u-p p-p)
         (error "Exactly one of :unit or :pid must be specified."))
        (p-p (%make-dim-number :value (rationalize value) :prim-id pid))
	(u-p (unit-to-dim-number unit :value value))))

(defun unit-to-dim-number (unit &key (value 1) (error-p t))
  (when (and (or (stringp unit) (symbolp unit))  ;; Handle Scalar!
	     (zerop (length (string unit))))
    (return-from unit-to-dim-number value))
  (unless (and (or (stringp unit) (symbolp unit))
	       (unit-char-p (char (string unit) 0) t))
    (error "Not a proper unit: ~A." unit))
  (let ((us (string unit))
	(slash-found 1))
    (when (stringp unit) (setq unit (intern unit *ms-package*)))     ; tar
    (labels
       ((unit-to-dn-1 (from)
	  (when (member (char us from) '(#\. #\/) :test #'char=)
	    (cond ((>= (1+ from) (length us))
		   (error "Invalid unit syntax: ~A." us))
		  ((char= (char us from) #\.)
		   (incf from))
		  ((eql slash-found -1)
		   (error "Invalid unit syntax: ~A." us))
		  (t (setq slash-found -1) (incf from))))
	  (let ((end-of-unit
		 (or (position-if-not #'constituent-unit-char-p us :start from)   ; tar
		     (length us))))
	    (if (zerop end-of-unit)
		(error "Invalid unit syntax: ~A." us)
		(let* ((us1 (subseq us from end-of-unit))
		       (unit1 (find-symbol us1
					; (symbol-package unit) ; <-- rm
					   *ms-package*  ; <-- tar
					   ))
		       (measure1 (measure-of unit1)))
		  (if measure1
		      (multiple-value-bind (exp end-of-exp)
			   (parse-integer us :start end-of-unit
					  :junk-allowed t)
			(let ((real-exp (* slash-found (or exp 1)))
			      (expv (ms-prim-id measure1))
			      (scale (cdr (assoc unit1 (ms-scale measure1)))))
			  (cons (%make-dim-number
				 :value (expt scale real-exp)
				 :prim-id (expt expv real-exp))
				(if (< end-of-exp (length us))
				    (unit-to-dn-1 end-of-exp)
				    nil))))
		      (if error-p
			  (error "Undefined unit ~A in unit specification: ~A."
				 us1 us)
			  (return-from unit-to-dim-number nil))))))))
      (let* ((unit0 (find-symbol us *ms-package*)) ; tar
	     (measure0 (measure-of unit0)))
	(if measure0
	    (%make-xdim-number
	     :value (rationalize
		     (* value (cdr (assoc unit0 (ms-scale measure0)))))
	     :prim-id (ms-prim-id measure0)
	     :pref-unit unit0)
	    (apply #'dim* value (unit-to-dn-1 0)))))))

(defun dim-number (value unit)
  (unit-to-dim-number unit :value value :error-p t))


(defun associate-unit-with-measure (unit measure scale)
  (setf (ms-scale measure)
	(merge 'list (ms-scale measure) (list (cons unit scale))
	       #'> :key #'cdr))
  (setf (measure-of unit) measure))

(defun make-measure (mname &key base-unit prim-id format regular)
  (let ((ms (make-instance 'measure
			   :name mname
			   :base-unit base-unit
			   :prim-id prim-id
			   :output-format format
			   :regular regular)))
    (associate-unit-with-measure base-unit ms 1)
    (set-prim-id-to-measure prim-id ms)
    ms))

(defparameter *measures-table* (make-hash-table :size 20 :test #'eql))

(defun set-prim-id-to-measure (pid ms)
  (if ms
      (setf (gethash pid *measures-table*) ms)
    (remhash pid *measures-table*)))
  
(defun list-all-measures ()
  (let ((mlist nil))
    (maphash #'(lambda (pid ms)
                 (declare (ignore pid))
                 (push ms mlist))
             *measures-table*)
    mlist))

(defun prim-id-to-measure (pid)
  (gethash pid *measures-table*))


(defun dim-number-equal (dn1 dn2)
  (and (= (%dim-value dn1) (%dim-value dn2))
       (eql (%dim-prim-id dn1) (%dim-prim-id dn2))))

(defun next-prim-number (last-pn)
  (flet ((prim-number-p (n)
	   (notany #'(lambda (div) (zerop (rem n div)))
		   (cdr *prim-numbers*))))
    (do ((pn (if (= last-pn 2) 3 (+ 2 last-pn))
	     (+ 2 pn)))
      ((prim-number-p pn) pn))))

(defun prim-factors (pid)
  (flet ((factors (pid)
	   (if (= pid 1) nil
	       (let ((pn *prim-numbers*)
		     (factors nil)
		     (cnt 0))
		 (loop
		   (multiple-value-bind (quot rem) (floor pid (car pn))
		     (cond ((plusp rem)
			    (if (zerop cnt) (pop pn)
				(push (cons (pop pn) cnt) factors))
			    (setq cnt 0))
			   ((= quot 1)
			    (return (cons (cons (car pn) (1+ cnt)) factors)))
			   (t (incf cnt) (setq pid quot)))))))))
    (values (factors (numerator pid))
	    (factors (denominator pid)))))

		       
(defparameter *unused-prim-numbers* *prim-numbers*)
  
(defun next-prim-id ()
  (prog1 (pop *unused-prim-numbers*)
    (if (null (cdr *unused-prim-numbers*))
        (nconc *unused-prim-numbers*
               (list (next-prim-number
                      (car *unused-prim-numbers*)))))))

(defun reset-prime-numbers ()
  (setq *unused-prim-numbers* *prim-numbers*))

(defun prim-id-to-unit (pid &optional (type 'string) prim)
  (labels
     ((pf-to-u (pf &aux m)
        (if (setq m (prim-id-to-measure (car pf)))
	    (cons (ms-base-unit m) (cdr pf))
	    (error "Invalid unit pid: ~A." pid)))
      (pid-to-u (pid &aux m)
	(cond ((and (not prim)
		    (setq m (prim-id-to-measure pid)))
	       (values (list (cons (ms-base-unit m) 1)) nil))
	      ((and (setq m (prim-id-to-measure (/ pid)))
		    (or (integerp pid) (not (ms-regular m))))
	       (values nil (list (cons (ms-base-unit m) 1))))
	      (t (multiple-value-bind (nom-pn denom-pn)
				      (prim-factors pid)
		   (values (sort (mapcar #'pf-to-u nom-pn)
				 #'> :key #'cdr)
			   (sort (mapcar #'pf-to-u denom-pn)
				 #'> :key #'cdr))))))
      (mlist-to-string1 (mlist)
	(apply #'concatenate 'string
	       (mapcon #'(lambda (u-rest &aux (u (car u-rest)))
			   (list
			    (string (car u))
			    (cond ((/= (cdr u) 1)
				   (princ-to-string (cdr u)))
				  ((and (cdr u-rest)
					(plusp (cdadr u-rest))) ".")
				  (t ""))))
		       mlist)))
      (mlist-to-string (numlist denomlist)
	(if denomlist
	    (concatenate 'string
			 (mlist-to-string1 numlist)
			 "/" (mlist-to-string1 denomlist))
	    (mlist-to-string1 numlist))))
    (multiple-value-bind (numlist denomlist)
			 (pid-to-u pid)
      (ecase type
	(string (mlist-to-string numlist denomlist))
	(symbol (intern (mlist-to-string numlist denomlist)
			*ms-package*))))))

(defun dim-value (dn &optional unit)
" DIM-VALUE dim-number [unit]

  Returns the value of dim-number as with the unit given.
  Unit defaults to the base unit of the dim-number's measure.
"
  (cond (unit
	 (check-type dn dim-number)
	 (check-type unit (or string symbol))
	 (let ((unit-dn (unit-to-dim-number unit)))
	   (if (eql (%dim-prim-id dn) (%dim-prim-id unit-dn))
	       (/ (%dim-value dn) (%dim-value unit-dn))
	       (error "Incompatible units: ~A, ~A."
		      (prim-id-to-unit (dim-prim-id dn)) unit))))
	(t (dim-value0 dn))))


(defun define-base-unit-fcn (unit measure-name &optional primitives)
  (if (stringp unit) (setq unit (intern unit *ms-package*)))
  (if (measure-of unit) (delete-measure (measure-of unit)))
  (let ((unit-dn (unit-to-dim-number unit :error-p nil))
	(prim-dn (if primitives
		     (apply #'dim*
			    (mapcar #'unit-to-dim-number primitives)))))
    (let ((pid (cond ((or (and unit-dn (/= (dim-value0 unit-dn) 1))
			 (and prim-dn (/= (dim-value0 prim-dn) 1)))
		     (error "Derived base units must be defined by base units."))
		    ((and (not unit-dn) (null primitives))
		     (next-prim-id))
		    ((and unit-dn (null primitives))
		     (dim-prim-id unit-dn))
		    ((or (not unit-dn)
			 (dim-number-equal unit-dn prim-dn))
		     (dim-prim-id prim-dn))
		    (t (error "Unit specification ~A is incompatible with: ~{~A~^ ~}."
			      unit primitives)))))
      (values unit (make-measure measure-name
				 :base-unit unit
				 :prim-id pid
				 :regular (and unit-dn t))))))

(defun define-unit-fcn (unit &rest unit-list)
  (if (stringp unit) (setq unit (intern unit *ms-package*)))
  (if (measure-of unit) (delete-unit unit))
  (let ((unit-dn (unit-to-dim-number unit :error-p nil))
	(prim-dn (if unit-list
		     (apply #'dim*
			    (mapcar #'(lambda (p)
					(etypecase p
					  (number p)
					  ((or symbol string)
					   (unit-to-dim-number p))))
				    unit-list)))))
    (let ((dn (cond ((and (not unit-dn) (null unit-list))
		     (error "No definition given for: ~A." unit))
		    ((and unit-dn (null unit-list))
		     unit-dn)
		    ((or (not unit-dn)
			 (dim-number-equal unit-dn prim-dn))
		     prim-dn)
		    (t (error "Unit specification ~A is incompatible with: ~{~A~^ ~}."
			      unit unit-list)))))
      (values unit
	      (associate-unit-with-measure
	       unit
	       (dim-measure dn)
	       (dim-value0 dn))))))

;;;; ------------------------- DEFINE-METRIC-UNITS --------------------------
#|
Name 		:  define-metric-units

Abstract	:  Definiert metrische Standard-Einheiten zu einer
		   Basis-Einheit

Arguments	:  BASE-UNIT -- Basis-Einheit
		   METRIC-TYPE -- :METRIC oder :SCIENTIFIC
                                  (opt., Default :METRIC)

Implicit Input	:  Prefix-Listen *METRIC-, *SCIENTIFIC-PREFIXES*

Implicit Output :  Neue Einheiten-Definitionen

Result		:  Liste der definierten Einheiten

Functional Desc.:  Gemaess den Prefix-Listen werden neue Einheiten durch
		   Vorsetzen des Prefix an die Basis-Einheit gebildet und mit
		   DEFINE-UNIT-FCN definiert. [Z.B. zu |m| (Meter) wird mit
		   einem Prefix-Eintrag (#\m 1E-3) (Milli-) die Definition
		   |mm| = 1E-3 |m| durchgefuehrt.]
|#

(defun define-metric-units (base-unit &optional (metric-type :metric))
  (mapcar #'(lambda (prefix)
	      (define-unit-fcn
                (intern (concatenate 'string
				     (string (car prefix))
				     (string base-unit))
			*ms-package*)
                (second prefix)
                base-unit))
	  (case metric-type
	    (:metric *metric-prefixes*)
	    (:scientific *scientific-prefixes*)
	    (t (error "Invalid key in unit definition: ~S." metric-type)))))


;;;; ------------------------- DEFINE-UNIT ----------------------------------
#|
Name 		:  define-unit

Abstract	:  Oberflaechenmakro zur Definition von Einheiten und Groessen

Arguments	:  UNIT -- Einheit
		   UNITS -- andere Einheiten und Angaben, Syntax:
		     {Einheit|Faktor}* [:BASE Groesse [:METRIC|:SCIENTIFIC]]

Implicit Output :  Definition von Einheiten und Groessen

Result		:  UNIT

Functional Desc.:  Je nach Vorhandensein der Keywords :BASE und
		   :METRIC/:SCIENTIFIC werden unterschiedliche Aufrufe an die
		   DEFINE-UNIT-Funktionen generiert:

		   (DEFINE-UNIT |m| :BASE length :metric)
 		   =>
		   (PROGN
		    (DEFINE-BASE-UNIT-FCN '|m| 'length)
		    (DEFINE-METRIC-UNITS '|m| :metric))

		   (DEFINE-UNIT |qm| |m| |m| :BASE area)
		   =>
		   (DEFINE-BASE-UNIT-FCN '|qm| 'area '(|m| |m|))

 		   (DEFINE-UNIT |km| 1000 |m|)
		   =>
		   (APPLY '#DEFINE-UNIT-FCN '|km| '(1000 |m|))
|#

;;;-exported-
(defmacro define-unit (unit &rest units)
"   DEFINE-UNIT  unit {unit|factor}* [:BASE measurement [:METRIC|:SCIENTIFIC]]

    Defines UNIT as a new unit derived from units and scale factors.
    If :BASE is given, UNIT is defined as a base unit for the new MEASUREMENT.
    If :METRIC or :SCIENTIFIC are specified, corresponding units are defined
    automatically.
"
  (let ((base (member :base units)))
    (case (length base)
      (2 `(define-base-unit-fcn ',unit ',(second base) ',(ldiff units base)))
      (3 `(multiple-value-bind (unew mnew)
	     (define-base-unit-fcn ',unit ',(second base) ',(ldiff units base))
	   (values unew mnew (define-metric-units ',unit ,(third base)))))
      (0 `(apply #'define-unit-fcn ',unit ',units))
      (t (error "Illegal :base specification.")))))

(defmacro defmeasure (measure-name base-expr &key units format)
  (let ((base-unit (if (typep base-expr '(or string symbol))
		       base-expr
		       (car base-expr)))
	(defined-as (if (consp base-expr) (cdr base-expr) nil))
	(metric (if (eq units ':metric) ':metric
		    (and (consp units) (find ':metric units))))
	(scientific (if (eq units ':scientific) ':scientific
			(and (consp units) (find ':scientific units)))))
    (if (and metric scientific)
	(error "Only one of :metric or :scientific must be specified."))
    `(progn
      (define-base-unit-fcn ',base-unit ',measure-name ',defined-as)
      ,(if (or metric scientific)
	   `(define-metric-units ',base-unit ,(or metric scientific)))
      ,@(if (consp units)
	    (mapcar #'(lambda (u) `(define-unit ,@(if (consp u) u (list u))))
		    (remove-if #'keywordp units)))
      ,(if format
	   `(set-unit-format (measure-named ',measure-name)
			     ',(if (symbolp format) (list format) format)))
      (measure-named ',measure-name))))

;;;; ------------------------- DELETE-UNIT ----------------------------------
#|
Name 		:  delete-unit

Abstract	:  Loeschen einer Einheit

Arguments	:  UNIT -- Einheit

Implicit Output :  UNIT und ggf. der zugehoerige Measure-Frame werden
		   geloescht.

Result		:  T

Functional Desc.:  Wenn UNIT eine Basis-Einheit bezeichnet, wird die
		   zugehoerige Groesse geloescht, andernfalls nur UNIT mit
		   DELETE-SINGLE-UNIT. 

Local Functions :  DELETE-SINGLE-UNIT -- loescht eine einzelne, abgeleitete
		   Einheit und die zugehoerige Inverse-Einheit. Verweise aus
		   der Basis-Einheit und dem Measure-Frame der zugehoerigen
		   Groesse werden entfernt. Die P-List der Einheitensymbole
		   wird bereinigt.
|#

;;;-exported-
(defun delete-unit (unit)
"   DELETE-UNIT  unit

    Deletes UNIT. If UNIT names a base unit, the corresponding measurement and
    all derived units are deleted as well.
"
  (etypecase unit
    (symbol unit)
    (string (setq unit (intern unit *ms-package*))))
  (cond ((not (unit-p unit))
	 (error "Not defined as unit: ~A." unit))
	((base-unit-p unit)
	 (delete-measure (measure-of unit) :base-unit unit))
	(t (delete-single-unit unit))))

(defun delete-single-unit (unit)
  (let* ((ms (measure-of unit)))
    (setf (ms-scale ms)
	  (delete unit (ms-scale ms) :key #'car))
    (remprop unit 'measure)
    t))


;;;; ------------------------- DELETE-OBJECT --------------------------------
#|
Name 		:  delete-object (measure)

Abstract	:  Methode zum Loeschen des Measure-Frames einer Groesse

Arguments	:  M -- Measure-Frame

Implicit Input  :  *NO-UNIT-DELETION-WARNINGS*

Implicit Output :  Alle Einheiten dieser Groesse und alle abgeleiteten
		   Groessen werden geloescht.

Result		:  T

Functional Desc.:  Beim Loeschen einer Groesse werden auch alle ihre Einheiten
		   geloescht. Ist die Groesse primitiv, werden auch alle
		   abgeleiteten Groessen geloescht. Sofern nicht
		   *NO-UNIT-DELETION-WARNINGS* T ist, wird zur Sicherheit ein
		   entsprechender Continuable Error generiert.

		   Von der Basis-Einheit und allen anderen Einheiten werden
		   die P-Listen bereinigt.
		   Beim Entfernen von Eintraegen aus der
		   *UNIT-CONVERSION-LIST* werden alle abgeleiteten Groessen
		   bestimmt (YET-TO-DLETE). Diese und M's inverse Groesse
		   werden anschliessend geloescht. Damit waehrenddessen nicht
		   erneut eine Fehlermeldung generiert wird, wird
		   *NO-UNIT-DELETION-WARNINGS* temporaer auf T gesetzt.

		   Abschliessend wird M selbst geloescht (RUN-SUPER).
|#

(defvar *no-warnings-default-p* nil)

(macrolet
  ((d-m-body ()
     `(progn
       (when (and base-unit (not no-warnings)
		  (> (length (ms-scale m)) 1))
	 (cerror "All other units of this measure will be deleted, too."
		 "Deleting a base-unit: ~S, measure ~S."
		 base-unit (measure-name m)))
       (let ((derived-ms (if derived nil (and (primitive-p m)
					      (get-derived-measures m)))))
	 (when (and derived-ms (not no-warnings))
	   (cerror "All derived measures (~*~{~S~^, ~}) will be deleted, too."
		   "Deleting a Primitive Measure: ~S."
		   (measure-name m) (mapcar #'measure-name derived-ms)))
	 (dolist (u (mapcar #'car (ms-scale m)))
	   (remprop u 'measure))
	 (dolist (d derived-ms)
	   (delete-measure d :derived t))
	 (set-prim-id-to-measure (ms-prim-id m) nil)))))

  (defun delete-measure (m &key derived base-unit (no-warnings *no-warnings-default-p*))
    (if (symbolp m) (setq m (measure-named m)))
    (d-m-body)
    t)
  ) ;macrolet

(defun derived-measure-p (m1 m2)
  (flet ((contains-divisor (n d) (zerop (rem n d))))
    (or (contains-divisor (numerator (ms-prim-id m2)) (ms-prim-id m1))
	(contains-divisor (denominator (ms-prim-id m2)) (ms-prim-id m1)))))

(defun get-derived-measures (ms)
  (remove ms
	  (remove ms (list-all-measures))
	  :test-not #'derived-measure-p))


;;;; ------------------------- MEASURING-P ---------------------------------
#|
Name 		:  measuring-p

Abstract	:  Testet, ob eine dimensionierte Zahl zu einer Groesse
		   gehoert.

Arguments	:  DIM -- dimensionierte Zahl
		   MEASURE -- Groesse (als Symbol oder Objekt)

Result		:  T oder NIL

Functional Desc.:  Liefert T, wenn DIM eine einfache Einheit enthaelt
		   (SIMPLE-UNIT-P), und diese zu MEASURE gehoert.
|#

;;;-exported-
(defun measuring-p (dim measure)
"    MEASURING-P dim-number measure

     T, if dim-number belongs to measure (given as symbol or object).
"
  (check-type dim dim-number)
  (check-type measure (or measure symbol))
  (let ((dm (dim-measure dim)))
    (cond ((null dm) nil)
	  ((measure-p measure) (eq dm measure))
	  (t (eq (measure-name dm) measure)))))

;;;; Rechnen

(declaim (inline dim-same-units-p))
(defun dim-same-units-p (x y)
  ;; Returns T if X and Y are of the same (ie, compatible for addition) units
  (= (dim-prim-id x) (dim-prim-id y)))

(defun dim-eql (x y)
  (if (and (dim-number-p x) (dim-number-p y))
      (and (= (dim-prim-id x) (dim-prim-id y))
	   (sdim= x y))
    (eql x y)))

(defun dim-equal (x y)
  (cond ((and (dim-number-p x) (dim-number-p y))
	 (and (= (dim-prim-id x) (dim-prim-id y))
	      (sdim= x y)))
	((or (atom x) (atom y))
	 (equal x y))
	(t ;; Must be lists
	 (and (dim-equal (first x) (first y))
	      (dim-equal (cdr x) (cdr y))))))

(defun dim-equalp (x y)
  (typecase x
    (CONS 
     (and (consp y)
	  (dim-equalp (first x) (first y))
	  (dim-equalp (cdr x) (cdr y))))
    (STRING (equalp x y))
    (DIM-NUMBER
     (and (dim-number-p y)       
	  (= (dim-prim-id x) (dim-prim-id y))
	  (sdim= x y)))
    (VECTOR 
     (and (vectorp y)
	  (= (length x) (length y))
	  (loop for i fixnum below (length x)
	      always (dim-equalp (aref x i) (aref y i)))))
    (ARRAY   ;; DO THIS:
     (and (arrayp y)
	  (equal (array-dimensions x) (array-dimensions y))
	  (loop for i fixnum below (array-total-size x)
	      always (dim-equalp (row-major-aref x i) 
				 (row-major-aref y i)))))
    (t (equalp x y))) )

(defun dim-zerop (dim)
  (etypecase dim
    (number (zerop dim))
    (dim-number (zerop (%dim-value dim)))))

(defun safe-apply-additive-operator
       (operator args &optional (args-no-0 (member 0 args :test-not #'eql)))
  (cond ((null args-no-0) 0)
	((numberp (car args-no-0))
	 ;(every #'numberp args-no-0)  ; Use this to support percent?
	 (apply operator args))
	((boundp '*tmp-unit-output-format*)
	 (let* ((val (apply operator (mapcar #'dim-value0 args)))
		(pid (dim-prim-id (car args-no-0)))
		(meas (prim-id-to-measure pid))
		(mformat (and meas (ms-output-format meas)))
		(unit0 (and mformat
			    (eq (first mformat) :unit)
			    (or (symbolp (second mformat))
				(find-symbol (second mformat) *ms-package*))
			    (second mformat))))
	   (if unit0
	       (%make-xdim-number
		:value (rationalize val)
		:prim-id pid
		:pref-unit unit0)
	       (make-dim-number :value val :pid pid)) ))
	(t (make-dim-number :value (apply operator (mapcar #'dim-value0 args))
			    :pid (dim-prim-id (car args-no-0))))))

(defun apply-additive-operator (operator args)
  (let ((args-no-0 (remove 0 args)))
    (cond ((null args-no-0) 0)
	  ((find (dim-prim-id (car args-no-0))
		 (cdr args-no-0)
		 :key #'dim-prim-id
		 :test-not #'eql)
	   (error "Incompatible measures: ~{~A~^, ~}." args))
	  (t (safe-apply-additive-operator operator args args-no-0)))))

(defmacro def-dim-add-op (op)
  `(progn
    (defun ,(intern (format nil "DIM~@[~*-~]~S"
			    (alpha-char-p (char (string op) 0))
			    op))
	   (&rest args)
      (apply-additive-operator #',op args))
    (defun ,(intern (format nil "SDIM~@[~*-~]~S"
			    (alpha-char-p (char (string op) 0))
			    op))
	   (&rest args)
      (safe-apply-additive-operator #',op args))))

(def-dim-add-op +)
(def-dim-add-op -)
(def-dim-add-op min)
(def-dim-add-op max)


(defun safe-apply-predicate (pred args)
  (apply pred (mapcar #'dim-value0 args)))

(defun apply-predicate (pred args)
  (let ((args-no-0 (remove 0 args)))
    (cond ((null args-no-0) (funcall pred 0 0))
	  ((find (dim-prim-id (car args-no-0))
		 (cdr args-no-0)
		 :key #'dim-prim-id
		 :test-not #'eql)
	   (error "Incompatible measures: ~{~A~^, ~}." args))
	  (t (safe-apply-predicate pred args)))))

(defmacro def-dim-pred (op)
  `(progn
    (defun ,(intern (format nil "DIM~@[~*-~]~S"
			    (alpha-char-p (char (string op) 0))
			    op))
	   (&rest args)
      (apply-predicate #',op args))
    (defun ,(intern (format nil "SDIM~@[~*-~]~S"
			    (alpha-char-p (char (string op) 0))
			    op))
	   (&rest args)
      (safe-apply-predicate #',op args))))

(def-dim-pred <)
(def-dim-pred <=)
(def-dim-pred =)
(def-dim-pred /=)
(def-dim-pred >=)
(def-dim-pred >)


(defun apply-mult-operator (op args)
  (let ((val (apply op (mapcar #'dim-value0 args)))
	(pid (apply op (mapcar #'dim-prim-id args))))
    (cond ((= 1 pid) val)
	  ((boundp '*tmp-unit-output-format*)
	   (let* ((meas (prim-id-to-measure pid))
		  (mformat (and meas (ms-output-format meas)))
		  (unit0 (and mformat
			      (eq (first mformat) :unit)
			      (or (symbolp (second mformat))
				  (find-symbol (second mformat) *ms-package*))
			      (second mformat))))
	     (if unit0
		 (%make-xdim-number
		  :value (rationalize val)
		  :prim-id pid
		  :pref-unit unit0)
	       (make-dim-number :value val :pid pid)) ))
	  (t (make-dim-number :value val :pid pid)))))

(defun dim* (&rest args)
  (apply-mult-operator #'* args))

(defun dim/ (&rest args)
  (apply-mult-operator #'/ args))

(defun dim-sqrt (dim)
  (if (numberp dim) (sqrt dim)
      (let ((sqrt-npid (sqrt (numerator (%dim-prim-id dim))))
	    (sqrt-dpid (sqrt (denominator (%dim-prim-id dim)))))
	(if (and (= (floor sqrt-npid) sqrt-npid)
		 (= (floor sqrt-dpid) sqrt-dpid))
	    (make-dim-number
	     :value (if *ratios-only*
			(rational (sqrt (%dim-value dim)))
			(sqrt (%dim-value dim)))
	     :pid (/ (floor sqrt-npid) (floor sqrt-dpid)))
	    (error "Can't calculate square root from unit ~A."
		   (prim-id-to-unit (%dim-prim-id dim)))))))

(defun dim-expt (dim power)
  (cond ((zerop power) 1)
	((numberp dim) (expt dim power))
	((not (typep power 'rational))
	 (error "Power-number ~S must be rational for dim numbers." power))
	((integerp power)
	 (make-dim-number
	  :value (expt (%dim-value dim) power)
	  :pid (expt (%dim-prim-id dim) power)))
	(t (let ((expt-npid (expt (numerator (%dim-prim-id dim)) power))
		 (expt-dpid (expt (denominator (%dim-prim-id dim)) power)))
	     (if (and (= (floor expt-npid) expt-npid)
		      (= (floor expt-dpid) expt-dpid))
		 (make-dim-number
		  :value (if *ratios-only*
			     (rational (expt (%dim-value dim) power))
			     (expt (%dim-value dim) power))
		  :pid (/ (floor expt-npid) (floor expt-dpid)))
		 (error "Can't raise unit ~A to power ~A."
			(prim-id-to-unit (%dim-prim-id dim)) power))))))


;;;;===================== Ein-/Ausgabe dimensionierter Zahlen ==============


;;;; ------------------------- [SIGNED-]DIM-NUMBER-READER --------------------
#|
Name 		:  dim-number-reader, signed-dim-number-reader

Abstract	:  Reader-Funktion fuer dimensionierte Zahlen

Arguments	:  STREAM -- Eingabe-Stream
		   CHAR -- ausloesendes Zeichen

Implicit Input  :  *RATIOS-ONLY*

Result		:  je nach Eingabe eine Zahl, eine dimensionierte Zahl oder
		   ein Symbol.

Functional Desc.:  Die Reader-Funktion DIM-NUMBER-READER ist an alle Ziffern
		   gebunden, SIGNED-DIM-NUMBER-READER an #\+ und #\- (siehe
		   INSTALL-DIM-NUMBER-READER).

		   DIM-NUMBER-READER liest die Eingabe zeichenweise. Solange
		   eine Zahl aufgebaut wird, werden die Zeichen im
		   *NUMBER-BUFFER* gesammelt. MODE kontrolliert die
		   begleitende Syntax-Kontrolle. Moegliche Werte sind
		   READING-NUMBER (Anfangswert), READING-FLOAT sobald ein #\.
		   aufgetreten ist, READING-RATIO sobald ein #\/ aufgetreten
		   ist, READING-EXPONENT sobald ein Exponenten-Bezeichner
		   (e,s,f,d,l) aufgetreten ist. Bei
		   unzulaessiger Zahl-Syntax wird auf READING-SYMBOL
		   umgeschaltet und abschliessend ein Symbol geliefert.

		   Sobald ein Unit-Char (UNIT-CHAR-P) oder ein #\/ gefolgt von
		   einem Non-Digit auftritt, werden die verbleibenden Zeichen
		   auf *UNIT-BUFFER* gesammelt, MODE ist dann READING-UNIT.
		   Treten dabei Symbol-Konstituenten auf, die keine Unit-Chars
		   sind, wird ebenfalls auf READING-SYMBOL umgeschaltet.

		   Der Leseprozess ist beendet, wenn ein terminierendes
		   Zeichen (TERMINATING-CHAR-P) auftritt oder STREAM
		   erschoepft ist. In Abhaengigkeit von MODE wird dann das
		   gelesene Objekt wie folgt generiert:
		   MODE = READING-UNIT: eine :dim-number wird generiert, deren
		   Value aus *NUMBER-BUFFER* mit READ-NUMBER-FROM-STRING
                   gelesen wird
		   und deren Unit mit INTERN (wegen der Case-Sensitivity beim
		   Lesen von Einheiten-Symbolen) aus *UNIT-BUFFER* gebildet
		   wird.
		   MODE = READING-SYMBOL: ein Symbol wird READ-FROM-STRING aus
		   den aneinandergehaengten *NUMBER-BUFFER* und *UNIT-BUFFER*
		   gelesen.
		   MODE sonst einer der Number-Modes: eine Zahl wird mit
		   READ-NUMBER-FROM-STRING aus *NUMBER-BUFFER* gelesen.

		   [Anm.: Das Leseverhalten entspricht dem des normalen
		   Readers fuer Zahlen und Symbole. Eine kompaktere Loesung
		   waere eigentlich das Lesen des gesamten Objektes mit READ
		   gewesen. Dies haette ein Symbol geliefert, dessen Pname
		   dann in Zahl und Unit zerlegt werden koennte. Die ganze
		   Problematik verschiedener Zahldarstellungen und der
		   Erkennung des Objekt-Endes waere dann von den
		   Standard-Funktionen uebernommen worden. Leider gibt es bei
		   diesem Ansatz keine Moeglichkeit in C'Lisp, die
		   Case-Sensitivity beizubehalten, da READ grundsaetzlich alle
		   gelesen Alpha-Character in Grossbuchstaben umwandelt.]

		   SIGNED-DIM-NUMBER-READER wird beim Lesen von #\+ und #\-
		   angestossen. Folgt auf das Vorzeichen eine Ziffer, so wird
		   DIM-NUMBER-READER zum Lesen der Zahl oder dimensionierten
		   Zahl verwendet, andernfalls wird das Vorzeichen auf den
		   Stream zurueckgegeben (UNREAD-CHAR) und der Input als
		   Symbol mit READ gelesen.

		   Die global genutzten Variablen *STANDARD-READTABLE* (zum
		   zeitweiligen Ausblenden der Read-Makros waehrend des
		   Dim-Number-Readers), *UNIT-BUFFER* und *NAME-BUFFER* werden
		   ausschliesslich hier benoetigt und sind daher hier
		   definiert.
		   
Local Functions :  UNIT-CHAR-P liefert T, wenn ein Zeichen in einem
		   Unit-Symbol enthalten sein kann, dies gilt fuer alle
		   Alpha-Character und in *UNIT-CHARACTERS* explizit genannte
		   Sonderzeichen. (Ziffern werden im Dim-Number-Reader
		   gesondert behandelt.)

		   DIGIT-OR-SIGN-P liefert T fuer Ziffern und Vorzeichen.

		   EXPONENT-CHAR-P liefert T fuer einen gueltigen Exponent-
		   Bezeichner.

		   TERMINATING-CHAR-P liefert T fuer alle nicht graphischen
		   Zeichen (#\Newline usw.), #\Space und alle Zeichen mit
		   einer terminierenden Makro-Definition.

		   PEEK-CHAR* -- PEEK-CHAR primitiv redefiniert, da die
		   Original-Version in Lucid im Zusammenhang mit dem Editor
		   fehlerhaft arbeitet.

                   READ-NUMBER-FROM-STRING -- fuehrt READ-FROM-STRING durch,
                   wenn RATIOS-ONLY nil ist, sonst wird jede Zahl (also auch
		   Float- und Exponential-Darstellungen) als Ratio gelesen.
|#

(defvar *standard-readtable* (copy-readtable nil))
(defvar *dim-number-readtable* (copy-readtable *readtable*))

(defun read-token-preserving-case (stream char)
  (unread-char char stream)
  (let ((old-readtable-case (readtable-case *readtable*)))
    (unwind-protect
	(progn
	  (setf (readtable-case *readtable*) ':preserve)
	  (read stream))
      (setf (readtable-case *readtable*) old-readtable-case))))


#||
;; Can't unintern the symbol, because if it is the second time we are
;;   reading this same symbol value, then we end up uninterning an existing
;;   symbol!  The only way to avoid creating unnecessary symbols is to 
;;   use the original read-token code.
(defun safe-parse-dim-number (string &key (start 0) (end (length string))
				          signed
					  (unit-pos (position-if #'unit-char-p string
								 :start start
								 :end end))
					  #+(or :CLTL2X :ANSI-CL) symbol)
  (let ((*readtable* *standard-readtable*))
    (cond ((or (null unit-pos) (zerop unit-pos)
	       (and signed (not (digit-char-p (char string (1+ start))))))
	   (read-from-string string t nil :start start :end end))
	  (t #+(or :CLTL2X :ANSI-CL) (if symbol (unintern symbol))
	     (if (char= (char string (1- unit-pos)) #\/)
		 (decf unit-pos))
	     (values
	      ;; erster Wert: (Symbol, falls irgendetwas schief geht)
	      #+:FRESKO
	      (or (on-error-return nil
		    (make-dim-number :value (read-number-from-string
					     string :start start :end unit-pos)
				     :unit (subseq string unit-pos end)))
		  (read-from-string string t nil :start start :end end))
	      #+(and (or :CLTL2 :ANSI-CL) (not :FRESKO))
	      (or (ignore-errors
		    (make-dim-number :value (read-number-from-string
					     string :start start :end unit-pos)
				     :unit (subseq string unit-pos end)))
		  (read-from-string string t nil :start start :end end))
	      #-(or :FRESKO :CLTL2 :ANSI-CL)
	      (make-dim-number :value (read-number-from-string
				       string :start start :end unit-pos)
			       :unit (subseq string unit-pos end))
	      ;; zweiter Wert:
	      end)))))
||#

(defun safe-parse-dim-number (string &key (start 0) (end (length string))
				          signed
					  (unit-pos (position-if #'unit-char-p string
								 :start start
								 :end end))
					  symbol)
  (let ((*readtable* *standard-readtable*)
	dim-number)
    (cond ((or (null unit-pos) (zerop unit-pos)
	       (and signed (not (digit-char-p (char string (1+ start))))))
	   (read-from-string string t nil :start start :end end))
	  (t (if (char= (char string (1- unit-pos)) #\/)
		 (decf unit-pos))
	     (setq dim-number
	       (ignore-errors
		    (make-dim-number :value (read-number-from-string
					     string :start start :end unit-pos)
				     :unit (subseq string unit-pos end))))

	     (if (and symbol dim-number)
                 (unintern symbol))
	     (values
	      ;; erster Wert: (Symbol, falls irgendetwas schief geht)
	      (or dim-number
		  (read-from-string string t nil :start start :end end))
	      ;; zweiter Wert:
	      end)))))

(defun parse-dim-number (string &key (start 0) (end (length string)))
  (let ((unit-pos (position-if #'unit-char-p string
			       :start start :end end)))
    (if (and unit-pos
	     (< unit-pos end)
	     (digit-or-sign-p (char string start)))
	(safe-parse-dim-number string :start start :end end
			       :unit-pos unit-pos)
	(cerror "Return NIL."
		"No dim-number found in ~S."
		(subseq string start end)))))
	     
(defun dim-number-reader (stream char)
  (let ((token (let ((*readtable* *standard-readtable*))
		 (read-token-preserving-case stream char))))
    (if (symbolp token)
	(safe-parse-dim-number (string token) :symbol token)
	token)))
      
(defun signed-dim-number-reader (stream char)
  (let ((token (let ((*readtable* *standard-readtable*))
		 (read-token-preserving-case stream char))))
    (if (symbolp token)
	(safe-parse-dim-number (string token) :symbol token :signed t)
	token)))
      
(defun dispatch-dim-number-reader (stream char ignore)
  (declare (ignore char ignore))
  (let ((first (read-char stream)))
    (cond ((digit-char-p first)
	   (dim-number-reader stream first))
	  ((member first '(#\+ #\-) :test #'char=)
	   (signed-dim-number-reader stream first))
	  (t (error "No dim-number to be read from stream ~S." stream)))))

(defparameter *unit-characters* (list #\$))

(defun unit-char-p (ch &optional extended)
  (or (and (alpha-char-p ch) (not (char-equal ch #\e)))
      (and extended (member ch '(#\. #\/ #\- #\e #\E)))
      (member ch *unit-characters*)))

  ;; This is like unit-char-p, but allows "e", so it can be used
  ;;   for parsing once we know a unit has been started.  This allows
  ;;   the use of "e" inside unit names like week or year!;
(defun constituent-unit-char-p (ch &optional extended)        ; tar
  (or (alpha-char-p ch)
      (and extended (member ch '(#\. #\/ #\-)))
      (member ch *unit-characters*)))
    
(defun declare-unit-char (ch)
  (progn (push ch *unit-characters*) ch))
  
(defun unit-characters () *unit-characters*)

(defun digit-or-sign-p (ch)
  (or (digit-char-p ch)
      (char= ch #\+)
      (char= ch #\-)))

(defun terminating-char-p (c)
  (or (not (graphic-char-p c))
      (char= c #\space)
      (multiple-value-bind (macro-def non-terminating)
			   (get-macro-character c)
	(and macro-def (not non-terminating)))))

(defun read-number-from-string (string &key (start 0) (end (length string)))
  (if *ratios-only*
      (multiple-value-bind (int junk)
			   (parse-integer string
					  :start start :end end
					  :junk-allowed t)
	(cond ((= junk end) int)
	      ((char= (char string junk) #\/)
	       (/ int (parse-integer string :start (1+ junk) :end end)))
	      ((char= (char string junk) #\.)
	       (multiple-value-bind (frac more-junk)
				    (parse-integer string :start (1+ junk)
						   :end end
						   :junk-allowed t)
		 (let ((num (+ int (/ (or frac 0)
				      (expt 10 (- more-junk junk 1))))))
		   (if (< more-junk end)
		       (* num (expt 10 (parse-integer string
						      :start (1+ more-junk)
						      :end end)))
		       num))))
	      #| kann weg...?
	      ((exponent-char-p (char string junk))
	       (* int (expt 10 (parse-integer string :start (1+ junk)
					      :end end))))
	      |#))
      (read-from-string string t nil :start start :end end)))

;;;; ------------------------- INSTALL-DIM-NUMBER-READER ---------------------
#|
Name 		:  install-dim-number-reader

Abstract	:  Installiert Reader fuer dimensionierte Zahlen

Arguments	:  keine

Implicit Output :  Modifikation der *READTABLE*

Result		:  unbestimmt

Functional Desc.:  Traegt fuer #\+ und #\- SIGNED-DIM-NUMBER-READER, fuer alle
		   Ziffern DIM-NUMBER-READER als Read-Makros in *READTABLE*
		   ein.
|#

;;;-exported-
(defun install-dim-number-reader (&key permanent dispatch
				      (readtable *dim-number-readtable* rt-p)
				      (dispatch-char #\#))
  (when permanent 
    (unless rt-p (setq readtable *readtable*))
    ;; Below was (copy-readtable *readtable*), but that causes infinite recursion
    ;;  problems if this function is called more than once!
    (setq *standard-readtable* (copy-readtable nil)))
  (dolist (digit '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (set-macro-character digit #'dim-number-reader
			 t ;t
                         readtable))
  (dolist (digit '(#\+ #\-))
    (set-macro-character digit #'signed-dim-number-reader
			 t ;t
                         readtable))
  (when dispatch
    (set-dispatch-macro-character
     dispatch-char dispatch #'dispatch-dim-number-reader
     *readtable*)))


;;;; ------------------------- DECLARE-UNIT-CHAR ----------------------------
#|
Name 		:  declare-unit-char

Abstract	:  Deklariert Sonderzeichen als Konstituent fuer Einheiten

Arguments	:  CH -- Zeichen

Implicit Output :  Modifikation von *UNIT-CHARACTERS*

Result		:  CH

Functional Desc.:  Fuegt CH in *UNIT-CHARACTERS* ein.
|#



(defun read-dim-number (&optional (stream *standard-input*))
  (dim-number-reader stream (read-char stream)))

(defmacro pdn (string) `(values (parse-dim-number ,string)))

(defun sdn (dim-op &rest args)
  (apply dim-op
	 (mapcar #'(lambda (arg)
		     (etypecase arg
		       (number arg)
		       (symbol (parse-dim-number (string arg)))
		       (string (parse-dim-number arg))
		       (dim-number arg)))
		 args)))


;;;; ------------------------- CHECK-PRECISION ------------------------------
#|
Name 		:  check-precision

Abstract	:  Testet Formatbeschreibungen auf das Vorhandensein einer
		   Precision-Angabe. 

Arguments	:  FORMAT-DESC -- Format-Beschreibung

Result		:  ggf. korrigierte Format-Beschreibung

Functional Desc.:  Wenn FORMAT-DESC  nicht auf eine Zahl endet, wird NIL (fuer
		   "keine Precision-Angabe" ergaenzt).
|#

(defun check-precision (format-desc)
  (if (numberp (car (last format-desc)))
      format-desc
      (append format-desc (list nil))))


;;;; ------------------------- DEFINE-UNIT-FORMAT ---------------------------
#|
Name 		:  define-unit-format

Abstract	:  Definiert Ausgabe-Format permanent

Arguments	:  MEASURE -- Groesse
		   FORMAT-DESC -- Format-Beschreibung

Implicit Input  :  *VALID-FORMAT-KEYS*

Implicit Output :  *UNIT-OUTPUT-FORMAT* modifiziert

Result		:  MEASURE

Functional Desc.:  Die Format-Beschreibung wird ueberprueft (der angegebene
		   Format-Key muss in *VALID-FORMAT-KEYS* enthalten sein, eine
		   einzeln genannte Einheit wird um :UNIT ergaenzt) und unter
		   dem angegebenen MEASURE in *UNIT-OUTPUT-FORMAT*
		   eingetragen. Ein frueherer Eintrag zum selben MEASURE wird
		   ggf. geloescht. 
|#

;;;-exported-



;;;; ------------------------- WITH-UNIT-FORMAT -----------------------------
#|
Name 		:  with-unit-format, print-converted

Abstract	:  Definiert temporaer ein Ausgabeformat fuer Masseinheiten

Arguments	:  FORMAT-DESC -- Format-Beschreibung oder Liste von
		                  Format-Beschreibungen.
		   BODY -- auszufuehrende Anweisungen

Implicit Input	:  *UNIT-OUTPUT-FORMAT*

Implicit Output :  *TEMPORARY-UNIT-OUTPUT-FORMAT*, *USE-TEMPORARY-FORMAT*

Result		:  Wert der letzten Anweisung in BODY

Functional Desc.:  FORMAT-DESC wird ueberprueft, *UNIT-OUTPUT-FORMAT*
		   vorangestellt und *TEMPORARY-OUTPUT-FORMAT* zugewiesen.
		   (Fehlt eine Measure-Angabe, wird TMP-DEFAULT ergaenzt.)
		   *USE-TEMPROARY-FORMAT* ist fuer die Dauer des Makros T.
		   BODY wird in den Rumpf des LET eingebaut.

		   Anm.: PRINT-OBJECT fuer Dim-Numbers verwendet
		   *TEMPORARY-OUTPUT-FORMAT*, wenn non-nil, und setzt 
		   *TEMPORARY-OUTPUT-FORMAT* zurueck, sobald
		   *USE-TEMPORARY-FORMAT* NIL ist. Durch diesen Trick wird
		   erreicht, dass das temporaere Format eine Ausgabe ueber den
		   Gueltigkeitsbereich des Makros hinaus wirkt, d.h.
		   normalerweise fuer die Ausgabe dessen Ergebnisses. Dadurch
		   verhaelt sich z.B.
                     (WITH-UNIT-FORMAT (LENGTH :BASE) (DIM+ 12cm 40mm))
                   -- wie intuitiv erwartet -- so, dass das Ergebnis mit der
		   Basis-Einheit erscheint.
|#

(eval-when (:execute :load-toplevel :compile-toplevel)

(defparameter *valid-format-keys* '(:current :base :unit :best-fit :step))
    
(defun expand-format-description (fdesc)
  (cond ((and (symbolp fdesc)
              (member fdesc *valid-format-keys*)
              (not (eq fdesc ':unit)))
         (list fdesc nil))
        ((and (consp fdesc)
              (member (car fdesc) *valid-format-keys*))
         (valid-format-desc fdesc))
        ((and (consp fdesc)
              (symbolp (car fdesc)))
         (values nil (list (cons (car fdesc)
                                 (valid-format-desc
                                  (cdr fdesc) (measure-named (car fdesc)))))))
        ((and (consp fdesc)
              (every #'consp fdesc))
         (values (let ((def-fd (find-if #'keywordp fdesc :key #'car)))
                   (if def-fd (valid-format-desc def-fd) nil))
                 (mapcar #'(lambda (fd)
                             (cons (car fd)
                                   (valid-format-desc
                                    (cdr fd) (measure-named (car fd)))))
                         (remove-if #'keywordp fdesc :key #'car :count 1))))
        (t (error "Illegal format description: ~S." fdesc))))
    
(defun valid-format-desc (fd &optional ms &aux (last-fd (car (last fd))))
  (let ((check-units (not (eval-when (compile) t)))
        (key (car fd))
        (prec (if (or (null last-fd) (numberp last-fd)) last-fd))
        (units (if (or (null last-fd) (numberp last-fd))
                   (cdr (butlast fd)) (cdr fd)))
        (bad-units nil))
    (cond ((not (member key *valid-format-keys*))
           (error "Illegal format key: ~S." key))
          ((and (member key '(:current :base)) units)
           (error "No units may be specified for format type ~S: ~S."
                  key units))
          ((and (eq key ':unit) (null units))
           (error "A unit must be specified for format type ~S." key))
          ((and check-units ms
                (setq bad-units
		      (remove-if #'(lambda (u)
				     (or (find u (ms-scale ms) :key #'car :test #'string=)
					 (eql (ms-prim-id ms) (dim-prim-id (unit-to-dim-number u)))))
                                 units)))
           (error "Unknown units specified for measure ~S: ~S."
                  (measure-name ms) bad-units))
          (t (cons key (nconc (mapcar #'(lambda (u)
                                          (if (symbolp u) u (intern u *ms-package*)))
                                      units)
                              (list prec)))))))

) ; eval-when


(defmacro define-unit-format (measure &rest format-desc)
"   DEFINE-UNIT-FORMAT  measure [format-key] {unit}* [precision]

    Defines a unit output format for measure. Measure must be a valid measure
    or T to indicate a default specification.
    Format-Key can be one of :current, :base, :unit, :best-fit, :step
    optionally followed by units. If only one unit is given it is interpreted
    as if preceded by :unit. Precision may be any positive integer or 0.
    (For details see the manual.)
"
  `(set-unit-format (measure-named ',measure) ',format-desc))

(defun set-unit-format (ms format-desc)
  (if (symbolp ms) (setq ms (measure-named ms)))
  (let ((fd (valid-format-desc format-desc ms)))
    (setf (ms-output-format ms) fd)))

;;;-exported-
(defmacro with-unit-format (format-desc &body body)
"   WITH-UNIT-FORMAT  format-description {form}*

    Format-Description may be a list in the form
       ([measure] format-key {unit*} [precision])
    or a list of such lists. If no measure is given, format description will
    temporarily overwrite all permanent specifications.
    Forms will be executed as implicit progn with format description in
    effect, i.e. each printed dim-number will be output according to the
    format given.

    Also, any new dimensioned numbers created within the scope of this macro
    will have their preferred print unit set to a specified unit, if \"format-key\"
    is :unit.  This is currently the only way to give dim-numbers created as the
    result of arithmetic manipulations a preferred output unit.
"
  (multiple-value-bind (default-fd measure-fds)
		       (expand-format-description format-desc)
    (let ((measure-exprs
	   (mapcar #'(lambda (fd)
		       (let ((var (gensym)))
			 (cons
			  `(,var (shiftf (ms-output-format (measure-named ',(car fd))) ',(cdr fd)))
			  `((ms-output-format (measure-named ',(car fd))) ,var))))
		   measure-fds)))
      `(let ((*tmp-unit-output-format* ',default-fd)
	     ,@(mapcar #'car measure-exprs))
	 (unwind-protect
	   (progn ,@body)
	   (setf ,@(mapcan #'cdr measure-exprs)))))))


	     
;;;; ------------------------- PRINT-CONVERTED ------------------------------
#|
Name 		:  print-converted

Abstract	:  Ausgabe einer einzelnen konvertierten dim. Zahl

Arguments	:  DIM -- dim. Zahl
   		   UNIT-FORMAT -- Formatbeschreibung

Result		:  DIM

Functional Desc.:  DIM wird in einer WITH-UNIT-FORMAT-Umgebung ausgegeben.
|#

;;;-exported-
(defmacro print-converted (dim &rest unit-format)
"   PRINT-CONVERTED  dim-number [format-key] {unit}* [precision]

    Prints dim-number in the format described by format-key, optionally
    followed by units and a precision value.
"
  `(with-unit-format ,unit-format (print ,dim)))


;;;; ------------------------- PRINT-OBJECT ---------------------------------
#|
Name 		:  print-object (:dim-number)

Abstract	:  Print-Methode fuer dim. Zahlen

Arguments	:  DIM -- dim. Zahl
		   STREAM -- Ausgabe-Stream

Implicit Input	:  Globale Variablen zur Ausgabeformatierung,
		   *SIMPLIFICATION-TIME*.

Implicit Output :  Ausgabe von DIM auf STREAM.

Result		:  DIM

Functional Desc.:  Wenn *PRINT-FORMATTED-DIM* NIL ist, fuehre eine normale
		   Ausgabe aus (RUN-SUPER).
		   Wenn DIM noch nicht vereinfacht ist und
		   *SIMPLIFICATION-TIME* :AT-OUTPUT ist, normalisiere und
		   vereinfache DIM.

		   Wenn DIM erfolgreich vereinfacht wurde und fuer DIM's Unit
		   eine Groesse definiert ist, dann wird das fuer die Groesse
		   definierte Ausgabeformat bestimmt und DIM entsprechend
		   ausgegeben. Andernfalls wird DIM "so wie es ist" mit
		   PRINT-FORMATTED-DIM ausgegeben.

		   Das Ausgabeformat wird aus *TEMPORARY-OUTPUT-FORMAT* (wenn
		   gegeben) oder sonst *UNIT-OUTPUT-FORMAT* bestimmt. Es wird
		   zuerst ein Eintrag fuer TMP-DEFAULT gesucht, dann einer
		   fuer die Groesse der auszugebenden Einheit, dann einer fuer
		   T als Default.
		   Wenn *USE-TEMPORRAY-FORMAT* NIL ist, setze
		   *TEMPORARY-OUTPUT-FORMAT* auch auf NIL
		   (s. WITH-UNIT-FORMAT).

		   Die Ausgabe von DIM erfolgt ueber PRINT-FORMATTED-DIM. Je
		   nach Format-Key im gefundenen MODE werden zusaetzlich
		   folgende Umwandlungen vorgenommen:
		     :BASE - falls noch nicht geschehen, wird DIM in die
		      Basiseinheit von UNIT umgewandelt (CONVERT-TO-UNIT).
		     :UNIT - DIM wird zur angegebenen Einheit umgewandelt.
		     :BEST-FIT - DIM wird zur durch BEST-FIT bestimmten
		      Einheit umgewandelt.
		     :STEP - die Ausgabe erfolgt iterativ ueber eine mit
		      STEPWISE-DIM-LIST generierte Werte-Liste (da die Umwand-
		      lung von DIM destruktiv geschieht, wird eine Kopie von
		      DIM verwendet). Die einzelnen Werte werden mit
		      PRINT-FORMATTED-DIM auf einen String geschrieben und mit
		      FORMAT hintereinander und mit #\: getrennt ausgegeben.
		     :CURRENT -- DIM wird unveraendert ausgegeben.
		   Ist keine MODE-Angabe gefunden worden, wird DIM wie mit
		   :CURRENT ausgegeben.

Local Functions :  PRINT-FORMATTED-DIM -- gibt DIM unter Beruecksichtigung von
		   PRECISION auf STREAM aus. Print-Methods werden abgeschaltet
		   (um einen rekursiven Aufruf zu verhindern).
		   Ist PRECISION angegeben, wird DIM's Wert mit als Float
		   mit PRECISION Stellen hinter dem Komma ausgegeben ("~,VF"
		   im Format-String). Andernfalls wird der Wert als Integer
		   ausgegeben, wenn es eine ganze Zahl ist, sonst als
		   (unbegrenztes) Float ("~:[~F~;~D~]" gesteuert durch
		   INTEGERP). Eine einzelne Einheit (Normalfall) wird direkt
		   anschliessend ausgegeben, mehrere (unvereinfachte oder
		   unvereinfachbare) Einheiten mit #\| geklammert
		   hintereinander ("|~{~A~^ ~}|").
		   Beispiele:
		     (:DIM-NUMBER 17.346 |cm|)
                        mit PRECISION = 2 ->  17.35cm
		     (:DIM-NUMBER 12.2 |m|)  ->  12.2m
                     (:DIM-NUMBER 12 |m| |h|)  ->  12|m h|

		   BEST-FIT -- bestimmt zur Groesse von DIM die "optimale
		   Einheit", dabei werden UNITS ggf. ausschliesslich verwendet.
		   Gemaess der "Stelligkeit" von DIM's Wert (Log10) wird aus
		   dem Scale-Set der Einheit von DIM die geeignete Einheit
		   ausgewaehlt. Sind UNITS angegeben, werden nur die dazu
		   passenden Eintraege aus dem Scale-Set verwendet.
		   Es wird derjenige Eintrag aus dem Scale-Set gewaehlt, zu
		   dessen Stelligkeit DIM's Stelligkeit den geringsten Abstand
		   hat (absolut). DIM wird zu der entsprechenden Einheit
		   konvertiert und geliefert.

		   STEPWISE-DIM-LIST -- generiert rekursiv eine Liste von dim.
		   Zahlen mit ganzzahligen Werten, Einheit entsprechend
		   UNIT-LIST. Dazu wird DIM auf die erste Einheit in UNIT-LIST
		   umgewandelt. Sind noch Einheiten in UNIT-LIST uebrig, wird
		   der Wert von DIM in den ganzzahligen und den Nachkomma-Teil
		   gespalten. Ist der ganzzahlige Teil 0 und SKIP-ZEROS T,
		   wird rekursiv mit dem Rest der UNIT-LIST fortgefahren.
 		   Andernfalls wird rekursiv mit dem Nachkomma-Teil und dem
		   Rest der UNIT-LIST fortgefahren und eine neue dim. Zahl mit
		   dem ganzzahligen Wert und der Unit gebildet und dem Rest
		   vorangestellt. Wurde kein Rest erzeugt und ist die Zahl 0
		   wird NIL geliefert.
		   Sind keine Einheiten mehr vorhanden, bleibt DIM's Wert
		   unveraendert. Bei *STEP-SKIP-TRAILING-ZEROS* und 0 als Wert
		   wird die Zahl komplett ignoriert. Ist jedoch
		   SKIP-ZEROS T (am Anfang der Liste), bleibt 0 erhalten.
|#

(defun print-dim-number (dim &optional (stream *standard-output*) level
			     &aux ms)
  (declare (ignore level))
  (cond ((not *print-formatted-dim*)
	 (format stream "#<Dim-Number ~A ~S>"
		 (%dim-value dim) (%dim-prim-id dim)))
        ((setq ms (dim-measure dim))
	 (let ((mode (or (and (boundp '*tmp-unit-output-format*)
			      *tmp-unit-output-format*)
			 (ms-output-format ms))))
	   (if (symbolp mode) (setq mode (list mode)))
	   (ecase (car mode)
	     ((:current nil)
	      (if (and (xdim-number-p dim) (%dim-pref-unit dim))
		  (print-formatted-dim
                   (dim-value dim (%dim-pref-unit dim))
		   (%dim-pref-unit dim)
		   (second mode) stream)
		  (print-formatted-dim
		   (%dim-value dim) 
		   (ms-base-unit ms)
		   (second mode) stream)))
	     (:base
	      (print-formatted-dim
	       (%dim-value dim) (ms-base-unit ms) (second mode) stream))
	     (:unit (print-formatted-dim
		     (dim-value dim (second mode))
		     (second mode)
		     (third mode)
		     stream))
	     (:best-fit
	      (multiple-value-bind (best-val best-unit)
				   (best-fit dim ms (butlast (cdr mode)))
		(print-formatted-dim best-val best-unit
				     (car (last mode)) stream)))
	     (:step
	      (let ((*print-escape* nil))
		(format stream "~{~A~^:~}"
			(mapcar #'(lambda (dim-and-unit-and-prec)
				    (apply #'print-formatted-dim dim-and-unit-and-prec))
				(stepwise-dim-list
				 dim
				 (scale-per-units
				  (ms-scale ms) (butlast (cdr mode)))
				 (car (last mode))))))))))
        ((and (boundp '*tmp-unit-output-format*)
	      (consp *tmp-unit-output-format*) ; tar
              (eq (first *tmp-unit-output-format*) :unit)
	      (eql (%dim-prim-id dim)
		   (%dim-prim-id (unit-to-dim-number (second *tmp-unit-output-format*)))))
	 (print-formatted-dim
	  (dim-value dim (second *tmp-unit-output-format*))
	  (second *tmp-unit-output-format*)
	  (third *tmp-unit-output-format*)
	  stream))
	((and (xdim-number-p dim) (%dim-pref-unit dim))
	 (print-formatted-dim
	  (dim-value dim (%dim-pref-unit dim))
	  (%dim-pref-unit dim)
	  nil stream))
	(t (print-formatted-dim
	    (%dim-value dim) (prim-id-to-unit (%dim-prim-id dim))
	    nil stream))))

(defun print-formatted-dim (val unit &optional precision stream)
  (cond ((null precision)
         (format stream "~:[~;#M~]~:[~F~;~D~]~A"
                 (and *print-escape* (eq *read-measures* ':dispatched))
                 (integerp val) val unit))
        ((zerop precision)
         (format stream "~:[~;#M~]~D~A"
                 (and *print-escape* (eq *read-measures* ':dispatched))
                 (round val) unit))
        (t
         (format stream "~:[~;#M~]~,VF~A"
                 (and *print-escape* (eq *read-measures* ':dispatched))
                 precision val unit))))

(defun scale-per-units (scale units)
  (flet ((scale-member-inv (set scale)
	    (member (car scale) set :test #'string=)))
    (cond ((null units) scale)
	  ((remove units scale :test-not #'scale-member-inv))
	  (t (error "Illegal units in format specification: ~{~A~^, ~}."
		    units)))))
	
(defun best-fit (dim ms &optional units)
  (flet ((log-diff (val scale)
                   (abs (- val (log (cdr scale) 10)))))
    (if (zerop (%dim-value dim))
	(values 0 (ms-base-unit ms))
	(let* ((log10 (log (abs (%dim-value dim)) 10))
	       (best-unit
		(car
		 (reduce #'(lambda (sc1 sc2)
			     (if (<= (log-diff log10 sc1)
				     (log-diff log10 sc2))
				 sc1 sc2))
			 (scale-per-units (ms-scale ms) units)))))
	  (values (dim-value dim best-unit) best-unit)))))

(defun stepwise-dim-list (dim scale-list
			  &optional (precision nil) (skip-zeros *step-skip-leading-zeros*))
  (let ((step-val (/ (%dim-value dim) (cdar scale-list))))
    (cond ((cdr scale-list)
	   (multiple-value-bind (int frac) (truncate step-val)
	     (if (and (zerop int) skip-zeros)
		 (stepwise-dim-list dim (cdr scale-list) precision t)
		 (let ((rest (stepwise-dim-list
			      (make-dim-number :value (* frac (cdar scale-list))
					       :pid (%dim-prim-id dim))
			      (cdr scale-list) precision nil)))
		   (if (and (zerop int) (null rest))
		       nil
		       (cons (list int (caar scale-list) (if rest nil precision)) rest))))))
	  ((and *step-skip-trailing-zeros*
		(not skip-zeros) (zerop step-val)) nil)
	  (t (list (list step-val (caar scale-list) precision))))))


(defmethod make-load-form ((dn dim-number) &optional environment)
  (declare (ignore environment))
  `(unit-to-dim-number ,(prim-id-to-unit (%dim-prim-id dn))
                       :value ,(%dim-value dn)))


(defmethod make-load-form ((dn xdim-number) &optional environment)
  (declare (ignore environment))
  `(unit-to-dim-number ',(%dim-pref-unit dn)
                       :value ,(dim-value dn (%dim-pref-unit dn))))


