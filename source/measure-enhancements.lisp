;;;-*- Mode: Lisp; Package: MEASURES -*-

(in-package "MEASURES")

(defclass special-measure (measure) nil)
(defun special-measure-p (ms) (typep ms 'special-measure))

(defun convert-dim (dn unit)
  (let ((measure (and (unit-p unit)
		      (if (symbolp unit)
			  (measure-of unit)
			  (measure-of (find-symbol unit *ms-package*))))))
    (if (special-measure-p measure)
      (if (eql (%dim-prim-id dn) (ms-prim-id measure))
        (convert-special-dim dn unit measure nil)
        (error "Incompatible units: ~A, ~A."
               (prim-id-to-unit (dim-prim-id dn)) unit))
      (let ((unit-dn (unit-to-dim-number unit)))
        (if (eql (%dim-prim-id dn) (%dim-prim-id unit-dn))
          (/ (%dim-value dn) (%dim-value unit-dn))
          (error "Incompatible units: ~A, ~A."
                 (prim-id-to-unit (dim-prim-id dn)) unit)) )) ))

(defun convert-special-dim (dn unit measure invertP)
  (unless (symbolp unit)
    (setq unit (find-symbol unit *ms-package*)))
  (cond ((eq unit (ms-base-unit measure))
         (%dim-value dn))
        (invertP
         (funcall (second (assoc unit (ms-scale measure))) (%dim-value dn)))
        (t
         (funcall (third (assoc unit (ms-scale measure))) (%dim-value dn)))))



(defun dim-value (dn &optional unit)
" DIM-VALUE dim-number [unit]

  Returns the value of dim-number as with the unit given.
  Unit defaults to the base unit of the dim-number's measure.
"
  (cond (unit
	 (check-type unit (or string symbol))
	 (etypecase dn
	   (DIM-NUMBER (convert-dim dn unit))
	   (NUMBER (convert-dim (make-dim-number :value dn :unit "") unit))))
	(t (dim-value0 dn))))

(defun unit-to-dim-number (unit &key (value 1) (error-p t))
  (unless (and (or (stringp unit) (symbolp unit))
	       (or (eql (length (string unit)) 0)
		   (unit-char-p (char (string unit) 0) t)))
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
		  ((eq slash-found -1)
		   (error "Invalid unit syntax: ~A." us))
		  (t (setq slash-found -1) (incf from))))
	  (let ((end-of-unit
		 (or (position-if-not #'constituent-unit-char-p us :start from)       ; tar
		     (length us))))
	    (if (zerop end-of-unit)
		(error "Invalid unit syntax: ~A." us)
		(let* ((us1 (subseq us from end-of-unit))
		       (unit1 (find-symbol us1
					; (symbol-package unit) ; <-- rm
					   *ms-package*)) ; tar
		       (measure1 (measure-of unit1)))
		  (if measure1
                    (if (special-measure-p measure1)
                      (if error-p
                        (error "Unit ~A is a special unit that cannot be combined with others: ~A"
				 us1 us)
                        (return-from unit-to-dim-number nil))
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
				    nil)))))
		      (if error-p
			  (error "Undefined unit ~A in unit specification: ~A."
				 us1 us)
                          (return-from unit-to-dim-number nil))))))))
      (let* ((unit0 (find-symbol us *ms-package*))  ; tar
	     (measure0 (measure-of unit0)))
        (cond ((null measure0) 
               (apply #'dim* value (unit-to-dn-1 0)))
              ((special-measure-p measure0)
               (%make-xdim-number
	        :value (funcall (second (assoc unit0 (ms-scale measure0))) value)
	        :prim-id (ms-prim-id measure0)
	        :pref-unit unit0))
              (t
	       (%make-xdim-number
	        :value (rationalize
		        (* value (cdr (assoc unit0 (ms-scale measure0)))))
	        :prim-id (ms-prim-id measure0)
	        :pref-unit unit0)))))))



(defun make-special-measure (mname &key base-unit prim-id format regular)
  (let ((ms (make-instance 'special-measure
			   :name mname
			   :base-unit base-unit
			   :prim-id prim-id
			   :output-format format
			   :regular regular)))
    (associate-unit-with-measure base-unit ms (list #'identity #'identity))
    (set-prim-id-to-measure prim-id ms)
    ms))

(defun define-special-base-unit-fcn (unit measure-name &optional primitives)
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
      (values unit (make-special-measure measure-name
					 :base-unit unit
					 :prim-id pid
					 :regular (and unit-dn t))))))

(defun associate-unit-with-special-measure (unit measure scale)
  (setf (ms-scale measure) (nconc (ms-scale measure)
				  (list (cons unit scale))))
  (setf (measure-of unit) measure))


(defun define-special-unit-fcn (base-unit unit to-base-fn from-base-fn)
  (if (stringp base-unit) (setq base-unit (intern base-unit *ms-package*)))
  (if (stringp unit) (setq unit (intern unit *ms-package*)))
  (if (measure-of unit) (delete-unit unit))
  (let ((unit-dn (unit-to-dim-number base-unit :error-p t)))
    (values unit
	    (associate-unit-with-special-measure
	     unit
	     (dim-measure unit-dn)
	     (list to-base-fn from-base-fn)))))

(defmacro define-special-unit (base-unit unit to-base-fn from-base-fn)
"   DEFINE-SPECIAL-UNIT  baseUnit unit to-base-fn from-base-fn

    Defines UNIT as a new unit derived from base-unit through the use of
    conversion functions.  A conversion function into the baseUnit and
    one from the base-unit to unit must be specified.
"
    `(define-special-unit-fcn ,base-unit ,unit ',to-base-fn ',from-base-fn))

(defmacro defspecial-measure (measure-name base-expr &key units format)
  (let ((base-unit (if (typep base-expr '(or string symbol))
		       base-expr
		       (car base-expr)))
	(defined-as (if (consp base-expr) (cdr base-expr) nil))
	(metric (if (eq units ':metric) ':metric
		    (and (consp units) (find ':metric units))))
	(scientific (if (eq units ':scientific) ':scientific
			(and (consp units) (find ':scientific units)))))
    (if (or metric scientific)
	(error "Special measures do not support :metric or :scientific."))
    `(progn
      (define-special-base-unit-fcn ',base-unit ',measure-name ',defined-as)
      ,@(if (consp units)
	    (mapcar #'(lambda (u)
			`(define-special-unit 
			     ,(if (stringp base-unit) base-unit `',base-unit)
			     ,@(if (consp u) u (list u))))
		    (remove-if #'keywordp units)))
      ,(if format
	   `(set-unit-format (measure-named ',measure-name)
			     ',(if (symbolp format) (list format) format)))
      (measure-named ',measure-name))))

;;; SCALAR DEFINITIONS

(defun safe-apply-additive-operator
       (operator args &optional (args-no-0 (member 0 args :test-not #'eql)))
  (cond ((null args-no-0) 0)
	(;(numberp (car args-no-0))
	 (every #'numberp args-no-0)  ; Use this to support scalar units?
	 (apply operator args))
	(t (make-dim-number :value (apply operator (mapcar #'dim-value0 args))
			    :pid (dim-prim-id (car args-no-0))))))



(defun define-scalar-measure-fcn ()
  (if (measure-of 'scalar) (delete-measure (measure-of 'scalar)))
  (make-measure 'scalar
		:base-unit '||
		:prim-id 1
		:regular nil))


(defmacro defscalars (&key units format)
  (let ((base-unit '||))
    (declare (ignore base-unit))
    (when (or (eq units ':metric)
	      (eq units ':scientific)
	      (and (consp units)
		   (find ':metric units)
		   (find ':scientific units)))
      (error ":Metric or :scientific units don't make sense for scalars."))
    `(progn
       (define-scalar-measure-fcn)
       ,@(if (consp units)
	     (mapcar #'(lambda (u) `(define-unit ,@(if (consp u) u (list u))))
		     units))
      ,(if format
	   `(set-unit-format 'scalar
			     ',(if (symbolp format) (list format) format)))
      (measure-named 'scalar))))


#|
;;;
;;; Special Measures
;;;

(defun f-to-c (x) (* 5/9 (- x 32)))
(defun c-to-f (x) (+ 32 (* 9/5 x)))
(defun k-to-c (x) (- x 27316/100))
(defun c-to-k (x) (+ x 27316/100))


(defspecial-measure temperature "oC"
  :units (("oK" k-to-c c-to-k)
	  ("oF" f-to-c c-to-f)
	  ("C" identity identity)
	  ("F" f-to-c c-to-f)
	  ("K" k-to-c c-to-k)))

;;;
;;; Scalar Measures
;;;

(declare-unit-char #\%)
(defscalars :units (("%" 1/100) ("%%" 1/1000)))

|#

;;
;; Alternate best-fit
;;

(defparameter *best-fit-leading-digit-bias* 3
  "A parameter that biases the choice of the :best-fit option.  Positive
numbers prefer more digits ahead of a decimal point.  Negative numbers more
digits behind the decimal.")

(defun best-fit (dim ms &optional units)
  (flet ((log-diff (val scale)
	   ;; Heuristic Log differential that prefers numbers in front
	   ;;  of the decimal point.
	   (let ((diff (- val (log (cdr scale) 10))))
	     (if (minusp diff)
		 (+ (- diff) *best-fit-leading-digit-bias*)
	         diff))))
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

