;;; -*- Package: CL-USER; Syntax: COMMON-LISP -*-

(in-package :cl-user)

#+(not (or :asdf :genera :allegro :lispworks))
(require "ASDF")

#+(not (or :asdf :genera :allegro :lispworks))
(error "No system declaration for ~A" (lisp-implementation-type))

#+(or OPENMCL-UNICODE-STRINGS SB-UNICODE UNICODE)
(pushnew 'ms-unicode *features*)

;;; Logical Pathnames

(setf (logical-pathname-translations "MEASURES")
      `(("MEASURES:**;*.*.*"
         ,(namestring
           (make-pathname
            :type :wild
            :name :wild
            :directory (append (pathname-directory *load-truename*)
                              '(:wild-inferiors)))))))

#+:allegro
(defsystem :measures
    (:default-pathname "measures:source;")
  (:serial "package"
           "measures" 
           "reader-setup"))

#+:LispWorks
(defsystem :measures
  (:default-pathname "MEASURES:SOURCE;"
   :object-pathname  "MEASURES:BIN;"
   :default-type :lisp-file
   :optimize ((speed 3)
              (space 2) 
              (safety 3)
              (debug 2)))
  :members ("package" 
            "measures"
            "measure-enhancements"
            "reader-setup"
            "unit-defs")
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))

;;;; Symbolics Common Lisp

#+:Genera
(scl:defsystem :measures
    (:default-pathname             "measures:source;"
     :default-destination-pathname "measures:bin;")
  (:serial
    "package"
    "measures"
    "symbolics-patch-1"
    "symbolics-patch-2"
    "reader-setup"))

#+(and asdf (not (or lispworks allegro genera)))
(asdf:defsystem "Measures"
  :description "Dimensions and units for Common Lisp"
  :author "Roman Cunis"
  :serial t
  :pathname #p"MEASURES:source;*.*.*"
  :components ((:file "package")
               (:file "measures")
               (:file "measure-enhancements")
               (:file "reader-setup")
               (:file "unit-defs")))


