;;;-*- Package: MEASURES; Syntax: Common-Lisp -*-

(in-package :measures)

;;;; ************************ Initialization ******************************

(ecase *read-measures*                 ; default is :dispatched
       (:on-demand (install-dim-number-reader))
       (:dispatched (install-dim-number-reader :permanent nil :dispatch #\M))
       (:integrated (install-dim-number-reader :permanent t)))

