(defpackage #:c4.5
  (:use "COMMON-LISP")
  (:export "*TEST-FUNCTION*"
           "BUILD-TREE"
           "C4.5"
           "CLASSIFY"))

(in-package :c4.5)

(defparameter *test-function* #'equalp "Used for doing all value comparisons")

(defvar *debug* 0)
