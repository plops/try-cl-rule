;; http://lisa.sourceforge.net/mab-clos.lisp
;; martin kielhorn 2018-07-05

(ql:quickload :lisa)
(ql:quickload :defclass-std)
(defpackage :g
  (:use :cl :defclass-std :lisa)
  (:shadowing-import-from :lisa :assert))
(in-package :g)
