;; http://www.newresalhaider.com/post/common-treasure/
;; martin kielhorn 2018-07-05
(ql:quickload :lisa)
(ql:quickload :defclass-std)
(defpackage :g
  (:use :cl :defclass-std))
(in-package :g)

(class/std permit-required)
(class/std area archeological-type size-disturbed)

