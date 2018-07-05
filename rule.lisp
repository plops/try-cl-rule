;; http://www.newresalhaider.com/post/common-treasure/
;; martin kielhorn 2018-07-05
(ql:quickload :lisa)
(ql:quickload :defclass-std)
(defpackage :g
  (:use :cl :defclass-std :lisa)
  (:shadowing-import-from :lisa :assert))
(in-package :g)

(class/std permit-required)
(class/std area archeological-type size-disturbed)

(defrule high-value ()
  (area (archeological-type :high-value)) =>
  (assert ((make-instance 'permit-required))))

(defrule high-expectation ()
  (area (archeological-type :high-expectation)
	(< 100 size-disturbed)) =>
	(assert ((make-instance 'permit-required))))

(defrule expectation ()
  (area (archeological-type :expectation)
	(< 1000 size-disturbed)) =>
	(assert ((make-instance 'permit-required))))

#+nil
(progn
  (reset)
  #+nil
  (assert ((make-instance 'area :archeological-type :high-value
			  :size-disturbed 2000)))
  (assert ((make-instance 'area :archeological-type :high-expectation :size-disturbed 10)))

 
 (facts)
 (run)
 (facts))
