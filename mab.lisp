;; http://lisa.sourceforge.net/mab-clos.lisp
;; martin kielhorn 2018-07-05

(ql:quickload :lisa)
(ql:quickload :defclass-std)
(defpackage :g
  (:use :cl :defclass-std :lisa)
  (:shadowing-import-from :lisa :assert))
(in-package :g)

(class/std mab-fundamental)
(defclass/std monkey ()
  ((location :std 'green-couch)
   (on-top-of :std 'floor)
   (satisfied :std nil)
   (holding :std 'nothing)))
(defclass/std thing (mab-fundamental)
  ((name)
   (location)
   (on-top-of :std 'floor)
   (weight :std 'light)))
(defclass/std chest (mab-fundamental)
  ((name)
   (contents)
   (unlocked-by)))
(defclass/std goal-is-to (mab-fundamental)
  ((action)
   (argument-1)
   (argument-2 :std nil)))

(defimport mab-fundamental (g::mab-fundamental) ())
(defimport monkey (g::monkey (mab-fundamental)) ())
(defimport thing (g::thing (mab-fundamental)) ())
(defimport chest (g::chest (mab-fundamental)) ())
(defimport goal-is-to (g::goal-is-to (mab-fundamental)) (action argument-1 argument-2))
