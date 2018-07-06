;; http://lisa.sourceforge.net/mab-clos.lisp
;; https://github.com/johanlindberg/lisa/blob/master/misc/mab-clos.lisp
;; quicklisp/dists/quicklisp/software/lisa-20120407-git/misc/mab-clos.lisp
;; https://github.com/briangu/OPS5/blob/master/demo/ops-demo-mab.lisp
;; https://en.wikipedia.org/wiki/OPS5

;; OPS5 uses a forward chaining inference engine; programs execute by
;; scanning "working memory elements" (which are vaguely object-like,
;; with classes and attributes) looking for matches with the rules in
;; "production memory". Rules have actions that may modify or remove
;; the matched element, create new ones, perform side effects such as
;; output, and so forth. Execution continues until no more matches can
;; be found.

;; how does lisa compare to ops5?
;; https://github.com/johanlindberg/benchmarks


;; http://rplaca.pulp.se/ However, the only, currently available, open
;; source Common Lisp Production System (LISA) is not only abandoned
;; but also shows quite poor performance.
;; https://github.com/johanlindberg/minimal-production-system
;; martin kielhorn 2018-07-05
(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload :lisa)
  (ql:quickload :defclass-std))
(defpackage :g
  (:use :cl :defclass-std :lisa)
  (:shadowing-import-from :lisa :assert))
(in-package :g)

(lisa:consider-taxonomy)

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

;;; unlocking chest

(defrule hold-chest-to-put-on-floor ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (on-top-of (not floor)) (weight light))
  (monkey (holding (not ?chest)))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 ?chest))))

(defrule put-chest-on-floor ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding ?chest)))
  (?thing (thing (name ?chest)))
  =>
  (format t "monkey throws the ~a off the ~a onto the floor.~%" ?chest ?on)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor)))

(defrule get-key-to-unlock ()
  (goal-is-to (action unlock) (argument-1 ?obj))
  (thing (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding (not ?key)))
  (not (goal-is-to (action hold) (argument-1 ?key)))
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 ?key))))

(defrule move-to-chest-with-key ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (location ?place) (on-top-of floor))
  (monkey (location (not ?cplace)) (holding ?key))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (argument-1 ?cplace)))
  =>
  (assert ((make-instance 'goal-is-to :action 'walk-to :argument-1 ?cplace))))

(defrule unlock-chest-with-key ()
  (?goal (goal-is-to (action unlock) (argument-1 ?name)))
  (?chest (chest (name ?name) (contents ?contents) (unlocked-by ?key)))
  (thing (name ?name) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?on) (holding ?key))
  =>
  (format t "monkey opens the ~a ith the ~a revealing a ~a.~%"
	  ?name ?key ?contents)
  (modify ?chest (contents nothing))
  (assert ((make-instance 'thing :name ?contents :location ?place
			  :weight 'light :on-top-of ?name)))
  (retract ?goal))

;;; hold objects

(defrule unlock-chest-to-hold-object ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert ((make-instance 'goal-is-to :action 'unlock :argument-1 ?chest))))

(defrule use-ladder-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (not (thing (name ladder) (location ?place)))
  (not (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place)))
  =>
  (assert ((make-instance 'goal-is-to :action 'move :argument-1 'ladder
			  :argument-2 ?place))))

(defrule climb-ladder-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (thing (name ladder) (location ?place) (on-top-of floor))
  (monkey (on-top-of (not ladder)))
  (not (goal-is-to (action on) (argument-1 ladder)))
  =>
  (assert ((make-instance 'goal-is-to :action 'on :argument-1 'ladder))))


(defrule grab-object-from-ladder ()
  (?goal (goal-is-to (action hold))))
