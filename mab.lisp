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
  (thing (name ?chest) (location ?cplace) (on-top-of floor))
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
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place)
		 (on-top-of ceiling) (weight light)))
  (thing (name ladder) (location ?place))
  (?monkey (monkey (location ?place) (on-top-of ladder) (holding blank)))
  =>
  (format t "monkey grabs the ~a.~%" ?name)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule climb-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place (not ceiling))
	 (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of (not ?on)))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert ((make-instance 'goal-is-to :action 'on
			  :argument-1 ?on))))
(defrule walk-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place)
	 (on-top-of (not ceiling)) (weight light))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert ((make-instance 'goal-is-to :action 'walk-to
			  :argument-1 ?place))))

(defrule drop-to-hold ()
  (?goal (goal-is-to (action hold) (argument-1 ?obj)))
  (thing (name ?obj) (location ?place) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ?on) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))  
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 'blank))))

(defrule grab-object ()
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place) (on-top-of ?on) (weight light)))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding blank)))
  =>
  (format t "monkey grabs the ~a.~%" ?name)
  (modify ?thing  (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule drop-object ()
  (?goal (goal-is-to (action hold) (argument-1 blank)))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding ?name (not blank))))
  (?thing (thing (name ?name))) 
  =>
  (format t "monkey drops the ~a.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing  (location ?place) (on-top-of ?on))
  (retract ?goal))


;; move object

(defrule unlock-chest-to-move-object ()
  (goal-is-to (action move) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert ((make-instance 'goal-is-to :action 'unlock :argument-1 ?chest))))

(defrule hold-object-to-move ()
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (thing (name ?obj) (location (not ?place)) (weight light))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 ?obj))))

(defrule move-object-to-place ()
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (monkey (location (not ?place)) (holding ?obj))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert ((make-instance 'goal-is-to :action 'walk-to :argument-1 ?place))))

(defrule drop-object-once-moved ()
  (?goal (goal-is-to (action move) (argument-1 ?name) (argument-2 ?place)))
  (?monkey (monkey (location ?place) (holding ?obj)))
  (?thing (thing (name ?name) (weight light)))
  =>
  (format t "monkey drops the ~a.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor))
  (retract ?goal))

(defrule already-moved-object ()
  (?goal (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place)))
  (thing (name ?obj) (location ?place))
  =>
  (retract ?goal))

;; walk to place

(defrule already-at-place ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (monkey (location ?place))
  =>
  (retract ?goal))

(defrule get-on-floor-to-walk ()
  (goal-is-to (action walk-to) (argument-1 ?place))
  (monkey (location (not ?place)) (on-top-of (not floor)))
  (not (goal-is-to (action on) (argument-1 floor)))
  =>
  (assert ((make-instance 'goal-is-to :action 'on :argument-1 'floor))))


(defrule walk-holding-nothing ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding blank)))
  =>
  (format t "monkey walks to ~a.~%" ?place)
  (modify ?monkey (location ?place))
  (retract ?goal))

(defrule walk-holding-object ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding ?obj)))
  (thing (name ?obj))
  =>
  (format t "monkey walks to ~a holding the ~a.~%" ?place ?obj)
  (modify ?monkey (location ?place))
  (retract ?goal))

;; get on object

(defrule jump-onto-floor ()
  (?goal (goal-is-to (action on) (argument-1 floor)))
  (?monkey (monkey (on-top-of ?on (not floor))))
  =>
  (format t "monkey jumps off the ~a onto the floor.~%" ?on)
  (modify ?monkey (on-top-of floor))
  (retract ?goal))

(defrule walk-to-place-to-climb ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert ((make-instance 'goal-is-to :action 'walk-to :argument-1 ?place))))

(defrule drop-to-climb ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location ?place) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 'blank))))

(defrule climb-indirectly ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?top
				       (and (not (eq ?top ?on))
					    (not (eq ?top ?obj))))
	  (holding blank))
  =>
  (assert ((make-instance 'goal-is-to :action 'on :argument-1 ?on))))

(defrule climb-directly ()
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (?monkey (monkey (location ?place) (on-top-of ?on)
	   (holding blank)))
  =>
  (format t "monkey climbs onto the ~a.~%" ?obj)
  (modify ?monkey (on-top-of ?obj))
  (retract ?goal))

(defrule already-on-object ()
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (monkey (on-top-of ?obj))
  =>
  (retract ?goal))

;; eat object

(defrule hold-to-eat ()
  (goal-is-to (action eat) (argument-1 ?obj))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (assert ((make-instance 'goal-is-to :action 'hold :argument-1 ?obj))))

(defrule satisfy-hunger ()
  (?goal (goal-is-to (action eat) (argument-1 ?name)))
  (?monkey (monkey (holding ?name)))
  (?thing (thing (name ?name)))
  =>
  (format t "monkey eats the ~a.~%" ?name)
  (modify ?monkey (holding blank) (satisfied t))
  (retract ?goal)
  (retract ?thing))

(defrule monkey-is-satisfied ()
  (monkey (satisfied t) (:object ?monkey))
  =>
  (format t "monkey is satisfied: ~s~%" ?monkey))

;; retract all objects who are instance of mab-fundamental

(defrule cleanup (:salience  -100)
  (?fact (mab-fundamental))
  =>
  (retract ?fact))

;; startup

(defrule startup ()
  =>
  (assert ((make-instance 'monkey :location 't5-7
			  :on-top-of 'green-couch
			  :location 'green-couch
			  :holding 'blank)))
  (assert ((make-instance 'thing :name 'green-couch
			  :location 't5-7
			  :on-top-of 'floor
			  :weight 'heavy)))
  (assert ((make-instance 'thing :name 'red-couch
			  :location 't2-2
			  :on-top-of 'floor
			  :weight 'heavy)))
  (assert ((make-instance 'thing :name 'big-pillow
			  :location 't2-2
			  :on-top-of 'red-couch
			  :weight 'light)))
  (assert ((make-instance 'thing :name 'red-chest
			  :location 't2-2
			  :on-top-of 'big-pillow
			  :weight 'light)))
  (assert ((make-instance 'chest :name 'red-chest
			  :contents 'ladder
			  :unlocked-by 'red-key)))
  (assert ((make-instance 'thing :name 'blue-chest
			  :location 't7-7
			  :on-top-of 'ceiling
			  :weight 'light)))
  (assert ((make-instance 'chest :name 'blue-chest
			  :contents 'bananas
			  :unlocked-by 'blue-key)))
  (assert ((make-instance 'thing :name 'grapes
			  :location 't7-8
			  :on-top-of 'ceiling
			  :weight 'light)))
  (assert ((make-instance 'thing :name 'blue-couch
			  :location 't8-8
			  :on-top-of 'floor
			  :weight 'heavy)))
  (assert ((make-instance 'thing :name 'green-chest
			  :location 't8-8
			  :on-top-of 'ceiling
			  :weight 'light)))
  (assert ((make-instance 'chest :name 'green-chest
			  :contents 'blue-key
			  :unlocked-by 'red-key)))
  (assert ((make-instance 'thing :name 'red-key
			  :location 't1-3
			  :weight 'light
			  :on-top-of 'floor)))
  (assert ((make-instance 'goal-is-to :action 'eat
			  :argument-1 'bananas))))

(defun run-mab (&optional (ntimes 1))
  (flet ((repeat-mab ()
	   (dotimes (i ntimes)
	     (format t "starting run:~%")
	     (reset)
	     (run))))
    (time (repeat-mab))))

#+nil
(run-mab)
