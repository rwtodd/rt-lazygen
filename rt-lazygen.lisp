;;;; rt-lazygen.lisp

;; TODO: let lmap lfilter etc operate on multiple GENs at once, like mapcar etc do.

(in-package #:rt-lazygen)

(defun lempty-gen ()
  "endlessly return the end-token"
  'end-token)

(defun llist (list)
  "turn LIST into a generator"
  (lambda ()
    (if list
	(pop list)
	'end-token)))

(defun lmap (fn gen)
  "create a lazy map applying FN to GEN"
  (lambda ()
    (let ((next (funcall gen)))
      (if (eq next 'end-token)
	  next
	  (funcall fn next)))))

(defun lfilter (fn gen)
  "create a lazy filter testing GEN with FN"
  (lambda ()
    (do ((next (funcall gen) (funcall gen)))
	((or (eq next 'end-token)
	     (funcall fn next)) next))))
  
(defun for-each (fn gen)
  "apply FN to all elements of GEN for side-effects"
  (do ((elt (funcall gen) (funcall gen)))
      ((eq elt 'end-token) nil)
    (funcall fn elt)))

(defun to-list (gen)
  "convert GEN into a list by evaluating all the elements"
  (do ((answer nil (cons elt answer))
       (elt (funcall gen) (funcall gen)))
      ((eq elt 'end-token) (nreverse answer))))

(defun ltake (n gen)
  "lazily take only the first N elements of GEN"
  (lambda ()
    (if (= n 0)
	'end-token
	(progn
	  (decf n)
	  (funcall gen)))))

(defun ldrop (n gen)
  "lazily drop the first N elements of GEN"
  (lambda ()
    (when n
      (dotimes (_ n) (funcall gen))
      (setf n 0))
    (funcall gen)))

(defun ltake-while (fn gen)
  "lazily supply elements of GEN while FN returns true on the elements"
  (lambda ()
    (let ((next (funcall gen)))
      (if (or (eq next 'end-token)
	      (funcall fn next))
	  next
	  (progn
	    (setf gen #'lempty-gen)
	    'end-token)))))

(defun ldrop-while (fn gen)
  "lazily drop elements of GEN while FN returns true on the elements"
  (lambda ()
    (if fn
	(do ((next (funcall gen) (funcall gen)))
	    ((or (eq next 'end-token)
		 (not (funcall fn next))) (progn (setf fn nil) next)))
	(funcall gen))))

(defun lrange (&optional (start 0) (end nil) (step 1))
  "lazily generate a range of numbers from START to END by STEP"
  (lambda ()
    (if (or (null end)
	    (< start end))
	(let ((answer start))
	  (setf start (+ start step))
	  answer)
	'end-token)))

(defun literate (fn init)
  "defines a generator: INIT, (FN INIT), (FN (FN INIT)), etc.."
  (lambda ()
    (let ((answer init))
      (setf init (funcall fn init))
      answer)))

(defmacro --> (&body clauses)
  "define a pipeline for data in a lazy generated sequence"
  (reduce #'(lambda (c1 c2) (append c2 (list c1))) clauses))
