;;;; rt-lazygen.lisp

;; TODO: let lmap lfilter etc operate on multiple GENs at once, like mapcar etc do.

(in-package "RT-LAZYGEN")

(defun empty-gen ()
  "endlessly return the end-token"
  'end-token)

(defun list->gen (list)
  "turn LIST into a generator"
  (lambda ()
    (if list
	(pop list)
	'end-token)))

(defun gen->list (gen)
  "convert GEN into a list by evaluating all the elements"
  (do ((answer nil (cons elt answer))
       (elt (funcall gen) (funcall gen)))
      ((eq elt 'end-token) (nreverse answer))))

(defun list->cycle (list)
  "turn LIST into an infinite repeated cycle"
  (let ((cur list))
    (lambda ()
      (when (null cur)
	(setq cur list))
      (pop cur))))
 
(defun map (fn gen)
  "create a lazy map applying FN to GEN"
  (lambda ()
    (let ((next (funcall gen)))
      (if (eq next 'end-token)
	  next
	  (funcall fn next)))))

(defun filter (fn gen)
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

(defun take (n gen)
  "lazily take only the first N elements of GEN"
  (lambda ()
    (if (= n 0)
	'end-token
	(progn
	  (decf n)
	  (funcall gen)))))

(defun first (gen)
  "eagerly return the first element of GEN, or NIL"
  (let ((val (funcall gen)))
    (if (eq val 'end-token)
	nil
	val)))

(defun last (gen)
  "eagerly return the last element of GEN, or NIL. 
   Don't call on infinite generators!"
  (do ((elt nil next)
       (next (funcall gen) (funcall gen)))
      ((eq next 'end-token) elt)))

(defun drop (n gen)
  "lazily drop the first N elements of GEN"
  (lambda ()
    (when n
      (dotimes (_ n) (funcall gen))
      (setf n 0))
    (funcall gen)))

(defun take-while (fn gen)
  "lazily supply elements of GEN while FN returns true on the elements"
  (lambda ()
    (let ((next (funcall gen)))
      (if (or (eq next 'end-token)
	      (funcall fn next))
	  next
	  (progn
	    (setf gen #'lg-empty-gen)
	    'end-token)))))

(defun drop-while (fn gen)
  "lazily drop elements of GEN while FN returns true on the elements"
  (lambda ()
    (if fn
	(do ((next (funcall gen) (funcall gen)))
	    ((or (eq next 'end-token)
		 (not (funcall fn next))) (progn (setf fn nil) next)))
	(funcall gen))))

(defun range (&optional (start 0) (end nil) (step 1))
  "lazily generate a range of numbers from START to END by STEP"
  (lambda ()
    (if (or (null end)
	    (< start end))
	(let ((answer start))
	  (setf start (+ start step))
	  answer)
	'end-token)))

(defun iterate (fn init)
  "defines a generator: INIT, (FN INIT), (FN (FN INIT)), etc.."
  (lambda ()
    (let ((answer init))
      (setf init (funcall fn init))
      answer)))

(defun fold (fn init gen)
  "eagerly fold over a lazy generator with (fn (fn init elt1) elt2)..."
  (do ((elt (funcall gen) (funcall gen)))
      ((eq elt 'end-token) init)
    (setq init (funcall fn init elt))))

(defun scan (fn init gen)
  "a fold with all intermediate results returned"
  (lambda ()
    (let ((answer init) 
          (val (funcall gen)))
       (setq init (if (eq val 'end-token) val (funcall fn init val)))
       answer)))

(defun every (fn gen)
  "eagerly determine if FN returns true for every element of GEN"
  (do ((elt (funcall gen) (funcall gen)))
      ((or (eq elt 'end-token) (not (funcall fn elt)))
       (eq elt 'end-token))))

(defun some (fn gen)
  "eagerly determine if FN returns true for an element of GEN"
  (do ((elt (funcall gen) (funcall gen)))
      ((or (eq elt 'end-token) (funcall fn elt))
       (if (eq elt 'end-token)
	   (values nil nil)
	   (values t elt)))))

(defun groups (size keep gen)
  "lazily group GEN in groups of SIZE, keeping KEEP amount of
   overlap in successize groups"
  (if (zerop keep)
      ;; no overlap in each result...
      (lambda ()
	(gen->list (take size gen)))
      ;; overlapping windows....
      (let ((overlap nil)
	    (remainder (- size keep)))
	(lambda ()
	  (when (null overlap)
	    (setq overlap (gen->list (take keep gen))))
	  (let ((result (common-lisp:append overlap
					    (gen->list (take remainder gen)))))
	    (setq overlap (nthcdr remainder result))
	    result)))))

(defun append (&rest gens)
  "provide elements from generators GENS, in the order given"
  (labels ((get-next ()
	     (if (null gens) 'end-token
		 (let ((elt (funcall (car gens))))
		   (if (eq elt 'end-token)
		       (progn
			 (setq gens (cdr gens))
			 (get-next))
		       elt)))))
    #'get-next))

(defmacro --> (&body clauses)
  "define a pipeline for data in a lazy generated sequence"
  (reduce #'(lambda (c1 c2) (common-lisp:append c2 (list c1))) clauses))
