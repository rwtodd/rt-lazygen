;;;; package.lisp

(defpackage #:rt-lazygen
  (:use #:cl)
  (:export #:lempty-gen
	   #:llist
	   #:lmap
	   #:lfilter
	   #:for-each
	   #:to-list
	   #:ltake
	   #:ldrop
	   #:ltake-while
	   #:ldrop-while
	   #:lrange))

