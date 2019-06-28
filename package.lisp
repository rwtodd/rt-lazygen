;;;; package.lisp

(defpackage "RT-LAZYGEN"
  (:use "COMMON-LISP")
  (:shadow "MAP" "EVERY" "SOME" "APPEND" "FIRST" "LAST")
  (:export "EMPTY-GEN"
	   "LIST->GEN"
	   "GEN->LIST"
	   "LIST->CYCLE"
	   "MAP"
	   "FILTER"
	   "FOR-EACH"
	   "TAKE"
	   "FIRST"
	   "LAST"
	   "DROP"
	   "TAKE-WHILE"
	   "DROP-WHILE"
	   "RANGE"
	   "ITERATE"
	   "FOLD"
	   "SCAN"
	   "EVERY"
	   "SOME"
	   "GROUPS"
	   "APPEND"
	   "-->"))
