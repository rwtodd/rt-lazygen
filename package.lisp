;;;; package.lisp

(defpackage "RT-LAZYGEN"
  (:use "COMMON-LISP")
  (:export "LG-EMPTY-GEN"
	   "LG-LIST"
	   "LG-MAP"
	   "LG-FILTER"
	   "LG-FOR-EACH"
	   "LG-TO-LIST"
	   "LG-TAKE"
	   "LG-FIRST"
	   "LG-LAST"
	   "LG-DROP"
	   "LG-TAKE-WHILE"
	   "LG-DROP-WHILE"
	   "LG-RANGE"
	   "LG-ITERATE"
	   "LG-FOLD"
	   "LG-EVERY"
	   "LG-SOME"
	   "LG-GROUPS"
	   "LG-APPEND"
	   "LG-->"))
