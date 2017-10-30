;;;; package.lisp

(defpackage #:what3words
  (:use #:cl)
  (:export :words-to-loc
	   :loc-to-words
	   :autosuggest
	   :standardblend
	   :grid
	   :get-languages))
