;;;; what3words.asd

(asdf:defsystem #:what3words
  :description "A Common Lisp client for working with the what3words.com API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
               #:cl-json
               #:babel)
  :serial t
  :components ((:file "package")
               (:file "what3words")))

