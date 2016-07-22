(in-package :cl-user)

(print ">>> Building system....")

(load (merge-pathnames "todopglisp.asd" *build-dir*))

(ql:quickload :todopglisp)

;;; Redefine / extend heroku-toplevel here if necessary

(print ">>> Done building sytem")
