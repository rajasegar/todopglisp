(asdf:defsystem #:todopglisp
    :serial t
    :description "Todo List using Common Lisp with Postgres database"
    :depends-on (#:hunchentoot
                #:cl-who
                #:parenscript
                #:postmodern)
    :components ((:file "package")
                (:module :src
                        :serial t
                        :components ((:file "todo")))))
