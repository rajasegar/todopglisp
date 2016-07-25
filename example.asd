(asdf:defsystem #:example
    :serial t
    :description "Todo List using Common Lisp with Postgres database"
    :depends-on (#:hunchentoot
                #:cl-who
                #:parenscript
                #:postmodern
                #:smackjack)
    :components ((:file "package")
                (:module :src
                        :serial t
                        :components ((:file "todo")))))
