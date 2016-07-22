;(ql:quickload '(:cl-who :hunchentoot :parenscript :postmodern))


(in-package :example)

;; Utils
(defun heroku-getenv (target)
  #+ccl (ccl:getenv target)
  #+sbcl (sb-posix:getenv target))

(defun heroku-slug-dir ()
  (heroku-getenv "HOME"))

(print (sb-ext:posix-getenv "DATABASE_URL"))
(defvar *heroku-pg-url* "postgres://quyzsdidqvupft:nwBYLXVX58EuDDPTQXZMc-fYsL@ec2-54-235-95-188.compute-1.amazonaws.com:5432/ddpe3h03js3ebm")

(defun db-params ()
  "Heroku database url format is postgres://username:password@host:port/database_name"
  (let* (
         ;(url (second (cl-ppcre:split "//" (heroku-getenv "DATABASE_URL"))))
         (url (second (cl-ppcre:split "//" *heroku-pg-url*)))
         (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
         (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
         (host (first (cl-ppcre:split ":" (first (cl-ppcre:split "/" (second (cl-ppcre:split "@" url)))))))
         (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@" url))))))
      ;(list "todopglisp" "Rajasegar" "" "localhost")
      (list database user password host)))



(defun add-todo (name)
  (with-connection (db-params)
      (query (:insert-into 'todo :set 'name name))))

(setf (html-mode) :html5)

;; Create Database Schema
(with-connection (db-params)
    (unless (table-exists-p "todo")
        (query (:create-table todo
            ((id :type serial :primary-key t)
             (name :type varchar :default "")
             (done :type boolean :default nil))))))


(defmacro todo-page ((&key title script) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
        (:html :lang "en"
            (:head
              (:meta :charset "utf-8")
              (:title, title)
              (:link
                :type "text/css"
                :rel "stylesheet"
                :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css")
              ,(when script
                 `(:script :type "text/javascript" (str, script))))
            (:body
              (:div :class "container"
                (:div :class "row"
                      (:div :class "col-md-12"
                            (:br)
                            (:div :class "jumbotron"
                                  (:h1 "Todo,Lisp!")
            ,@body))))))))

(defun row-count ()
  (with-connection (db-params)
        (query (:select (:count '*) :from 'todo) :single)))


              
;; Handlers

(define-easy-handler (app :uri "/") ()
    (todo-page (:title "TodoList"
                :script (ps
                          (chain console (log "Hello"))))
               (:h4 :class "text-right" "Total items: " (:span (fmt "~A" (row-count))))
               (:ol
                 (dolist (item (with-connection (db-params)
                    (query (:select 'name 'done :from 'todo))))
                   (htm
                     (:li
                       (fmt "~a" (first item))
                       (:a :class "text-danger" :href (format nil "/delete?name=~a" (url-encode (first item))) "Delete")))))
               (:form :class "form" :action "/todo-added" :method "post"
                    (:p "Add a new task:"
                        (:input :class "form-control" :type "text" :name "name"))
                    (:p :class "text-right"
                        (:input :type "submit" :value (format nil "Add Todo #~d" (+ 1 (row-count))) :class "btn btn-primary btn-lg")))))

(define-easy-handler (todo-added :uri "/todo-added") (name)
    (unless (or (null name) (zerop (length name)))
      (add-todo name))
    (redirect "/"))

(define-easy-handler (todo-delete :uri "/delete") (name)
    ; delete the item here
    (with-connection (db-params)
        (query (:delete-from 'todo :where (:= 'name name))))
    (redirect "/"))


;(defun start-server (port) 
;  (start (make-instance 'easy-acceptor :port port)))
;(start-server 3000)
