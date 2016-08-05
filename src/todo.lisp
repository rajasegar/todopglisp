(in-package :example)

;; Utils
(defun heroku-getenv (target)
  #+ccl (ccl:getenv target)
  #+sbcl (sb-posix:getenv target))

(defun db-params ()
  "Heroku database url format is postgres://username:password@host:port/database_name"
  (let* (
         (url (second (cl-ppcre:split "//" (heroku-getenv "DATABASE_URL"))))
         (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
         (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
         (host (first (cl-ppcre:split ":" (first (cl-ppcre:split "/" (second (cl-ppcre:split "@" url)))))))
         (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@" url))))))
      ;(list "todopglisp" "Rajasegar" "" "localhost")))
      (list database user password host)))

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/api"))

(defun-ajax toggle-done (name done) (*ajax-processor* :callback-data :response-text)
    (with-connection (db-params)
        (query (:update 'todo :set 'done '$1 :where (:= 'name name)) done))
    (concatenate 'string "Todo status updated"))

(defun add-todo (name)
  (with-connection (db-params)
      (query (:insert-into 'todo :set 'name name))))

(setf (html-mode) :html5)

;; Create Database Schema
(defun init-db ()
    (with-connection (db-params)
        (unless (table-exists-p "todo")
            (query (:create-table todo
                ((id :type serial :primary-key t)
                 (name :type varchar :default "")
                 (done :type boolean :default nil)))))))


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
              (str (generate-prologue *ajax-processor*))
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
                          (chain console (log "Hello"))
                          (defun callback (response)
                            (chain console (log response)))

                          (chain document  (add-event-listener "click" (lambda (event)
                                ;(chain console (log event))
                                (let* ((target (@ event target))
                                      (target-class (@ target class-name))
                                      (target-done (@ target checked))
                                       )
                                  (when (= target-class "chkDone")
                                        (chain console (log target-done))
                                       (chain smackjack (toggle-done (chain target (get-attribute "data-name")) target-done callback)))
                                  (when (= target-class "btnDelete")
                                        (chain event (prevent-default))
                                        (chain console (log "Delete button clicked"))
                                        (chain smackjack (ajax-delete (chain target (get-attribute "data-id")) callback))
                                        (let ((el (chain document (get-element-by-id (chain target (get-attribute "data-id"))))))
                                        (chain (@ el parent-node) (remove-child el))))
                                  
                                  ))))))
               (:h4 :class "text-right" "Total items: " (:span (fmt "~A" (row-count))))
               (:ol
                 (dolist (item (with-connection (db-params)
                    (query (:select '* :from 'todo))))
                   (htm
                     (:li :id (first item)
                       (:input :type "checkbox"  :data-name (second item) :checked (third item) :class "chkDone")
                       (:spacer)
                       (fmt "~a" (second item))
                       ;(:button :class "btnDelete" :data-id (first item) "Delete")
                       (:a :class "text-danger" :data-name (second item) :href (format nil "/delete?name=~a" (url-encode (second item))) "Delete")
                       ))))
               (:form :class "form" :action "/todo-added" :method "post"
                    (:p "Add a new task:"
                        (:input :class "form-control" :type "text" :name "name" :required t :autofocus t))
                    (:p :class "text-right"
                        (:input :type "submit" :value (format nil "Add Todo #~d" (+ 1 (row-count))) :class "btn btn-success btn-lg")))))

(define-easy-handler (todo-added :uri "/todo-added") (name)
    (unless (or (null name) (zerop (length name)))
      (add-todo name))
    (redirect "/"))

(define-easy-handler (todo-delete :uri "/delete") (name)
    ; delete the item here
    (with-connection (db-params)
        (query (:delete-from 'todo :where (:= 'name name))))
    (redirect "/"))

(define-easy-handler (init-db :uri "/init-db") ()
    (init-db)
    (redirect "/"))

(defun-ajax ajax-delete (id) (*ajax-processor* :callback-data :response-text)
    (with-connection (db-params)
        (query (:delete-from 'todo :where (:= 'id id))))
    (concatenate 'string "Todo deleted with: " id))

(setq *dispatch-table* (list 'dispatch-easy-handlers (create-ajax-dispatcher *ajax-processor*)))

(defun start-server()
    (start (make-instance 'easy-acceptor :port 3000)))
