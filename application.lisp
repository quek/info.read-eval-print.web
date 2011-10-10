(in-package :info.read-eval-print.web)

(defvar *application* nil)


(defgeneric start-application (application))

(defgeneric stop-application (application))


(defclass* application ()
  ((env +development+)
   (rucksack nil)
   (port 3000)
   (server nil)
   (db-directory)
   (context-root "/")
   (document-root "/tmp")))


(defmethod start-application ((self application))

  ;; rucksack
  (setf (rucksack-of self) (rucksack:open-rucksack (db-directory-of self)))
  (with-transaction
    ;; medels
    (defclass* model ()
      ()
      (:metaclass rucksack:persistent-class))
    (defclass* timestamp-mixin ()
      ((created-at (dt:now) :accessor created-at)
       (updated-at (dt:now) :accessor updated-at))
      (:metaclass rucksack:persistent-class))
    (defmethod rucksack::save-dirty-object :before ((self timestamp-mixin) cache transaction object-id &key)
      (setf (updated-at self) (dt:now))))

  ;; hunchentoot
  (setf hunchentoot:*catch-errors-p* (eq (env-of self) +production+))
  (push (create-request-dispatcher self) hunchentoot:*dispatch-table*)
  (setf (server-of self) (make-instance 'hunchentoot:acceptor :port (port-of self)))
  (hunchentoot:start (server-of self))

  self)

(defmethod stop-application ((self application))
  (rucksack:close-rucksack (rucksack-of self))
  (hunchentoot:stop (server-of self))
  self)


(defun start (&key (env +development+) (port 3000))
  (setf *application*
        (make-instance 'application :env env :port port))
  (start-application *application*))

(defun stop ()
  (stop-application *application*))


(defgeneric create-request-dispatcher (application))
(defmethod create-request-dispatcher ((self application))
  (let ((document-dispatcher (hunchentoot:create-folder-dispatcher-and-handler
                              (context-root-of self) (document-root-of self)))
        (action-dispatcher (hunchentoot:create-prefix-dispatcher (context-root-of self) #'dispatch)))
    (lambda (request)
      (let ((document-handler (funcall document-dispatcher request))
            (action-handler (funcall action-dispatcher request)))
        (lambda ()
          (let* ((handler-done t)
                 (result (catch 'hunchentoot::handler-done
                           (prog1 (funcall document-handler)
                             (setf handler-done nil)))))
            (cond ((= (hunchentoot::return-code*) hunchentoot::+http-not-found+)
                   (setf (hunchentoot::return-code*) hunchentoot::+http-ok+)
                   (funcall action-handler))
                  (handler-done
                   (throw 'hunchentoot::handler-done result))
                  (t result))))))))
