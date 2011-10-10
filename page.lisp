(in-package :info.read-eval-print.web)

(defparameter *default-layers* '(rucksack-transaction) "layers")

(defvar *errors* nil)

(contextl:define-layered-class page ()
  ())

(contextl:define-layered-function eval-page (page &key &allow-other-keys))

(defclass* page-output ()
  ((contents)))

(defmethod print-object ((page-output page-output) stream)
  (let ((*response-stream* stream))
    (collect-ignore
     (render (scan-lists-of-lists-fringe (contents-of page-output))
             *browser*))))

(defmacro defpage (name (&key
                           url
                           varspecs
                           (page-class 'page)
                           layers)
                   &body body)
  (let ((bindings (and url (collect-bindings url)))
        (layers (alexandria:ensure-list layers)))
    `(progn
       (contextl:deflayer ,name)
       (contextl:define-layered-method eval-page :in ,name ((page page)
                                                            &key ,@bindings)
         ,@(when (and (consp (cdr body))
                      (stringp (car body)))
             (prog1 (list (car body))
               (setf body (cdr body))))
         ,@(when (and (consp (car body))
                      (eq 'declare (caar body)))
             (prog1 (list (car body))
               (setf body (cdr body))))
         (with-http-parameters ,@body))
       (defun ,name ,(if bindings `(&rest args &key ,@bindings))
         ,@(when bindings `((declare (ignorable ,@bindings))))
         (make-instance 'page-output
                        :contents (contextl:with-active-layers (,@layers ,@*default-layers* ,name)
                                    (apply #'eval-page (make-instance ',page-class) ,(if bindings 'args ())))))
       ,(when url
          `(connect ',name ,url ,varspecs)))))

(defun connect (function url &optional varspecs)
  (routes:connect *routes*
                  (make-instance 'route
                                 :function function
                                 :template (routes:parse-template url varspecs))))

(defun collect-bindings (url)
  (progs ()
    (scan (routes:parse-template url))
    (choose-if #'routes::variable-p)
    (routes:template-data)
    (symbol-name)
    (intern)
    (collect 'bag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; トランザクション
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(contextl:define-layered-method eval-page :in rucksack-transaction :around (page &key)
  (let (response (handler-done t))
    (with-transaction
      ;; hunchentoot のリダイレクトのハンドリング
      (catch 'hunchentoot::handler-done
        (setf response (call-next-method))
        (setf handler-done nil)))
    (if handler-done
        (throw 'hunchentoot::handler-done nil)
        response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; For test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-render (x)
  (with-output-to-string (*response-stream*)
    (collect-ignore
     (render (scan-lists-of-lists-fringe x)
             *browser*))))

