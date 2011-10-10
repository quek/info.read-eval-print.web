(in-package :info.read-eval-print.web)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Web server dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defvar *dispatch* (hunchentoot:create-prefix-dispatcher *content-root* 'dispatch))
;;
;;
;;(setf hunchentoot:*dispatch-table*
;;      (list
;;       (hunchentoot:create-folder-dispatcher-and-handler "/css/" *css-path*)
;;       (hunchentoot:create-folder-dispatcher-and-handler "/js/" *js-path*)
;;       (hunchentoot:create-folder-dispatcher-and-handler "/img/" *img-path*)
;;       *dispatch*))

(defvar *routes* (make-instance 'routes:mapper))

(defclass* route (routes:route)
  ((function)))

(defun dispatch ()
  (let* ((url (hunchentoot:request-uri hunchentoot:*request*))
         (*get-parameters* (hunchentoot:get-parameters*))
         (*post-parameters* (hunchentoot:post-parameters*)))
    (multiple-value-bind (route bindings) (routes:match *routes* url)
      (call-action route (alexandria:alist-plist bindings)))))

(defun call-action (route bindings)
  (princ-to-string (apply (if route (function-of route) '|404|)
                          bindings)))

(defun redirect (url &key permanently-p)
  (hunchentoot:redirect url :code (if permanently-p
                                      hunchentoot:+http-moved-permanently+
                                      hunchentoot:+http-moved-temporarily+)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Web server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf
 ;; for utf-8
 hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8)
 hunchentoot:*default-content-type* "text/html; charset=utf-8")
