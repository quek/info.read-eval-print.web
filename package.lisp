;;;; package.lisp

(quek:sdefpackage
 :info.read-eval-print.web
 (:use :cl :quek :anaphora)
 (:import-from :hu.dwim.defclass-star #:defclass*)
 (:export #:*application*
          #:application
          #:env-of
          #:rucksack-of
          #:port-of
          #:server-of
          #:db-directory-of
          #:conetx-root-of
          #:document-root-of
          #:start-application
          #:stop-application
          #:start
          #:stop

          #:model
          #:timestamp-mixin
          #:created-at
          #:updated-at

          #:html
          #:raw
          #:url
          #:with-http-parameters
          #:redirect

          #:defpage

          #:*response-stream*
          #:*login-user*

          #:*default-layers*
          #:rucksack-transaction
          #:with-transaction
          #:scan-slot
          #:scan-class

          #:*env*
          #:+production+
          #:+development+
          #:+test+


          #:|404|))
