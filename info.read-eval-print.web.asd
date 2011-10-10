(asdf:defsystem :info.read-eval-print.web
  :serial t
  :components ((:file "package")
               (:file "type")
               (:file "util")
               (:file "prelude")
               (:file "html")
               (:file "tag")
               (:file "database")
               (:file "web-server")
               (:file "layer")
               (:file "model")
               (:file "page")
               (:file "page-404")
               (:file "application"))
  :depends-on (:quek
               :hunchentoot
               :parenscript
               :rucksack
               :cl-oauth
               :routes
               :simple-date-time
               :contextl
               :hu.dwim.defclass-star
               :uuid))
