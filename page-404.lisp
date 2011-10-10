(in-package :info.read-eval-print.web)

(defpage |404| ()
  (html
    (:html
      (:head
       (:title "Not Found."))
      (:body
       (:p "Not Found.")))))
