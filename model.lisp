(in-package :info.read-eval-print.web)

(eval-always
  (iterate ((x (scan '(:index :unique))))
    (pushnew x hu.dwim.defclass-star:*allowed-slot-definition-properties*)))

