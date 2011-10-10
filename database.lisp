(in-package :info.read-eval-print.web)

(defun open-db ()
  (unless rucksack:*rucksack*
    (setf rucksack:*rucksack* (rucksack::open-rucksack (get-db-directory)))))

(defun close-db ()
  (when rucksack:*rucksack*
    (rucksack:close-rucksack rucksack:*rucksack*)
    (setf rucksack:*rucksack* nil)))

(defmacro with-transaction (&body body)
  `(let ((rucksack:*rucksack* (rucksack-of *application*)))
     (rucksack:with-transaction ()
       ,@body)))


(defun scan-slot% (class slot options)
  (let (xs)
    (apply #'rucksack:rucksack-map-slot
           (getf options :rucksack rucksack:*rucksack*)
           class
           slot
           (^ push _ xs)
           (rucksack::sans options :rucksack))
    (nreverse xs))  )

(defun scan-slot-f (class slot options)
  "動くだけの実装
options is &key (rucksack rucksack:*rucksack*)
equal min max include-min include-max
order include-subclasses"
  (declare (optimizable-series-function))
  (producing (z) ((xs (scan-slot% class slot options))
                  x)
    (loop
      (tagbody
         (if (endp xs)
             (terminate-producing))
         (setq x (car xs))
         (setq xs (cdr xs))
         (next-out z x)))))

(defmacro scan-slot (class slot &rest args &key (rucksack 'rucksack:*rucksack*)
                                             equal min max include-min include-max
                                             order include-subclasses)
  (declare (ignore rucksack equal min max include-min include-max order include-subclasses))
  `(scan-slot-f ',class ',slot (list ,@args)))


(defun scan-class% (class options)
  (let (xs)
    (apply #'rucksack:rucksack-map-class
           (getf options :rucksack rucksack:*rucksack*)
           class
           (^ push _ xs)
           :id-only (getf options :id-only nil)
           :include-subclasses (getf options :include-subclasses t)
           nil)
    (nreverse xs)))

(defun scan-class-f (class options)
  "動くだけの実装
options is &key (rucksack '*rucksack*) id-only (include-subclasses t)"
  (declare (optimizable-series-function))
  (producing (z) ((xs (scan-class% class options))
                  x)
    (loop
      (tagbody
         (if (endp xs)
             (terminate-producing))
         (setq x (car xs))
         (setq xs (cdr xs))
         (next-out z x)))))

#+こっちでもいける。
(series::defs scan-class-f (class options)
  ""
  (series::fragl ((class) (options))
                 ((object t))
                 ((object t) (lst list nil))
                 ()
                 ((setq lst (scan-class% class options)))
                 ((if (null lst) (go series::end))
                  (setq object (car lst))
                  (setq lst (cdr lst)))
                 ()
                 ()
                 :mutable))

(defmacro scan-class (class &key (rucksack 'rucksack:*rucksack*) id-only (include-subclasses t))
  `(scan-class-f ',class (list :rucksack ,rucksack
                               :id-only ,id-only
                               :include-subclasses ,include-subclasses)))

(defstruct scan-class-iterator
  (visited-p (make-hash-table))
  index
  (superclasses ())
  node
  node-size
  node-index
  next)

(defun scan-class-iterator-next (iterator)
  (prog ()
     go
     (unless (scan-class-iterator-node iterator)
       (unless (slot-boundp (scan-class-iterator-index iterator) 'root)
         ))))
(defun scan-class-iterator (class &key (rucksack 'rucksack:*rucksack*) id-only (include-subclasses t))
  (make-scan-class-iterator :index (rucksack::rucksack-class-index class :errorp nil)
                            :superclasses (when include-subclasses
                                            (rucksack::class-direct-subclasses
                                             (if (symbolp class)
                                                 (find-class class)
                                                 class))))
  )

#|
(with-transaction
  (collect (scan-slot user email)))

(let ((email "user1@example.com"))
  (with-transaction
    (collect-first (scan-slot user email :equal email))))

(with-transaction
  (collect (scan-class user)))

(with-transaction
  (collect (scan-class user :id-only t)))
|#
