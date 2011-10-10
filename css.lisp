(in-package :info.read-eval-print.web)

(eval-always
  (named-readtables:defreadtable css
    (:merge nil)
    (:macro-char #\# (lambda (stream char)
                       (declare (ignore char))
                       (let* ((*read-base* 16)
                              (x (read stream t nil t)))
                         (typecase x
                           (number (format nil "#~6,'0x" x))
                           (t (format nil "#~a" x))))))
    (:case :upcase))

  (defun split-css-form (form)
    (let ((result '())
          (acc '()))
      (labels ((f (xs)
                 (cond ((endp xs)
                        (nreverse (cons (nreverse acc)  result)))
                       ((keywordp (car xs))
                        (push (nreverse acc) result)
                        (setf acc (list (car xs)))
                        (f (cdr xs)))
                       (t
                        (push (car xs) acc)
                        (f (cdr xs))))))
        (f form))))

  (defun css-filter (form bindings)
    (cond ((null form)
           nil)
          ((atom form)
           (getf bindings form (string-downcase form)))
          (t
           (cons (css-filter (car form) bindings)
                 (css-filter (cdr form) bindings)))))


  (defmacro css (bindings &body body)
    (alexandria:with-gensyms (out)
      `(with-output-to-string (,out)
         ,@(loop for xs in body
                 for splited = (split-css-form xs)
                 append `((format ,out "~{~a ~}{" (css-filter ',(car splited) ',bindings))
                          ,@(loop for xs in (cdr splited)
                                  collect `(format ,out " ~a:" ,(string-downcase (car xs)))
                                  collect `(format ,out "~{ ~a~};" (css-filter ',(cdr xs) ',bindings)))
                          (format ,out " }~%"))))))
  )

(named-readtables:in-readtable css)

(print (css ((color1 #00000f)
             (color2 #ff22ff))
         (body :text-color #000000
               :font-size 100%)
         (div.foo ul :border solid 1px color1
                     :float left
                     :width (+ 1 2))
         (#footer :text-color color2)))



(NAMED-READTABLES:IN-READTABLE NIL)
