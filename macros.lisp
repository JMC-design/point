(in-package :point)

;;useless util
(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun split-dot (symbol)
   (destructuring-bind (sym fn) (split-sequence:split-sequence #\. (symbol-name symbol))
     (list (intern fn) (intern sym))))

    (defun walk (tree test transform)
      (flet ((transform? (item test transform)
               (if (funcall test item)
                   (funcall transform item)
                   item)))
        (cond
          ((null tree) (values))
          ((atom tree) (case tree
                         (+ 'cl:+)
                         (- 'cl:-)
                         (/ 'cl:/)
                         (* 'cl:*)
                         (max 'cl:max)
                         (min 'cl:min)
                         (t (transform? tree test transform))))
          ((consp tree) (cons (walk (car tree) test transform) (walk (cdr tree) test transform)))))))

(defmacro with-dot (&body body)
  `(let (dummy)
     (declare (ignore dummy))
     ,@(walk body (lambda (x) (and (symbolp x) (position #\. (symbol-name x)) (not (eql x '/.)))) #'split-dot)))
