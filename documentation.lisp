(in-package #:point)

(defun document (package doc-strings)
  (let ((package (find-package package)))
    (dolist (doc-string doc-strings)
      (destructuring-bind (fn doc) doc-string
	(setf (documentation (find-symbol (symbol-name fn) package) 'function) doc)))))

(document 'point
          '((*
             "Multiplies a point by a scalar, or by another point component wise.")
            (distance
             "Minkowski distance calculator with optional hyper set to 2 (euclidean).")
            (euclidean-sqrt
             "Badly named euclidean distance without performing the final sqrt. Valid comparison between points.")
            (lerp
             "Your basic lerp function that takes an optional transformer function to apply to the tee. Useful with EASING.")))
