(in-package #:point)

;; Accessors
(defgeneric x (p)
  (:method ((p 2dpoint)) (sx p))
  (:method ((p cons)) (car p))
  (:method ((p complex)) (realpart p))
  (:method ((p vector)) (aref p 0)))

(defgeneric y (p)
  (:method ((p 2dpoint)) (sy p))
  (:method ((p cons)) (cdr p))
  (:method ((p complex)) (imagpart p))
  (:method ((p vector)) (aref p 1)))

(defgeneric z (p)
  (:method ((p 3dpoint)) (sz p))
  (:method ((p vector)) (if (= 3 (length p))(aref p 2) 0))
  (:method (p) 0)) ;more harmful than helpful? same above

(defgeneric w (p)
  (:method (p) 0))

(defgeneric (setf x) (p value)
  (:method (value (p 3dpoint)) (setf (sx p) value))
  (:method (value (p vector)) (setf (aref p 0) value))
  (:method (value (p 2dpoint)) (setf (sx p) value))
  (:method (value(p cons)) (rplaca p value ))
  (:method (value(p complex)) (complex value (imagpart p))));this can't be right

(defgeneric (setf y) (p value)
  (:method (value (p 3dpoint)) (setf (sy p) value))
  (:method (value (p vector)) (setf (aref p 1) value))
  (:method (value (p 2dpoint)) (setf (sy p) value))
  (:method (value(p cons)) (rplacd p value ))
  (:method (value(p complex)) (complex (realpart p) value)));fix this crap

(defgeneric (setf z) (p value)
  (:method (value (p 3dpoint)) (setf (sz p) value))
  (:method (value (p vector)) (setf (aref p 2) value))
  (:method (value p)));mucho badness, no?

(defgeneric (setf w) (p value)
  (:method (value p)))
;; Transforms
(defgeneric ->list (p)
  (:method ((p 2dpoint)) (list (sx p) (sy p)))
  (:method ((p 3dpoint)) (list (sx p) (sy p) (sz p)))
  (:method ((p cons)) (list (car p) (cdr p)))
  (:method ((p complex)) (list (realpart p) (imagpart p)))
  (:method ((p vector)) (coerce p 'list))
  (:method ((p integer)) (list p 0)))

(defgeneric ->values (p)
  (:method ((p 2dpoint)) (values (sx p) (sy p)))
  (:method ((p 3dpoint)) (values (sx p) (sy p) (sz p)))
  (:method ((p cons)) (values (car p) (cdr p)))
  (:method ((p complex)) (values (realpart p) (imagpart p)))
  (:method ((p vector)) (values (aref p 0) (aref p 1)))
  (:method ((p integer)) (values p 0)))

(defgeneric ->vector (p)
  (:method ((p 2dpoint)) (vector (sx p) (sy p)))
  (:method ((p 3dpoint)) (vector (sx p) (sy p) (sz p)))
  (:method ((p cons)) (vector (car p) (cdr p)))
  (:method ((p complex)) (vector (realpart p) (imagpart p)))
  (:method ((p vector)) p)
  (:method ((p integer)) (vector p 0)))

;; util
(defgeneric dimensions (p)
  (:method ((p 3dpoint))3)
  (:method ((p 2dpoint)) 2)
  (:method ((p cons)) 2)
  (:method ((p complex)) 2)
  (:method ((p vector)) (length p))
  (:method ((p number))1))

(defgeneric copy (p)
  (:method ((p 3dpoint)) (3d (sx p) (sy p) (sz p)))
  (:method ((p vector)) (copy-seq p))
  (:method ((p 2dpoint)) (2d (sx p) (sy p)))
  (:method ((p cons)) (cons (car p) (cdr p)))
  (:method ((p complex))(complex (x p) (y p)))
  ;(:method ((list list)) (mapcar #'copy list))
  ) 

(defgeneric element-type (p)
  (:method (p) 'number))

