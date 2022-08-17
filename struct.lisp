(in-package :point)

;;;; struct points to fall back on
(declaim (inline 2d 3d sx sy sz))
(defstruct (2dpoint (:conc-name nil)
		    (:constructor 2d (sx sy)))
  sx sy)
(defstruct (3dpoint (:conc-name nil)(:include 2dpoint) (:constructor 3d (sx sy sz)))
  sz)

(defmethod print-object ((p 2dpoint)stream)
  (if *print-readably*
      (call-next-method)
      (format stream "P (~a,~a)" (x p) (y p))))

(defmethod print-object ((p 3dpoint)stream)
  (if *print-readably*
      (call-next-method)
      (format stream "P (~a,~a,~a)" (x p) (y p) (z p))))







