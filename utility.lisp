(in-package #:point)

(defgeneric angle (p1 &optional p2 degrees) ;the big fixme, angles in degrees CW from +y? Radians be radians? handle 0 mags
  (:method (p1 &optional (p2 p1) degrees) (let* ((mags  (cl:* (magnitude p1)(magnitude p2)))
                                                 (dot (dot p1 p2))
                                                 (result 
                                                   (if (zerop mags)
                                                       (error "I'm too dumb to handle 0 magnitude points.")
                                                       (acos (cl:/ dot mags)))))
                                            (if degrees
                                                (cl:* result (cl:/ 180 pi))
                                                result))))
;;;; Distances 
(defgeneric euclidean-sqrt (p1 p2)
  (:method (p1 p2) (with-dot (reduce #'+ (mapcar (lambda (i j) (expt (- j i)2)) (->list p1)(->list p2))))))
(defgeneric euclidean(p1 p2)
  (:method (p1 p2) (sqrt (euclidean-sqrt p1 p2))))
(defgeneric manhattan (p1 p2)
  (:method (p1 p2) (reduce #'cl:+ (map 'list #'abs (->list (- p2 p1))))))
(defgeneric minkowski (p1 p2 h)
  (:method (p1 p2 h) (with-dot (expt (reduce #'+ (mapcar (lambda (i j) (expt (- j i) h)) (->list p1)(->list p2))) (/ h)))))
(defgeneric distance (p1 p2 &optional h)
  (:method (p1 p2 &optional (h 2)) (minkowski p1 p2 h)))
(defgeneric of-division (p1 p2 &optional divisor)
   (:method (p1 p2 &optional (divisor 0.5))(* (+ p1 p2) divisor)))
(defgeneric 1norm (p)
  (:method (p) (manhattan p +origin+) ))
(defgeneric 2norm (p)
  (:method (p) (euclidean p +origin+)))
(defgeneric pnorm (p hyper)
  (:method (p hyper) (minkowski p +origin+ hyper)))



(defun random (type &optional (n 1) (width 500) (height width) depth &key (random-fn #'cl:random)) ;add return types
  (let ((points (random-points type n width height depth :random-fn random-fn)))
    (if (= 1 n)
        (car points)
        points)))

(Defun random-points (type n width height &optional depth &key (random-fn #'cl:random) &aux (dimension (if depth 3 2)))
  (loop :repeat n :collect (case type
                             (:vector (if (= 3 dimension)
                                          (vector (funcall random-fn width)(funcall random-fn height)(funcall random-fn depth))
                                          (vector (funcall random-fn width)(funcall random-fn height))))
                             (:cons (cons (funcall random-fn width)(funcall random-fn height)))
                             (:complex (complex (funcall random-fn width)(funcall random-fn height)))
                             (:2d (2d (funcall random-fn width)(funcall random-fn height)))
                             (:3d (3d (funcall random-fn width)(funcall random-fn height)(funcall random-fn depth)))
                             (t (new type (funcall random-fn width)(funcall random-fn height)(funcall random-fn depth)))))
  ;; (let ((points (case dimension
  ;;       	  (2 (loop :for i :below n :collect (point:2d (funcall random-fn width) (funcall random-fn height))))
  ;;       	  (3 (loop :for i :below n :collect (point:3d (funcall random-fn width) (funcall random-fn height) (funcall random-fn depth)))))))
  ;;   (if (= (length points) 1) ;why force everybody to check if its a list or atom?
  ;;       (car points)
  ;;       points))
  )

(defun transform (p transform)
  (let ((result (copy p)))
    (with-dot
      (setf result.x (funcall transform p.x)
            result.y (funcall transform p.y)
            result.z (funcall transform p.z))))) ;welp that's inefficient for 2d, suck it. Maybe make generic, maybe inline.

(defun ntransform (p transform)
  (with-dot
    (setf p.x (funcall transform p.x)
          p.y (funcall transform p.y)
          p.z (funcall transform p.z))))



;; Is there any use for something like these?
(defmethod add-x ((p1 3dpoint) (p2 3dpoint))
  (3d (cl:+ (x p1)(x p2))(y p1)(z p1)))
(defmethod add-y ((p1 3dpoint) (p2 3dpoint))
  (3d (x p1)(cl:+ (y p1)(y p2))(z p1)))
(defmethod add-z ((p1 3dpoint) (p2 3dpoint))
  (3d (x p1)(y p1)(cl:+ (z p1)(z p2))))

(defun sort-x (points)
  (sort (copy-seq points) (lambda (p1 p2) (or (< (x p1)(x p2))
					(and (= (x p1)(x p2))
					     (< (y p1)(y p2)))))))
(defun sort-y (points)
  (sort (copy-seq points) (lambda (p1 p2) (or (< (y p1)(y p2))
				              (and (= (y p1)(y p2))
					           (< (x p1)(x p2)))))))


;;;; taken from kons-9, frankly it(quit's just blatant copying at the bottom. I didn't include some that I didn't understand the purpose of, or didn't see as part of a protocal but a usage of that protocol. I may have bent that rule.

;I may unbend this. What do specific points have to do with a point protocol?
;; (defconstant +origin+ (point:3d 0 0 0))
;; (defconstant +x-axis+ (3d 1 0 0))
;; (defconstant +y-axis+ (3d 0 1 0))
;; (defconstant +z-axis+ (3d 0 0 1))

(declaim (inline slerp))
(defun slerp (i j tee &optional transform) "simple"
  (with-dot (let ((tee (if (null transform) tee (funcall transform tee))))
              (+ i ( * tee (- j i))))))

(defgeneric lerp (p1 p2 tee &optional transform)
  (:method (p1 p2 tee &optional transform) (with-dot
                                             (if (= 3 (dimensions p1))
                                                 (3d (slerp p1.x p2.x tee transform)(slerp p1.y p2.y tee transform)(slerp p1.z p2.z tee transform))
                                                 (2d (slerp p1.x p2.x tee transform)(slerp p1.y p2.y tee transform))))))

(defgeneric == (p1 p2 &optional epsilon) ;clean this up by element type, wasteful for integers only though we have no integer only impls.
  (:method (p1 p2 &optional (epsilon single-float-epsilon)) (with-dot (and (< (abs (- p1.x p2.x)) epsilon)
                                                                           (< (abs (- p1.y p2.y)) epsilon)
                                                                           (< (abs (- p1.z p2.z)) epsilon)))))

(defgeneric negate (p)
  (:method (p) (* p -1)))

(defgeneric normalize (p)
  (:method ((p 3dpoint)) (let ((mag (magnitude p)))
                           (if (= 0 mag)
                               (3d 0 0 0)
                               (with-dot
                                 (3d (/ p.sx mag)
                                     (/ p.sy mag)
                                     (/ p.sz mag))))))
  (:method (p) (let ((mag (magnitude p)))
                           (if (= 0 mag)
                               (* p 0)
                               (/ p mag)))))

(defgeneric angle-cosine (p1 &optional p2) ;this doesn't seem right.
  (:method (p1 &optional p2) (if p2
                                 (dot (normalize p1) (normalize p2))
                                 (let ((norm (normalize p1)))
                                   (dot norm norm)))))

(defgeneric angle-sine (p1 &optional p2)
  (:method (p1 &optional p2) (let ((c (angle-cosine p1 p2)))
                               (sqrt (cl:- 1 (cl:* c c))))))

(defgeneric angle-acos (p1 &optional p2)
  (:method (p1 &optional (p2 p1)) (acos (max -1.0 (min 1.0 (dot (normalize p1) (normalize p2)))))))

(defgeneric z-alignment-angles (p)
  (:method (p) (with-dot
                 (let* ((x p.x)
                        (z p.z)
                        (x-angle (- (/ pi 2) (angle-acos p +y-axis+)))
                        (y-angle (if (and (= x 0) (= z 0))
                                     0.0
                                     (acos (/ z (sqrt (+ (* x x) (* z z))))))))
                   (values y-angle x-angle)))))

(defgeneric angle-2d (p1 &optional p2)
  (:method (p1 &optional (p2 p1)) (with-dot
                                    (let* ((theta1 (atan p1.y p1.x))
	                                   (theta2 (atan p2.y p2.x))
	                                   (dtheta (- theta2 theta1)))
                                      (loop while (> dtheta pi)
	                                    do (setf dtheta (- dtheta (* 2 pi))))
                                      (loop while (< dtheta (- pi))
	                                    do (setf dtheta (+ dtheta (* 2 pi))))
                                      dtheta))))

(defun jitter (p amount &optional random-fn excluded-channels)
  (if (= 2 (dimensions p))
      (+ p (random (type p) 1 amount amount nil :random-fn random-fn))
      (+ p (random (type p) 1 amount amount amount :random-fn random-fn))))

;;;; functions for groups of points. Does this even belong here?

(defun center (points)
  (let ((n (length points)))
    (* (reduce #'+ points) (3d (/ 1.0 n) (/ 1.0 n) (/ 1.0 n)))))


(defun in-polygon? (p points)
  (let ((angle 0.0))
    (loop :for (p1 p2) :on (append points (list (first points))) :by #'cdr :while p2
	  :do (setf angle (cl:+ angle (angle-2d (- p1 p) (- p2 p)))))
    (if (< (abs angle) pi)
	nil
	t)))

(defun bounds (points)
  (loop :with low := (copy (car points))
        :with high := (copy (car points))
        :for p :in points
        :do (nmin low p) (nmax high p)
        :finally (return (values low high))))

(defun sphericize (p radius &optional (factor 1.0) (center +origin+))
  (lerp p (* (normalize (- p center)) radius) factor))


;; (progn (setf *p3d* (point:random 1000000 1000.0 1000.0 1000.0)) (values))
;; (progn (setf *psv* (loop :repeat 1000000 :collect (3d-vectors:vec3-random 0 1000.0 ))) (values))

