(in-package #:point)

;; Actual math with general cases that eases the burden of new implementations to get a working backend.
;; probably worth it for 2d point implementations.

(defgeneric + (p1 p2)
  (:method ((p1 3dpoint) (p2 3dpoint))           (with-dot (3d (+ p1.sx p2.sx)(+ p1.sy p2.sy)(+ p1.sz p2.sz))))
  (:method ((p1 2dpoint) (vector simple-vector)) (with-dot (2d (+ p1.x (aref vector 0))(+ p1.y (aref vector 1)))))
  (:method ((p1 2dpoint) (p2 2dpoint))           (with-dot (2d (+ p1.x p2.x)(+ p1.y p2.y))))
  (:method ((p1 vector)  (p2 vector))            (map 'vector #'cl:+ p1 p2))
  (:method ((p1 cons)    (p2 cons))              (cons (cl:+ (car p1) (car p2)) (cl:+ (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex))           (complex (cl:+ (realpart p1) (realpart p2)) (cl:+ (imagpart p1)(imagpart p2))))
  (:method (p1 (scalar number)) (let ((p (copy p1)))
                                  (incf (x p) scalar)
                                  (incf (y p) scalar)
                                  (incf (z p) scalar)
                                  p))
  (:method (p1 p2)(with-dot
                    (let ((p (copy p1)))
                      (setf p.x (+ p1.x p2.x)
                            p.y (+ p1.y p2.y))
                      (when (= 3 (dimensions p))
                        (setf p.z (+ p1.z p2.z)))
                      p))))

(defgeneric - (p1 p2)
  (:method ((p1 3dpoint) (p2 3dpoint))     (with-dot (3d (- p1.sx p2.sx)(- p1.sy p2.sy)(- p1.sz p2.sz))))
  (:method ((p1 2dpoint) (p2 2dpoint))     (with-dot (2d (- p1.sx p2.sx)(- p1.sy p2.sy))))
  (:method ((p1 2dpoint) (vector array))   (2d (cl:- (x p1) (aref vector 0))(-(y p1) (aref vector 1))))
  (:method ((p1 vector)  (p2 vector))      (map 'vector #'cl:- p1 p2))
  (:method ((p1 cons)    (p2 cons))        (cons (cl:- (car p1) (car p2)) (cl:- (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex))     (complex (cl:- (realpart p1) (realpart p2)) (cl:- (imagpart p1)(imagpart p2))))
  (:method (p1 (scalar number)) (let ((p (copy p1)))
                                  (decf (x p) scalar)
                                  (decf (y p) scalar)
                                  (decf (z p) scalar)
                                  p))
  (:method (p1 p2) (with-dot
                     (let ((p (copy p1)))
                       (setf p.x (- p1.x p2.x)
                             p.y (- p1.y p2.y))
                       (when (= 3 (dimensions p))
                         (setf p.z (- p1.z p2.z)))
                       p))))

(defgeneric * (p1 thing)
  ;;points
  (:method ((p1 3dpoint) (p2 3dpoint)) (with-dot (3d (* p1.x p2.x) (* p1.y p2.y) (* p1.z p2.z))))
  (:method ((p1 2dpoint) (p2 2dpoint)) (with-dot (2d (* p1.x p2.x) (* p1.y p2.y))))
  (:method ((p1 vector)  (p2 vector))  (map 'vector #'* p1 p2))
  (:method ((p1 cons)    (p2 cons))    (cons (cl:* (car p1) (car p2)) (cl:* (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:+ (realpart p1)p2) (cl:* (imagpart p1) p2)))
  (:method ((p1 integer) (p2 complex)) (complex (cl:* p2 p1) ))
  ;;scalars
  (:method ((p 3dpoint) (scalar number)) (with-dot (3d (* p.x scalar) (* p.y scalar) (* p.z scalar))))
  (:method ((p 2dpoint) (scalar number)) (with-dot (2d (* p.x scalar) (* p.y scalar))))
  (:method ((p vector)  (scalar number)) (map 'vector (lambda (x) (cl:* x scalar)) p))
  (:method ((p cons)    (scalar number)) (cons (cl:* (car p) scalar) (cl:* (cdr p) scalar)))
  (:method ((p complex) (scalar number)) (complex (cl:+ (realpart p)scalar) (cl:* (imagpart p) scalar)))
  (:method ((p integer) (scalar number)) (complex (cl:* scalar p) ))
  (:method (p           (scalar number)) (with-dot
                                           (let ((p (copy p)))
                                             (setf p.x (* p.x scalar)
                                                   p.y (* p.y scalar))
                                             (when (= 3 (dimensions p))
                                               (setf p.z (* p.z scalar)))
                                             p)))
  ;;hopefully combinations of valid points
  (:method (p1 p2) (with-dot
                     (let ((p (copy p1)))
                       (setf p.x (* p1.x p2.x)
                             p.y (* p1.y p2.y))
                       (when (= 3 (dimensions p))
                         (setf p.z (* p1.z p2.z)))
                       p))))

(defgeneric / (p1 thing) ;judicious allowance to divide by zero, solution for all number types? When you make up things what do you allow?
  ;;points
  (:method ((p1 3dpoint) (p2 3dpoint)) (with-dot (3d (/ p1.sx p2.sx) (/ p1.sy p2.sy) (/ p1.sz p2.sz))))
  (:method ((p1 2dpoint) (p2 2dpoint)) (with-dot (2d (/ p1.sx p2.sx) (/ p1.sy p2.sy))))
  (:method ((p1 vector)  (p2 vector))  (map 'vector #'/ p1 p2))
  (:method ((p1 cons)    (p2 cons))    (cons (cl:/ (car p1) (car p2)) (cl:/ (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:/ (realpart p1) (realpart p2)) (cl:/ (imagpart p1) (imagpart p2))))
  (:method ((p1 integer) (p2 complex)) (complex (cl:/ p2 p1)))
  ;;scalars
  (:method ((p 3dpoint) (scalar number)) (with-dot (3d (/ p.x scalar) (/ p.y scalar) (/ p.z scalar))))
  (:method ((p 2dpoint) (scalar number)) (with-dot (2d (/ p.x scalar) (/ p.y scalar))))
  (:method ((p vector)  (scalar number)) (map 'vector (lambda (x) (cl:/ x scalar)) p))
  (:method ((p cons)    (scalar number)) (cons (cl:/ (car p) scalar) (cl:/ (cdr p) scalar)))
  (:method ((p complex) (scalar number)) (complex (cl:/ (realpart p)scalar) (cl:/ (imagpart p) scalar)))
  (:method ((p number)  (scalar number)) (complex (cl:/ (realpart p)scalar) 0)) ;assume numbers are complex numbers with a 0 imaginary part.  What could go wrong?
  ;;hopefully combinations of valid points
  (:method (p (scalar number)) (with-dot
                                 (let ((p (copy p)))
                                   (setf p.x (/ p.x scalar)
                                         p.y (/ p.y scalar))
                                   (when (= 3 (dimensions p))
                                     (setf p.z (/ p.z scalar)))
                                   (when (= 4 (dimensions p))
                                     (setf p.w (/ p.w scalar)))
                                   p)))
  (:method (p1 p2)(with-dot
                    (let ((p (copy p1)))
                      (setf p.x (/ p1.x p2.x)
                            p.y (/ p1.y p2.y))
                      (when (= 3 (dimensions p))
                        (setf p.z (/ p1.z p2.z)))
                      p))))

(defgeneric min (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (3d (min p1.sx p2.sx) (min p1.sy p2.sy) (min p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (2d (min p1.sx p2.sx) (min p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector))  (map 'vector #'cl:min p1 p2))
  (:method ((p1 cons)   (p2 cons))    (with-dot (cons (min (car p1) (car p2)) (min (cdr p1) (cdr p2)))))
  (:method ((p1 complex)(p2 complex)) (with-dot (complex (min (realpart p1) (realpart p2)) (min (imagpart p1) (imagpart p2)))))
  (:method (p1 p2)                    (with-dot  (let ((p (copy p1)))
                                                   (setf p.x (min p1.x p2.x)
                                                         p.y (min p1.y p2.y))))))

(defgeneric max (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (3d (max p1.sx p2.sx) (max p1.sy p2.sy) (max p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (2d (max p1.sx p2.sx) (max p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector))  (map 'vector #'cl:max p1 p2))
  (:method ((p1 cons)   (p2 cons))    (with-dot (cons (max (car p1) (car p2)) (max (cdr p1) (cdr p2)))))
  (:method ((p1 complex)(p2 complex)) (with-dot (complex (max (realpart p1) (realpart p2)) (max (imagpart p1) (imagpart p2)))))
  (:method (p1 p2)                    (with-dot  (let ((p (copy p1)))
                                                   (setf p.x (max p1.x p2.x)
                                                         p.y (max p1.y p2.y))))))

;;non consing versions always store result in first point

(defgeneric nmin (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (setf p1.sx (min p1.sx p2.sx)
                                                      p1.sy (min p1.sy p2.sy)
                                                      p1.sz (min p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (setf p1.sx (min p1.sx p2.sx)
                                                      p1.sy (min p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector))  (map-into p1 #'cl:min p1 p2))
  (:method ((p1 cons)   (p2 cons))    (with-dot (rplaca p1 (min (car p1) (car p2)))
                                        (rplacd p1 (min (cdr p1) (cdr p2)))))
  (:method (p1 p2)                    (with-dot (setf p1.x (min p1.x p2.x)
                                                      p1.y (min p1.y p2.y)))))
  ;;(:method ((p1 complex)(p2 complex)))) why can't we set realpart?
(defgeneric nmax (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (setf p1.sx (max p1.sx p2.sx)
                                                      p1.sy (max p1.sy p2.sy)
                                                      p1.sz (max p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (setf p1.sx (max p1.sx p2.sx)
                                                      p1.sy (max p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector)) (map-into p1 #'cl:max p1 p2))
  (:method ((p1 cons)(p2 cons)) (with-dot
                                  (rplaca p1 (max (car p1) (car p2)))
                                  (rplacd p1 (max (cdr p1) (cdr p2)))))
  ;;(:method ((p1 complex)(p2 complex)))) why can't we set realpart?
  (:method (p1 p2) (with-dot (setf p1.x (max p1.x p2.x)
                                   p1.y (max p1.y p2.y)))))

;; In need? of specific methods, but not necessary cause who needs performance?
(defgeneric dot (p1 p2)
  (:method (p1 p2) (reduce #'cl:+ (mapcar #'cl:* (->list p1) (->list p2)))))

(defgeneric magnitude (p)
  (:method (p)(sqrt (dot p p))))

(defgeneric cross (a b)
  (:method (a b) (with-dot(3d (- (* a.y b.z) (* a.z b.y))
                              (- (* a.z b.x) (* a.x b.z))
                              (- (* a.x b.y) (* a.y b.x))))))

(defun average (&rest points)
  (let ((div (cl:/ 1.0 (length points))))
    (* (reduce #'+ points) (new (type (car points))div div div div))))
