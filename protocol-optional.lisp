(in-package #:point)

;; Actual math with general cases that eases the burden of new implementations to get a working backend.
;; probably worth it for 2d point implementations.

(defgeneric + (p1 p2)
  (:method ((p1 3dpoint) (p2 3dpoint)) (with-dot (3d (+ p1.sx p2.sx)(+ p1.sy p2.sy)(+ p1.sz p2.sz))))
  (:method ((p1 2dpoint) (vector array)) (with-dot (2d (+ p1.x (aref vector 0))(+ p1.y (aref vector 1)))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (2d (+ p1.x p2.x)(+ p1.y p2.y))))
  (:method ((p1 vector) (p2 vector)) (map 'vector #'cl:+ p1 p2))
  (:method ((p1 cons) (p2 cons)) (cons (cl:+ (car p1) (car p2)) (cl:+ (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:+ (realpart p1) (realpart p2)) (cl:+ (imagpart p1)(imagpart p2))))
  (:method (p1 p2) (with-dot
                     (let ((p (copy p1)))
                       (setf p.x (+ p1.x p2.x)
                             p.y (+ p1.y p2.y)
                             p.z (+ p1.z p2.z) )))))

(defgeneric - (p1 p2)
  (:method ((p1 3dpoint) (p2 3dpoint))(3d (cl:- (sx p1)(sx p2))(cl:- (sy p1)(sy p2))(cl:- (sz p1)(sz p2))))
  (:method ((p1 2dpoint) (vector array))(point:2d (cl:- (x p1) (aref vector 0))(-(y p1) (aref vector 1))))
  (:method ((p1 2dpoint)(p2 2dpoint))(point:2d (cl:- (sx p1) (sx p2))(cl:-(sy p1) (sy p2))))
  (:method ((p1 vector) (p2 vector)) (map 'vector #'cl:- p1 p2))
  (:method ((p1 cons) (p2 cons)) (cons (cl:- (car p1) (car p2)) (cl:- (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:- (realpart p1) (realpart p2)) (cl:- (imagpart p1)(imagpart p2))))
  (:method (p1 p2) (with-dot
                     (let ((p (copy p1)))
                       (setf p.x (- p1.x p2.x)
                             p.y (- p1.y p2.y)
                             p.z (- p1.z p2.z) )))))

(defgeneric * (p1 thing)
  ;;points
  (:method ((p1 3dpoint) (p2 3dpoint)) (with-dot (3d (* p1.x p2.x) (* p1.y p2.y) (* p1.z p2.z))))
  (:method ((p1 vector) (p2 vector)) (map 'vector #'* p1 p2))
  (:method ((p1 2dpoint) (p2 2dpoint)) (with-dot (2d (* p1.x p2.x) (* p1.y p2.y))))
  (:method ((p1 cons)(p2 cons)) (cons (cl:* (car p1) (car p2)) (cl:* (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:+ (realpart p1)p2) (cl:* (imagpart p1) p2)))
  (:method ((p1 integer) (p2 complex)) (complex (cl:* p2 p1) ))
  ;;scalars
  (:method ((p1 3dpoint) scalar)(3d (cl:* (x p1) scalar) (cl:* (y p1) scalar) (cl:* (z p1) scalar)))
  (:method ((p1 vector) scalar) (map 'vector (lambda (x) (cl:* x scalar)) p1))
  (:method ((p1 2dpoint)scalar) (2d (cl:* (x p1) scalar) (cl:* (y p1) scalar)))
  (:method ((p1 cons) scalar) (cons (cl:* (car p1) scalar) (cl:* (cdr p1) scalar)))
  (:method ((p1 complex) scalar) (complex (cl:+ (realpart p1)scalar) (cl:* (imagpart p1) scalar)))
  (:method ((p1 integer) scalar) (complex (cl:* scalar p1) ))
  (:method (p (scalar number)) ())
  ;;hopefully combinations of valid points
  (:method (p1 p2) (with-dot
                     (let ((p (copy p1)))
                       (setf p.x (* p1.x p2.x)
                             p.y (* p1.y p2.y)
                             p.z (* p1.z p2.z))))))

(defgeneric / (p1 thing)  ;judicious allowance to divide by zero, solution for all number types? When you make up things what do you allow?
  ;;points
  (:method ((p1 3dpoint) (p2 3dpoint)) (with-dot (3d (/ p1.x p2.x) (/ p1.y p2.y) (/ p1.z p2.z))))
  (:method ((p1 2dpoint) (p2 2dpoint)) (with-dot (2d (/ p1.x p2.x) (/ p1.y p2.y))))
  (:method ((p1 vector)  (p2 vector))  (map 'vector #'/ p1 p2))
  (:method ((p1 cons)    (p2 cons))    (cons (cl:/ (car p1) (car p2)) (cl:/ (cdr p1) (cdr p2))))
  (:method ((p1 complex) (p2 complex)) (complex (cl:+ (realpart p1)p2) (cl:/ (imagpart p1) p2)))
  (:method ((p1 integer) (p2 complex)) (complex (cl:/ p2 p1)))
  ;;scalars
  (:method ((p1 3dpoint) scalar) (with-dot (3d (/ p1.x scalar) (/ p1.y scalar) (/ p1.z scalar))))
  (:method ((p1 2dpoint) scalar) (with-dot (2d (/ p1.x scalar) (/ p1.y scalar))))
  (:method ((p1 vector)  scalar) (map 'vector (lambda (x) (cl:/ x scalar)) p1))
  (:method ((p1 cons)    scalar) (cons (cl:/ (car p1) scalar) (cl:/ (cdr p1) scalar)))
  (:method ((p1 complex) scalar) (complex (cl:/ (realpart p1)scalar) (cl:/ (imagpart p1) scalar)))
  (:method ((p1 number)  scalar) (complex (cl:/ (realpart p1)scalar) 0));assume numbers are complex numbers with a 0 imaginary part.  What could go wrong?
  ;;hopefully combinations of valid points
  (:method (p1 p2)(with-dot
                     (let ((p (copy p1)))
                       (setf p.x (/ p1.x p2.x)
                             p.y (/ p1.y p2.y)
                             p.z (/ p1.z p2.z))))))

(defgeneric min (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (3d (min p1.sx p2.sx) (min p1.sy p2.sy) (min p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (2d (min p1.sx p2.sx) (min p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector)) (map 'vector #'cl:min p1 p2))
  (:method ((p1 cons)(p2 cons)) (with-dot (cons (min (car p1) (car p2)) (min (cdr p1) (cdr p2)))))
  (:method ((p1 complex)(p2 complex)) (with-dot (complex (min (realpart p1) (realpart p2)) (min (imagpart p1) (imagpart p2)))))
  (:method (p1 p2) (with-dot (2d (min p1.x p2.x) (min p1.y p2.y)))))

(defgeneric max (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (3d (max p1.sx p2.sx) (max p1.sy p2.sy) (max p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (2d (max p1.sx p2.sx) (max p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector)) (map 'vector #'cl:max p1 p2))
  (:method ((p1 cons)(p2 cons)) (with-dot (cons (max (car p1) (car p2)) (max (cdr p1) (cdr p2)))))
  (:method ((p1 complex)(p2 complex)) (with-dot (complex (max (realpart p1) (realpart p2)) (max (imagpart p1) (imagpart p2)))))
  (:method (p1 p2) (with-dot (2d (max p1.x p2.x) (max p1.y p2.y)))))

;;non consing versions always store result in first point

(defgeneric nmin (p1 p2)
  (:method ((p1 3dpoint)(p2 3dpoint)) (with-dot (setf p1.sx (min p1.sx p2.sx)
                                                      p1.sy (min p1.sy p2.sy)
                                                      p1.sz (min p1.sz p2.sz))))
  (:method ((p1 2dpoint)(p2 2dpoint)) (with-dot (setf p1.sx (min p1.sx p2.sx)
                                                      p1.sy (min p1.sy p2.sy))))
  (:method ((p1 vector) (p2 vector)) (map-into p1 #'cl:min p1 p2))
  (:method ((p1 cons)(p2 cons)) (with-dot (rplaca p1 (min (car p1) (car p2)))
                                  (rplacd p1 (min (cdr p1) (cdr p2)))))
;;(:method ((p1 complex)(p2 complex)))) why can't we set realpart?
  (:method (p1 p2) (with-dot (setf p1.x (min p1.x p2.x)
                                   p1.y (min p1.y p2.y)))))
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
