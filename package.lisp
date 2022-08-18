(defpackage #:point
  (:use :cl)
  (:shadow cl:+
           cl:-
           cl:*
           cl:/
           cl:random
           cl:min
           cl:max)
  (:export #:2dpoint
	   #:3dpoint
	   #:2d
	   #:3d
	   #:sx
	   #:sy
	   #:sz

           ;; Protocol basic parts that need to be implemented by new implementations
           ;; and setf for x y z.

           #:dimensions
           #:copy
           #:element-type ;isn't anything that uses this going to be impl specific. maybe just provide a generic to create based on type.

           #:w
           #:x
	   #:y
	   #:z
           
           #:->list
	   #:->vector
	   #:->values

           ;; Protocol parts that would be nice to have for new implementations

           #:+
           #:-
           #:*
           #:/

           #:magnitude
           #:dot
           #:cross
           #:normalize

           ;; Should work with anything that provides the basics.
           #:random
           #:transform
           #:ntransform
           
           #:min
           #:max
           #:nmin
           #:nmax
           
           #:distance
           #:euclidean-sqrt ;this stupid naming style will never catch on for good reason!
           #:euclidean
           #:manhattan
           #:minkowski      ;just rename to distance?
           #:of-division

           #:angle
           ;;bevy of angle calcs from kons-9 this should be structured better.
           #:angle-cosine
           #:angle-sine
           #:angle-acos
           #:z-alignment-angles
           #:angle-2d
                    
           #:lerp
           #:==
           #:negate
           #:jitter
           
           ;; operations on groups of points that probably shouldn't be here, probably separate into required protocol, optional, and utils
           #:in-polygon?
           #:bounds
           #:sphericize

           ;; junk to be cut or refined

           ;; #:add-x
	   ;; #:add-y
	   ;; #:add-z
	   
	   ;; #:sort-x
	   ;; #:sort-y
           ;; #:radial-sort
           ;; #:<x
           ;; #:z-sort

           ;; stuff from kons-9 that probably shouldn't be here
           #:+origin+
           #:+x-axis+
           #:+y-axis+
           #:+z-axis+
           ))
