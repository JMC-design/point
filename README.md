# point
Generic point protocol for 2d and 3d points. 
rationale:
I've seen too many people start a graphics project and immediately write some throw away point interface, usually replaced in needs of speed with something less flexible. I like flexibility. I like my points not always having to be floats.  Maybe I want rationals?
I like being able to not have to choose, i.e. prematurely optimize and limit, right away how my points will be represented. It is also useful for AoC which was my main use until recently.  It's sort of necessary now to allow my graphics libraries to accept most point formats you can throw a stick at.

Currently supported point representations are 2d: cons, complex, point:2dpoint, vector 3d: point:3dpoint, vector and in a separate point-3d-vectors package support for Shinmera's Vec2, Vec3, and Vec4 formats. 

All lesser dimension points are assumed to lie at 0 on higher planes. 
Any combination of different point formats will return a point in the same format as the left most point. <-rethink this, while easily specified and implemented, if a 2d point is assumed to lie at 0 in 3d, subtracting or adding a 3d point should shift the 2d point into 3d.
examples.

```
LIFE> (3d-vectors:vec2 10 20)
(3D-VECTORS:VEC2 10.0 20.0)

LIFE> (point:x *)
10.0

LIFE> (point:* ** (cons 2 2))
(3D-VECTORS:VEC2 20.0 40.0)

LIFE> (point:/ * 4)
(3D-VECTORS:VEC2 5.0 10.0)

LIFE> (point:- * (complex 1 1))
(3D-VECTORS:VEC2 4.0 9.0)

LIFE> (point:+ (point:3d -4 -9 0) *)
P (0.0,0.0,0)
LIFE> (point:euclidean * (cons 4 3))
5.0
LIFE> (point:manhattan ** (cons 4 3))
7.0
LIFE> (point:minkowski *** (cons 4 3) 2)
5.0
LIFE> (point:== (cons 10 10) (3d-vectors:vec2 10 10))
T
