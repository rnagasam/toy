# cadtoy

## intro

A small domain specific language. Meant to work with [FreeCAD](https://www.freecadweb.org/).

The primitives of this language are CAD objects - cubes (called boxes here) and cylinders. Programs evaluate
to compound shapes made from these primitives.

The parser for this language has been taken from Peter Norvig's [blog post](http://norvig.com/lispy.html)
on creating a Lisp interpreter in Python.

## language

Vectors, Cubes and Cylinders are written as
```
(vec 0 0 0) ; origin vector
(! 0 0 0)   ; alternate syntax for writing vectors
(box 1 1 1) ; unit cube, specify length, width, height
(cyl 1 10)  ; cylinder, specify radius, height
```

A vector with 4 coordinates, is a rotation vector
```
(vec 0 1 0 90) ; specify rotation about y-axis, 90 degrees
```

Cubes and cylinders can be augmented with additional information to specify position and orientation.
By default everything is placed at the origin and with no rotation.
```
(box 1 1 1 (! 10 0 0) (! 0 1 0 45)) ; unit cube, at (x=10, y=0, z=0), rotated 45 deg about y-axis
(cyl 1 10  (! 10 0 0) (! 0 1 0 45)) ; same thing for the cylinder
```

Here is the set of operations the language supports
```
(+ (box 1 1 2) (cyl 2 10)) ; add a box and cylinder. a 'join' operation
(- (box 1 1 2) (cyl 2 10)) ; subtract a cylinder from a box. a 'cut' operation. used to create holes

(move (box 1 1 1) (vec 0 0 1)) ; move the box to a new position (absolute)
(|> (box 1 1 1) (vec 0 0 1))   ; alternate syntax for move

(move-relative (box 1 1 1) (vec 0 0 1)) ; move box to a position relative to its old one
(|>> (box 1 1 1) (vec 0 0 1))           ; alternate syntax for move-relative

(rotate (box 1 1 1) (vec 0 0 1 90)) ; rotate the box (absolute)
(|# (box 1 1 1) (vec 0 0 1 90))     ; alternate syntax for rotate

(rotate-relative (box 1 1 1) (vec 0 0 1 90)) ; rotate the box (relative)
(|## (box 1 1 1) (vec 0 0 1 90))             ; alternate syntax for rotate-relative
```