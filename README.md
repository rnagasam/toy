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

(scale (box 1 1 1) 5) ; create (box 5 5 5)
(^^ (box 1 1 1) 5)    ; alternate syntax for scale
```

## sample programs

```
(box 1 1 1)
```

![unitbox](https://github.com/bluerama/cadtoy/blob/master/img/unitbox.png)

```
(cyl 1 1)
```

![unitcyl](https://github.com/bluerama/cadtoy/blob/master/img/unitcyl.png)

```
(define (l-bracket size thickness)
  (- (scale unitbox size)
     (box (- size thickness)
          (- size thickness)
	  size)))

(l-bracket 100 15)
```

![l-bracket](https://github.com/bluerama/cadtoy/blob/master/img/l-bracket.png)


```
(define (plate-with-hole size thickness radius)
  (- (box size size thickness)
     (move (cyl radius thickness)
           (vec (/ size 2) (/ size 2) 0))))
```

![plate-with-hold](https://github.com/bluerama/cadtoy/blob/master/img/pwh.png)


There is support for recursive functions

```
(define (recurse num)
  (if (> num 1)
      (+ unitbox (|> unitbox (! num num num))
         (recurse (- num 1)))
      unitbox))
```

![recurse](https://github.com/bluerama/cadtoy/blob/master/img/recurse1.png)


Sometimes, we get shapes like this...

![weird](https://github.com/bluerama/cadtoy/blob/master/img/weird2.png)



## is this language useful?

No.

## running

It is quite a pain to run all of this at this point in time. FreeCAD is used to render
the CAD parts, but the language itself doesn't depend on it. Parts are created using
the `create` function in `interpreter.py`. One would have to start a FreeCAD session
and import `interpreter` and `lparser` and then write programs in FreeCAD's python
console.

