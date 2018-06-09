from interpreter import *

# Sample Objects
################

UnitBox = Box(1,1,1)
UnitCyl = Cyl(1,1)

# Sample Functions
##################

# add numbers
addNums = MFun('addNums','x','y',Add('x','y'))

# subtract numbers
subNums = MFun('subNums','x','y',Sub('x','y'))

# scale UnitBox by x
unitScl = Fun('unitScl','x',Scl(UnitBox,'x'))

# make vectors
makeVec  = MFun('makeVec','x','y','z',Vec('x','y','z'))
#makeRVec = MFun('makeRVec','x','y','z','a'
#                RVec('x','y','z','a'))

# recursion test -> offset cubes
OC = Fun('OC','x',
          If(Gt('x',0),
             Add(Mov(UnitBox,Vec('x','x','x')),
                 Call('OC',Sub('x',1))),
             Unit))

# simple L bracket, specify cube size and thickness
LB = MFun('L','x','t',
          Sub(Scl(UnitBox,'x'),
              Box(Sub('x','t'),
                  Sub('x','t'),
                  Sub('x','t'))))

# a plate with a hole in the center
PwH = MFun('PwH','Pd','Pt','Hr',
           Sub(Box('Pd','Pd','Pt'),
               Mov(Cyl('Hr','Pt'),Vec(Div('Pd',2),Div('Pd',2),0))))


# With parser included. Language has Lisp like syntax
oc = '(define (oc x) (if (> x 0) (+ (|> unitbox (vec 0 number number)) (oc (- x 1))) unitbox))'

oc1 = '''(let (oc (lambda (x)
      	   (if (> x 0)
	       (+ (|> unitbox (vec 0 0 x))
		  (oc (- x 1)))
	     unitbox)))
      (oc 5))'''

oc2 = '''(let (recurse (lambda (x)
                       (if (> x 0)
                           (+ (|# unitcyl (vec 0 (/ x 2) 1 90))
                              (recurse (- x 1)))
                           unitcyl)))
              (recurse 5))'''

oc25 = '''(let (recurse (lambda (x)
                       (if (> x 0)
                           (+ (|# unitcyl (vec (/ x 5) (/ x 2) 1 90))
                              (recurse (- x 1)))
                           unitcyl)))
              (recurse 10))'''


oc3 = '''(let (recurse (lambda (x)
                       (if (> x 0)
                           (+ (+ (^^ unitcyl x) (|>> unitbox (vec 0 0 1)))
                              (recurse (- x 1)))
                           unitcyl)))
              (recurse 5))'''

oc4 = '''(let (recurse (lambda (x)
                       (if (> x 0)
                           (+ (+ (^^ unitcyl 0.5) (|>> unitbox (vec 0 0 1)))
                              (recurse (- x 1)))
                           unitcyl)))
              (recurse 5))'''


oc5 = '''(let (recurse (lambda (x)
                       (if (> x 0)
                           (+ (|>> unitcyl (vec (/ x 5) (/ x 2) 1))
                              (recurse (- x 1)))
                           unitcyl)))
              (recurse 5))'''

bt = '''(let (recurse (lambda (x)
                      (if (> x 0)
                          (+ (|# (cyl 1 50) (vec 0 0 1 90))
                             (recurse (- x 1)))
                          (|# (cyl 1 50) (vec 1 0 0 90)))))
             (recurse 5))'''
