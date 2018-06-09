import sys
sys.path.append('/usr/lib/freecad-daily/lib')

from collections import namedtuple

####################
# LANG DEF
####################

# Primitives
Unit = () # None
Bool = namedtuple("Bool", "val")
Vec  = namedtuple("Vector", "x y z")
RVec = namedtuple("RVector", "x y z a") # a is in degrees

Box = namedtuple("Box", "length width height position rotation")
Box.__new__.__defaults__ = (Vec(0,0,0),RVec(0,0,0,0))

Cyl = namedtuple("Cylinder", "radius height position rotation")
Cyl.__new__.__defaults__ = (Vec(0,0,0),RVec(0,0,0,0))

Cmpd = namedtuple("Cmpd", "operator part1 part2")

# Operations on Parts
Add    = namedtuple("Add", "exp1 exp2")
Sub    = namedtuple("Sub", "exp1 exp2")
Mul    = namedtuple("Mul", "exp1 exp2")
Div    = namedtuple("Div", "exp1 exp2")
Scl    = namedtuple("Scl", "exp1 exp2")
Mov    = namedtuple("Mov", "exp1 exp2")
Movr   = namedtuple("Movr", "exp1 exp2")
Rot    = namedtuple("Rot", "exp1 exp2")
Rotr   = namedtuple("Rotr", "exp1 exp2")
Select = namedtuple("Select", "exp1 exp2")

# Language constructs
Var     = namedtuple("Var", "name")
Let     = namedtuple("Let", "var value exp")
If      = namedtuple("If", "exp1 exp2 exp3")
Eq      = namedtuple("Eq", "exp1 exp2")
Gt      = namedtuple("Gt", "exp1 exp2")
Lt      = namedtuple("Lt", "exp1 exp2")
Cons    = namedtuple("Cons", "fst snd")
Car     = namedtuple("Car", "pair")
Cdr     = namedtuple("Cdr", "pair")
Fun     = namedtuple("Fun", "name formal body")
Closure = namedtuple("Closure", "fun env")
Call    = namedtuple("Call", "funexp arg")

ATOMS = [Box, Cyl, Cmpd]


####################
# MACROS
####################

# List: a 'macro' for creating Cons lists
def List(*vs):
    if vs == ():
        return Unit
    return Cons(vs[0], List(*(vs[1:])))

# MCall: a 'macro' for calling a curried function with a list
def MCall(f, *xs):
    if xs == ():
        return f
    return MCall(Call(f, xs[0]), *(xs[1:]))

# MFun: a 'macro' for writing functions with multiple arguments
def MFun(fname, *args):
    fbody = args[-1]
    def helper(ps,inner):
        if ps == ():
            return fbody
        return Fun(fname+inner,ps[0],helper(ps[1:],'_'+inner))
    return helper(args[:-1],'')

def MFunner(fname,fbody,*args):
    def helper(ps,inner):
        if ps == ():
            return fbody
        return Fun(fname+inner, ps[0], helper(ps[1:],'_'+inner))
    return helper(args,'')

####################
# LANG EVALUATOR
####################

def evaluate(exp, env={}):
    if exp is Unit:
        return exp

    # don't have to specify 'Var' each time
    elif type(exp) is str:
        return evaluate(Var(exp), env)
    
    elif type(exp) in [int, float]:
        return exp
    
    elif type(exp) is Var:
        if exp.name in env.keys():
            # print "ENV: ", env
            return env[exp.name]
        else:
            print "ERROR!!!"
            print "EXP NAME: ", exp.name
            print "ENV: ", env
            # raise Exception("Unknown Variable")
            return "Unknown variable"

    elif type(exp) is Vec:
        x = evaluate(exp.x, env)
        y = evaluate(exp.y, env)
        z = evaluate(exp.z, env)
        return Vec(x,y,z)

    elif type(exp) is RVec:
        x = evaluate(exp.x, env)
        y = evaluate(exp.y, env)
        z = evaluate(exp.z, env)
        a = evaluate(exp.a, env)
        return RVec(x, y, z, a)
    
    elif type(exp) is Box:
        length = evaluate(exp.length, env)
        width = evaluate(exp.width, env)
        height = evaluate(exp.height, env)
        position = evaluate(exp.position, env)
        rotation = evaluate(exp.rotation, env)
        return Box(length, width, height, position, rotation)
    
    elif type(exp) is Cyl:
        radius = evaluate(exp.radius, env)
        height = evaluate(exp.height, env)
        position = evaluate(exp.position, env)
        rotation = evaluate(exp.rotation, env)
        return Cyl(radius, height, position, rotation)

    elif type(exp) is Cmpd:
        # not meant to be used outside evaluator
        return exp

    elif type(exp) is Add:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        if type(val1) in [int, float] and type(val2) in [int, float]:
            return val1 + val2
        if type(val1) is Vec and type(val2) is Vec:
            return Vec(*tuple(map(lambda x: sum(x), zip(val1, val2))))
        if type(val1) is RVec and type(val2) is RVec:
            return RVec(*tuple(map(lambda x: sum(x), zip(val1, val2))))
        if (val1) in [Unit, None]:
            return val2
        if (val2) in [Unit, None]:
            return val1
        if type(val1) in ATOMS and type(val2) in ATOMS:
            return Cmpd('+', val1, val2)
        else:
            print "Invalid operands -- ADD: ", (val1, val2), env

    elif type(exp) is Sub:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        if type(val1) in [int, float] and type(val2) in [int, float]:
            return val1 - val2
        if type(val1) is Vec and type(val2) is Vec:
            return Vec(*tuple(map(lambda x: x[0]-x[1], zip(val1, val2))))
        if type(val1) is RVec and type(val2) is RVec:
            return RVec(*tuple(map(lambda x: x[0]-x[1], zip(val1, val2))))
        if val1 in [Unit, None]:
            return val2
        if val2 in [Unit, None]:
            return val1
        if type(val1) in ATOMS and type(val2) in ATOMS:
            return Cmpd('-', val1, val2)
        else:
            print "Invalid operands -- SUB: ", (val1, val2), env

    elif type(exp) is Mul:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        if type(val1) in [int, float] and type(val2) in [int, float]:
            return val1 * val2
        print "Illegal operands -- MUL: ", (val1, val2), env

    elif type(exp) is Div:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        if type(val1) in [int, float] and type(val2) in [int, float]:
            return val1 / val2
        print "Illegal operands -- DIV: ", (val1, val2), env
    
    elif type(exp) is Mov:
        shape = evaluate(exp.exp1, env)
        pos = evaluate(exp.exp2, env)
        if type(pos) is not Vec:
            print "Non-vector -- MOV: ", pos
            return
        if type(shape) is Box:
            return Box(shape.length, shape.width, shape.height,
                    pos, shape.rotation)
        elif type(shape) is Cyl:
            return Cyl(shape.radius, shape.height, pos, shape.rotation)
        elif type(shape) is Cmpd:
            return Cmpd(shape.operator,
                        evaluate(Mov(shape.part1, pos), env),
                        evaluate(Mov(shape.part2, pos), env))
        else:
            print "Invalid operands -- MOV: ", (shape, pos)

    elif type(exp) is Movr:
        shape = evaluate(exp.exp1, env)
        pos = evaluate(exp.exp2, env)
        if type(pos) is not Vec:
            print "Non-vector -- MOVR: ", pos
            return
        if type(shape) is Box:
            newpos = evaluate(Add(shape.position, pos))
            return Box(shape.length, shape.width, shape.height,
                    newpos, shape.rotation)
        elif type(shape) is Cyl:
            newpos = evaluate(Add(shape.position, pos))
            return Cyl(shape.radius, shape.height,
                    newpos, shape.rotation)
        elif type(shape) is Cmpd:
            return Cmpd(shape.operator,
                        evaluate(Movr(shape.part1, pos), env),
                        evaluate(Movr(shape.part2, pos), env))
        else:
            print "Invalid operands -- MOVR: ", (shape, pos)

    elif type(exp) is Rot:
        shape = evaluate(exp.exp1, env) # gives shape
        rot = evaluate(exp.exp2, env)   # gives rotation vector
        if type(rot) is not RVec:
            print "Non-rotation-vector -- ROT: ", rot
            return
        if type(shape) is Box:
            return Box(shape.length, shape.width, shape.height,
                    shape.position, rot)
        elif type(shape) is Cyl:
            return Cyl(shape.radius, shape.height,
                    shape.position, rot)
        elif type(shape) is Cmpd:
            return Cmpd(shape.operator,
                        evaluate(Rot(shape.part1, rot), env),
                        evaluate(Rot(shape.part2, rot), env))
        else:
            print "Invalid operands -- ROT: ", (shape, rot)

    elif type(exp) is Rotr:
        shape = evaluate(exp.exp1, env)
        rot = evaluate(exp.exp2, env)
        if type(rot) is not RVec:
            print "Non-rotation-vector -- ROTR: ", rot
            return
        if type(shape) is Box:
            newrot = evaluate(Add(shape.rotation, rot))
            return Box(shape.length, shape.width, shape.height,
                    shape.position, newrot)
        elif type(shape) is Cyl:
            newrot = evaluate(Add(shape.rotation, rot))
            return Cyl(shape.radius, shape.height, shape.position, newrot)
        elif type(shape) is Cmpd:
            return Cmpd(shape.operator,
                        evaluate(Rotr(shape.part1, rot), env),
                        evaluate(Rotr(shape.part2, rot), env))
        else:
            print "Invalid operands -- ROTR: ", (shape, rot)

    elif type(exp) is Scl:
        part = evaluate(exp.exp1, env)
        scale = evaluate(exp.exp2, env)
        if type(part) is Box:
            return Box(part.length * scale,
                       part.width * scale,
                       part.height * scale,
                       part.position, part.rotation)
        elif type(part) is Cyl:
            return Cyl(part.radius * scale,
                       part.height * scale,
                       part.position, part.rotation)
        elif type(part) is Cmpd:
            return Cmpd(part.operator,
                        evaluate(Scl(part.part1, scale), env),
                        evaluate(Scl(part.part2, scale), env))
        else:
            print "Invalid operands -- SCL: ", (part, scale)
    
    elif type(exp) is Select:
        val = evaluate(exp.exp1, env)
        prop = str(exp.exp2)
        return val.__dict__[prop]
    
    elif type(exp) is Let:
        newenv = env.copy()
        newenv[exp.var] = evaluate(exp.value, env)
        # newenv[exp.var.name] = evaluate(exp.value, env)
        return evaluate(exp.exp, newenv)
    
    elif type(exp) is Eq: # = 
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        return Bool(val1 == val2)
    
    elif type(exp) is Gt:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        return Bool(val1 > val2)
    
    elif type(exp) is Lt:
        val1 = evaluate(exp.exp1, env)
        val2 = evaluate(exp.exp2, env)
        return Bool(val1 < val2)
    
    elif type(exp) is If:
        pred = evaluate(exp.exp1, env)
        if pred.val:
            return evaluate(exp.exp2, env)
        return evaluate(exp.exp3, env)
    
    elif type(exp) is Fun: # A function evaluates to a closure
        return Closure(exp, env.copy())
    
    elif type(exp) is Closure: # A closure evaluates to itself
        return exp
    
    elif type(exp) is Call:
        actual = evaluate(exp.arg, env)
        closure = evaluate(exp.funexp, env)
        if type(closure) is Closure:
            fun = closure.fun
            env.update(closure.env) # new addition
            newenv = env.copy()     # new addition
            # newenv = closure.env.copy()
            newenv[fun.name] = closure
            newenv[fun.formal] = actual
            return evaluate(fun.body, newenv)
        else:
            print "Non-function -- CALL: ", closure
            return

    elif type(exp) is Cons:
        head = evaluate(exp.fst, env)
        tail = evaluate(exp.snd, env)
        return Cons(head, tail)
    
    elif type(exp) is Car:
        if type(exp.pair) is Cons:
            return exp.pair.fst
        else:
            print "Non-pair -- CAR: ", exp
            return
    
    elif type(exp) is Cdr:
        if type(exp.pair) is Cons:
            return exp.pair.snd
        else:
            print "Non-pair -- CDR: ", exp
            return
    
    else:
        return "Unknown expression"


####################
# FREECAD EVALUATOR
####################

def create(exp):
    """Create a FreeCAD part from a given expression"""
    import FreeCAD, Part
    if type(exp) is Box:
        vec = FreeCAD.Vector(exp.position.x, exp.position.y,
                exp.position.z)
        if (exp.rotation.x, exp.rotation.y, exp.rotation.z) == (0,0,0):
            rotax = FreeCAD.Vector(0,0,1)
        else:
            rotax = FreeCAD.Vector(exp.rotation.x, exp.rotation.y,
                    exp.rotation.z)
        # p = Part.makeBox(exp.length, exp.width, exp.height, vec)
        p = Part.makeBox(exp.length, exp.width, exp.height)
        p.translate(vec)
        p.rotate(vec,rotax,exp.rotation.a)
        # p.rotate(FreeCAD.Vector(0,0,0),rotax,exp.rotation.a)
        return p
    elif type(exp) is Cyl:
        vec = FreeCAD.Vector(exp.position.x, exp.position.y,
                exp.position.z)
        if (exp.rotation.x, exp.rotation.y, exp.rotation.z) == (0,0,0):
            rotax = FreeCAD.Vector(0,0,1)
        else:
            rotax = FreeCAD.Vector(exp.rotation.x, exp.rotation.y,
                    exp.rotation.z)
        # p = Part.makeCylinder(exp.radius, exp.height, vec)
        p = Part.makeCylinder(exp.radius, exp.height)
        p.translate(vec)
        p.rotate(vec,rotax,exp.rotation.a)
        # p.rotate(FreeCAD.Vector(0,0,0),rotax,exp.rotation.a)
        return p
    elif type(exp) is Cmpd:
        if exp.operator is '-':
            return create(exp.part1).cut(create(exp.part2))
        elif exp.operator is '+':
            return create(exp.part1).fuse(create(exp.part2))
    else:
        print "Illegal argument to create: ", exp


####################
# HELPER FUNCTIONS
####################

E = evaluate

def CE(exp):
    return create(evaluate(exp))

def P(exp):
    import FreeCAD, Part
    return Part.show(CE(exp))

##################
# TEST FUNCTIONS
##################

# sample objects
UnitBox = Box(1,1,1)
UnitCyl = Cyl(1,1)

unitScl = Fun('unitScl','x',Scl(UnitBox,'x'))
subNums = MFun('subNums','x','y',Sub('x','y'))
addNums = MFun('addNums','x','y',Add('x','y'))
makeVec = Fun('makeVec','x',Vec('x','x','x'))

# Recursion test
recurse = Fun("test","x",
        If(Gt(Var("x"),0),
            (Add(Mov(UnitBox,Vec(Var("x"),Var("x"),Var("x"))),
                Call(Var("test"),Sub(Var("x"),1)))),
            UnitBox))

recurse1 = Fun("test","x",
        If(Gt(Var("x"),0),
            (Add(Mov(UnitBox,Vec(Var("x"),Var("x"),Var("x"))),
                Call(Var("test"),Sub(Var("x"),1)))),
            Unit))

L = Fun('L', 'x',
        Fun('_', 't', Sub(Scl(UnitBox,'x'),
                          Box(Sub('x','t'),
                              Sub('x','t'),
                              'x'))))

# Lbracket
L1 = MFun('L1','x','t',
        Sub(Scl(UnitBox,'x'),
            Box(Sub('x','t'),
                Sub('x','t'),
                'x')))

# Plate with Hole
PwH = MFun('PwH','Pd','Pt','Hr',
        Sub(Box('Pd','Pd','Pt'),
            Mov(Cyl('Hr','Pt'),Vec(Div('Pd',2),Div('Pd',2),0))))

# Plate with 2 holes
Pw2H = MFun('Pw2H','Pd','Pt','Hr',
        Add(MCall(PwH,'Pd','Pt','Hr'),
            Movr(MCall(PwH,'Pd','Pt','Hr'),
                Vec('Pd',0,0))))

# Plate with 2 holes using Let
Pw2HL = MFun('Pw2HL','Pd','Pt','Hr',
        Let('p',MCall(PwH,'Pd','Pt','Hr'),
            Add('p',Movr('p',Vec('Pd',0,0)))))

# Plate with 4 holes
Pw4H = MFun('Pw4H','Pd','Pt','Hr',
        Let('p',MCall(Pw2HL,'Pd','Pt','Hr'),
            Add('p',Movr('p',Vec(0,'Pd',0)))))

LwH = MFun('LwH','Pd','Pt','Hr',
           Let('p',MCall(Pw4H,'Pd','Pt','Hr'),
               Add('p',Rotr('p',RVec(0,1,0,90)))))

# Binary Tree ? Right now, just a 'V' shape
BT = MFun('BT','h','p','r',
        Add(Rot(Mov(Cyl(1,'h'),'p'),RVec(0,1,0,Sub(60,Mul('r',60)))),
            Rot(Mov(Cyl(1,'h'),'p'),RVec(0,1,0,Sub(150,Mul('r',60))))))


 
