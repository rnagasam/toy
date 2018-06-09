# Lisp like syntax for this language

from interpreter import * # require language definitions

# from http://norvig.com/lispy.html

def tokenize(chars):
    return chars.replace('(',' ( ').replace(')',' ) ').split()

def read_from_tokens(tokens):
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF')
    token = tokens.pop(0)
    if token == '(':
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0)
        return L
    elif token == ')':
        raise SyntaxError('unexpected )')
    else:
        return token

def Parse(program):
    return read_from_tokens(tokenize(program))

def AST(prog):
    # prog = read_from_tokens(tokenize(program))
    if type(prog) is str:
        if prog == 'unitbox':
            return UnitBox
        elif prog == 'unitcyl':
            return UnitCyl
        elif prog == 'true' or prog == '#t':
            return Bool(True)
        elif prog == 'false' or prog == '#f':
            return Bool(False)
        else: # is a number
            try:
                return float(prog)
            except ValueError:
                return Var(prog)
                # raise Exception("Invalid symbol -- ", prog)
    elif type(prog) is list:
        token = prog.pop(0)

        # Language portions related to CAD
        if token == 'unitbox':
            raise Exception("Cannot apply unitcyl")
        elif token == 'unitcyl':
            raise Exception("Cannot apply unitbox")
        elif token == 'true' or token == '#t':
            raise Exception("Cannot apply true")
        elif token == 'false' or token == '#f':
            raise Exception("Cannot apply false")
        elif token == 'cyl':
            if len(prog) == 2:
                return Cyl(AST(prog[0]),AST(prog[1]))
            elif len(prog) == 3:
                return Cyl(AST(prog[0]),AST(prog[1]),AST(prog[2]))
            elif len(prog) == 4:
                return Cyl(AST(prog[0]),AST(prog[1]),AST(prog[2]),AST(prog[3]))
            else:
                raise Exception("Incorrect number of arguments -- ", len(prog))
        elif token == 'box':
            if len(prog) == 3:
                return Box(AST(prog[0]),AST(prog[1]),AST(prog[2]))
            elif len(prog) == 4:
                return Box(AST(prog[0]),AST(prog[1]),AST(prog[2]),AST(prog[3]))
            elif len(prog) == 5:
                return Box(AST(prog[0]),AST(prog[1]),AST(prog[2]),AST(prog[3]),AST(prog[4]))
            else:
                raise Exception("Incorrect number of arguments -- ", len(prog))
        elif token == 'vec' or token == '!': # overloaded to handle vec and rvec
            if len(prog) == 3:
                return Vec(AST(prog[0]),AST(prog[1]),AST(prog[2]))
            elif len(prog) == 4:
                return RVec(AST(prog[0]),AST(prog[1]),AST(prog[2]),AST(prog[3]))
            else:
                raise Exception("Invalid number of arguments to vec (require 3) -- ", len(prog))
        elif token == 'rvec' or token == '!!': # only rvec
            if len(prog) == 4:
                return RVec(AST(prog[0]),AST(prog[1]),AST(prog[2]),AST(prog[3]))
            else:
                raise Exception("Invalid number of arguments to rvec (require 4) -- ", len(prog))
        elif token == 'add' or token == '+':
            return Add(AST(prog[0]),AST(prog[1]))
        elif token == 'sub' or token == '-':
            return Sub(AST(prog[0]),AST(prog[1]))
        elif token == 'mul' or token == '*':
            return Mul(AST(prog[0]),AST(prog[1]))
        elif token == 'div' or token == '/':
            return Div(AST(prog[0]),AST(prog[1]))
        elif token == 'scale' or token == '^^':
            return Scl(AST(prog[0]),AST(prog[1]))
        elif token == 'move' or token == '|>':
            return Mov(AST(prog[0]),AST(prog[1]))
        elif token == 'move-relative' or token == '|>>':
            return Movr(AST(prog[0]),AST(prog[1]))
        elif token == 'rotate' or token == '|#':
            return Rot(AST(prog[0]),AST(prog[1]))
        elif token == 'rotate-relative' or token == '|##':
            return Rotr(AST(prog[0]),AST(prog[1]))
        elif token == 'select' or token == '@':
            return Sel(AST(prog[0]),AST(prog[1]))

        # Core language constructs
        elif token == '>':
            return Gt(AST(prog[0]),AST(prog[1]))
        elif token == '<':
            return Lt(AST(prog[0]),AST(prog[1]))
        elif token == '=':
            return Eq(AST(prog[0]),AST(prog[1]))
        elif token == 'if':
            return If(AST(prog[0]),AST(prog[1]),AST(prog[2]))
        elif token == 'cons':
            return Cons(AST(prog[0]),AST(prog[1]))
        elif token == 'car':
            return Car(AST(prog[0]))
        elif token == 'cdr':
            return Cdr(AST(prog[0]))
        elif token == 'list':
            args = []
            for exp in prog:
                args.append(AST(exp))
            return List(*args)
        elif token == 'let':
            # var = AST(prog[0][0])
            var = prog[0][0]
            value = AST(prog[0][1])
            return Let(var, value, AST(prog[1]))
        elif token == 'lambda':
            fname = 'lambda'
            var = prog[0][0]
            body = AST(prog[1])
            return Fun(fname, var, body)
        elif token == 'define':
            fname = prog[0][0]
            fargs = prog[0][1:]
            args = []
            for exp in fargs:
                args.append(AST(exp))
            return MFunner(fname,AST(prog[1]),*args)
        else: # function application
            args = []
            for exp in prog:
                args.append(AST(exp))
            return MCall(token,*args)
    return prog

def A(program):
    return AST(Parse(program))

def run(program_list):
    for expression in program_list:
        evaluate(expression)
