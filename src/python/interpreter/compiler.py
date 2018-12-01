import json


## Beagle types

class Boolean(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "boolean"
    def __str__(self):
        return "bg-" + str(self.value)
    def __repr__(self):
        return self.__str__()
    def is_true(self):
        return self.value

bgl_true = Boolean(True)
bgl_false = Boolean(False)

def bgl_if(pred):
    if pred:
#        print "returning bgl_true"
        return bgl_true
#    print "returning bgl_false"
    return bgl_false

class BuiltinFunc(object):
    def __init__(self, name, types, op):
        self.name = name
        self.types = types
        self.op = op
    def apply(self, args):
        if len(args) != len(self.types):
            raise Exception("{}: expected {} args, found {}".format(self.name, len(args), len(self.types)))
        for (a, t) in zip(args, self.types):
#            if not isinstance(a, t):
 #               raise Exception("{}: expected type {}, found {}".format(self.name, t, a))
            if t is not None and a.bgl_type() != t:
                raise Exception("{}: expected type {}, found {}".format(self.name, t, a.bgl_type()))
        return self.op(*args)
    def is_builtin(self):
        return True
    def bgl_type(self):
        return "func"
    def __str__(self):
        return "{{builtin func {} ({})}}".format(self.name, self.types)
    def __repr__(self):
        return self.__str__()

class Nil(object):
    def __init__(self):
        pass
    def car(self):
        raise Exception("cannot take car of Nil")
    def cdr(self):
        raise Exception("cannot take cdr of Nil")
    def is_empty(self):
        return True
    def bgl_type(self):
        return "list"
    def __str__(self):
        return "bg-nil"
    def __repr__(self):
        return self.__str__()

bgl_nil = Nil()
    
class List(object):
    def __init__(self, value, next):
        self.value = value
        self.next = next
    def car(self):
        return self.value
    def cdr(self):
        return self.next
    def is_empty(self):
        return False
    def bgl_type(self):
        return "list"
    def __str__(self):
        values = []
        head = self
        while not head.is_empty():
            values.append(head.value)
            head = head.next
        return "(" + " ".join(map(str, values)) + ")"
    def __repr__(self):
        return self.__str__()

list_car = BuiltinFunc("car", ["list"], lambda l: l.car())
list_cdr = BuiltinFunc("cdr", ["list"], lambda l: l.cdr())
list_cons = BuiltinFunc("cons", [None, "list"], lambda h, t: List(h, t))
list_snoc = BuiltinFunc("snoc", ["list", None], lambda t, h: List(h, t))
def nilq_action(l):
#    print "nilq arg:", l, l.is_empty()
    return bgl_if(l.is_empty())
list_nilq = BuiltinFunc("nil?", ["list"], nilq_action)

class Closure(object):
    def __init__(self, env, func, name):
        self.env = env
        self.func = func # TODO should this be a code pointer?
        self.name = name
    def is_builtin(self):
        return False
    def bgl_type(self):
        return "func"
    def __str__(self):
        return "{{closure ({})}}".format(len(self.func.params))
    def __repr__(self):
        return self.__str__()

class Number(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "number"
    def __str__(self):
        return "bg-" + str(self.value)
    def __repr__(self):
        return self.__str__()

#def unwrap_apply(f, a, b):
#    return f(a.value, b.value)

plus  = BuiltinFunc("+" , ["number", "number"], lambda x, y: Number(x.value  + y.value))
minus = BuiltinFunc("-" , ["number", "number"], lambda x, y: Number(x.value  - y.value))
mul   = BuiltinFunc("*" , ["number", "number"], lambda x, y: Number(x.value  * y.value))
div   = BuiltinFunc("/" , ["number", "number"], lambda x, y: Number(x.value  / y.value))
mod   = BuiltinFunc("%" , ["number", "number"], lambda x, y: Number(x.value  % y.value))
# yes, these comparisons only do numbers (at least for now) -- TODO genericize??
lt    = BuiltinFunc("<" , ["number", "number"], lambda x, y: bgl_if(x.value  < y.value))
le    = BuiltinFunc("<=", ["number", "number"], lambda x, y: bgl_if(x.value <= y.value))
gt    = BuiltinFunc(">" , ["number", "number"], lambda x, y: bgl_if(x.value  > y.value))
ge    = BuiltinFunc(">=", ["number", "number"], lambda x, y: bgl_if(x.value >= y.value))
eq    = BuiltinFunc("==", ["number", "number"], lambda x, y: bgl_if(x.value == y.value))


class String(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "string"
    def __str__(self):
        return "bg-" + self.value
    def __repr__(self):
        return self.__str__()

def print_action(o):
    """
    TODO should this return a void/nil/None value or something?
    """
    print str(o)
    return o
bgl_print = BuiltinFunc("print", [None], print_action)


## Types to support execution, but which aren't directly visible from Beagle

class Func(object):
    def __init__(self, params, instructions):
        self.params = params
        self.instructions = instructions
    def __repr__(self):
        return json.dumps(self, default=lambda o: o.__dict__)

class Frame(object):
    def __init__(self, name, env, i, instructions):
        self.name = name
        self.env = env
        self.i = i
        self.instructions = instructions
    def next_inst(self):
        return self.instructions[self.i]
    def is_done(self):
        return self.i == len(self.instructions)
    def get_env(self):
        return self.env

class ClosureFrame(object):
    def __init__(self, i, bindings, closure):
        self.i = i
        self.env = Env(bindings, name="closure-{}".format(closure.name), parent=closure.env)
        self.closure = closure
    def next_inst(self):
        return self.closure.func.instructions[self.i]
    def is_done(self):
        return self.i == len(self.closure.func.instructions)
    def get_env(self):
        return self.env

class Env(object):
    def __init__(self, bindings, name, parent=None):
        self.name = name
        self.bindings = bindings
        self.parent = parent
        for key in self.bindings:
            self._check_key_type(key)
    def get(self, key):
        if key in self.bindings:
            return self.bindings[key]
        if self.parent is None:
            raise Exception("unbound variable {}".format(key))
        return self.parent.get(key)
    def _check_key_type(self, key):
        if not isinstance(key, str):
            raise TypeError("keys must be strings, got {}".format(type(key)))
    def set(self, key, value):
        self.bindings[key] = value
    def __str__(self):
        s = str({'name': self.name, 'keys': list(self.bindings.iterkeys())})
        if self.parent is None:
            return s
        return "{} -> {}".format(s, repr(self.parent))
    def __repr__(self):
        return self.__str__()


## Compiler

def bgl_compile(tree):
    instrs = []
    if tree.type == "def":
        # TODO verify that not yet defined
        instrs.extend(bgl_compile(tree.value))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "set":
        # TODO verify that already defined
        instrs.extend((bgl_compile(tree.value)))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "cond":
        labels = ["c" + str(i) for i in range(len(tree.branches))]
        labels.extend(["else", "end"])
        offsets = dict((key, None) for key in labels)
        conds = []
        for (i, (pred, result)) in enumerate(tree.branches):
            offsets[labels[i]] = len(conds)
            conds.extend(bgl_compile(pred))
            conds.append(("ifn", labels[i+1]))
            conds.extend(bgl_compile(result))
            conds.append(("jump", "end"))
        offsets["else"] = len(conds)
        conds.extend(bgl_compile(tree.else_value))
        offsets["end"] = len(conds)
        cond_instrs = []
        for (i, (inst, arg)) in enumerate(conds):
            if inst in ["jump", "ifn"] and arg in offsets:
                cond_instrs.append((inst, offsets[arg] - i))
            else:
                cond_instrs.append((inst, arg))
        instrs.extend(cond_instrs)
    elif tree.type == "application":
        for a in tree.arguments:
            instrs.extend(bgl_compile(a))
        instrs.extend(bgl_compile(tree.operator))
        instrs.append(("apply", len(tree.arguments)))
    elif tree.type == "number":
        instrs.append(("push", Number(tree.value)))
    elif tree.type == "symbol":
        instrs.append(("read", tree.value))
    elif tree.type == "beagle":
        for f in tree.forms:
            instrs.extend(bgl_compile(f))
            instrs.append(("empty", None))
    elif tree.type == "fn":
        proc_instrs = []
        for f in tree.forms:
            proc_instrs.extend(bgl_compile(f))
            instrs.append(("empty", None))
        params = [p.value for p in tree.params]
#        print "params?", tree.params, params
        instrs.append(("func", Func(params=params, instructions=proc_instrs)))
    elif tree.type == "list":
        instrs.append(("push", bgl_nil))
        for e in tree.elems[::-1]:
            instrs.extend(bgl_compile(e))
            instrs.append(("push", list_snoc))
            instrs.append(("apply", 2))
    else:
        raise Exception("unrecognized instruction type {}".format(tree.type))
    return instrs



## Root environment

bindings = {
    "print": bgl_print,
    "true" : Boolean(True),
    "false": Boolean(False),
    "nil"  : bgl_nil,
    "cons" : list_cons,
    "car"  : list_car,
    "cdr"  : list_cdr,
    "nil?" : list_nilq,
    "+"    : plus,
    "-"    : minus,
    "*"    : mul,
    "/"    : div,
    "%"    : mod,
    "<"    : lt,
    "<="   : le,
    ">"    : gt,
    ">="   : ge,
    "=="   : eq}
root_env = Env(bindings, "root")


## Evaluator

def evaluate(instructions, env):
    closure_counter = 0

    stack = []
    code = [Frame("root", env, 0, instructions)]

    while len(code) > 0:
        frame = code[-1]
        # finished the current subroutine: pop its frame
        if frame.is_done():
            code.pop()
            continue
        # continue executing the current subroutine
        ins, arg = frame.next_inst()
#        print "instruction, arg, stack:", ins, arg, stack
        jump = 1
        if ins == "read":
#            print "read env?", list(frame.get_env().bindings.iterkeys()), type(frame.get_env().parent)
#            print "name, parent name:", frame.get_env().name, frame.get_env().parent.name if frame.get_env().parent is not None else "<no parent>"
#            print "frame:", frame.get_env()
            stack.append(frame.get_env().get(arg))
        elif ins == "ifn":
            val = stack.pop()
#            print "val?", val
            if not val.is_true():
                jump = arg
        elif ins == "jump":
            jump = arg
        elif ins == "return":
            code.pop()
        elif ins == "push":
            stack.append(arg)
        elif ins == "store":
            frame.get_env().set(arg, stack.pop())
        elif ins == "func":
            func = arg
#            print "func?", type(func), func
            stack.append(Closure(frame.get_env(), func, str(closure_counter)))
            closure_counter += 1
        elif ins == "empty":
            if len(stack) > 1:
                raise Exception("expected stack size of 0 or 1, found {}".format(len(stack)))
#            print "length of stack (should be 0):", len(stack)
            while len(stack) > 0:
                print "popping for cleanup:", stack.pop()
        elif ins == "apply":
            op = stack.pop()
            if op.is_builtin():
                args = []
                for _ in range(arg):
                    args.append(stack.pop())
                stack.append(op.apply(args[::-1]))
            else:
                bindings = {}
#                print "user-defined op:", dir(op), dir(op.__dict__), type(op.func), op.func
                for key in op.func.params[::-1]:
                    bindings[key] = stack.pop()
                code.append(ClosureFrame(0, bindings, op))
        else:
            raise Exception("unrecognized instruction {}, arg {}".format(ins, arg))
        frame.i += jump
    print "execution finished, {} values left on stack ({})".format(len(stack), stack)
