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
    def __init__(self, parent_env, address, name):
        self.parent_env = parent_env
        self.address = address
        self.name = name
    def is_builtin(self):
        return False
    def bgl_type(self):
        return "func"
    def __str__(self):
        return "{{closure ({}@{})}}".format(self.name, self.address)
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

class Frame(object):
    def __init__(self, name, env, return_address):
        self.name = name
        self.env = env
        self.return_address = return_address
    def __str__(self):
        return str((len(list(self.env.bindings.iterkeys())), self.env.parent_count(), self.return_address))
    def __repr__(self):
        return self.__str__()

class Env(object):
    def __init__(self, bindings, name, parent=None):
        self.name = name
        self.bindings = bindings
        self.parent = parent
        for key in self.bindings:
            self._check_key_type(key)
    def parent_count(self):
        e = self
        parents = 0
        while e is not None:
            parents += 1
            e = e.parent
        return parents
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

def bgl_compile_wrapper(tree):
    ctx = {'funcs': []}
    instrs = bgl_compile(tree, ctx)
    instrs.append(("return", None))
    offsets = {}
    # calculate function addresses
    for (label, func_instrs) in ctx['funcs']:
        print "label:", label
        offsets[label] = len(instrs)
        instrs.extend(func_instrs)
    # fill in function addresses
    for i in range(len(instrs)):
        inst, arg = instrs[i]
        if inst == "func":
            if arg in offsets:
                instrs[i] = (inst, offsets[arg])
            else:
                raise Exception("couldn't find label {} in offset (keys {})".format(arg, list(offsets.iterkeys())))
    return instrs

def bgl_compile(tree, ctx):
    instrs = []
    if tree.type == "def":
        # TODO verify that not yet defined
        # TODO distinguish between def and set
        instrs.extend(bgl_compile(tree.value, ctx))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "set":
        # TODO verify that already defined
        # TODO distinguish between def and set
        instrs.extend((bgl_compile(tree.value, ctx)))
        instrs.append(("store", tree.symbol.value))
    elif tree.type == "cond":
        labels = ["c" + str(i) for i in range(len(tree.branches))]
        labels.extend(["else", "end"])
        offsets = dict((key, None) for key in labels)
        conds = []
        for (i, (pred, result)) in enumerate(tree.branches):
            offsets[labels[i]] = len(conds)
            conds.extend(bgl_compile(pred, ctx))
            conds.append(("ifn", labels[i+1]))
            conds.extend(bgl_compile(result, ctx))
            conds.append(("jump", "end"))
        offsets["else"] = len(conds)
        conds.extend(bgl_compile(tree.else_value, ctx))
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
            instrs.extend(bgl_compile(a, ctx))
        instrs.extend(bgl_compile(tree.operator, ctx))
        instrs.append(("apply", len(tree.arguments)))
    elif tree.type == "number":
        instrs.append(("push", Number(tree.value)))
    elif tree.type == "symbol":
        instrs.append(("read", tree.value))
    elif tree.type == "beagle":
        for f in tree.forms:
            instrs.extend(bgl_compile(f, ctx))
            instrs.append(("empty", None))
    elif tree.type == "fn":
        proc_instrs = []
        for p in tree.params[::-1]:
            proc_instrs.append(("store", p.value))
        for (ix, f) in enumerate(tree.forms):
            proc_instrs.extend(bgl_compile(f, ctx))
            # ensure the stack has been emptied for all but the last form
            if ix < (len(tree.forms) - 1):
                proc_instrs.append(("empty", None))
        proc_instrs.append(("return", None))
        label = "f-{}".format(len(ctx['funcs']))
        ctx['funcs'].append((label, proc_instrs))
        instrs.append(("func", label))
    elif tree.type == "list":
        instrs.append(("push", bgl_nil))
        for e in tree.elems[::-1]:
            instrs.extend(bgl_compile(e, ctx))
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
    i = 0
    code = [Frame("root", env, len(instructions))]

    while len(code) > 0:
        frame = code[-1]
        ins, arg = instructions[i]
        print "i, in, a, st, c:  ", i, "\t", ins, "\t", arg, "\t", stack, "\t", code
        jump = 1
        if ins == "read":
#            print "read env?", list(frame.get_env().bindings.iterkeys()), type(frame.get_env().parent)
#            print "name, parent name:", frame.get_env().name, frame.get_env().parent.name if frame.get_env().parent is not None else "<no parent>"
#            print "frame:", frame.get_env()
            stack.append(frame.env.get(arg))
        elif ins == "ifn":
            val = stack.pop()
#            print "val?", val
            if not val.is_true():
                jump = arg
        elif ins == "jump":
            jump = arg
        elif ins == "return":
            popped_frame = code.pop()
            jump = popped_frame.return_address - i + 1
        elif ins == "push":
            stack.append(arg)
        elif ins == "store":
            frame.env.set(arg, stack.pop())
        elif ins == "func":
            address = arg
#            print "func?", type(func), func
            name = "closure-{}".format(closure_counter)
            stack.append(Closure(frame.env, address, name))
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
#                print "user-defined op:", dir(op), dir(op.__dict__), type(op.func), op.func
                env = Env({}, name="closure-{}".format(op.name), parent=op.parent_env)
                code.append(Frame("closure-{}".format(op.name), env, i))
                jump = op.address - i
        else:
            raise Exception("unrecognized instruction {}, arg {}".format(ins, arg))
        i += jump
    print "execution finished, {} values left on stack ({})".format(len(stack), stack)
