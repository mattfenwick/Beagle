import operator
import json

# actions = {
#    "eval": lambda x, env, i: env[x], i + 1
#    "ifn": lambda new_ix, env, i: 
# }

class Op(object):
    def __init__(self, op):
        self.op = op
    def apply(self, args):
        return self.op(*args)
    def is_builtin(self):
        return True

class Func(object):
    def __init__(self, params, instructions):
        self.params = params
        self.instructions = instructions

class Closure(object):
    def __init__(self, env, func):
        self.env = env
        self.func = func
    def is_builtin(self):
        return False

class OldFrame(object):
    def __init__(self, name, env, i, func):
        self.name = name
        self.env = env
        self.i = i
        self.func = func

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
        self.env = Env(bindings, parent=closure.env)
        self.closure = closure
    def next_inst(self):
        return self.closure.func.instructions[self.i]
    def is_done(self):
        return self.i == len(self.closure.func.instructions)
    def get_env(self):
        return self.env

class Env(object):
    def __init__(self, bindings, name="", parent=None):
        self.name = name
        self.bindings = bindings
        self.parent = parent
    def get(self, key):
        if key in self.bindings:
            return self.bindings[key]
        if self.parent is None:
            raise Exception("unbound variable {}".format(key))
        return self.parent.get(key)
    def set(self, key, value):
        self.bindings[key] = value
    def __str__(self):
        return json.dumps(self, default=lambda o: o.__dict__)

def evaluate(instructions, env):
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
        i = frame.i
        if ins == "read":
            stack.append(frame.get_env().get(arg))
            i += 1
        elif ins == "ifn":
            val = stack.pop()
            if not val:
                i = arg
            else:
                i += 1
        elif ins == "jump":
            i = arg
        elif ins == "return":
            code.pop()
        elif ins == "push":
            stack.append(arg)
            i += 1
        elif ins == "store":
            frame.get_env().set(arg, stack.pop())
            i += 1
        elif ins == "func":
            func = stack.pop()
            stack.append(Closure(frame.get_env(), func))
            i += 1
        elif ins == "apply":
            op = stack.pop()
            if op.is_builtin():
                args = []
                for _ in range(arg):
                    args.append(stack.pop())
                stack.append(op.apply(args[::-1]))
                i += 1
            else:
                bindings = {}
                for key in op.func.params[::-1]:
                    bindings[key] = stack.pop()
                code.append(ClosureFrame(0, bindings, op))
                i += 1
        elif ins == "print":
            print stack.pop()
            i += 1
#        elif ins == "func": TODO
#            ???
#            leave a func object on the stack, consisting of pointer to env, code
        else:
            raise Exception("unrecognized instruction {}".format(ins))
        frame.i = i
    if len(stack) != 1:
        raise Exception("expected one value on stack, found {}".format(len(stack)))
    ret_val = stack.pop()
#    print "finished with:", ret_val
    return ret_val


env1 = Env({"a": True, "b": False, "x": 13, "z": 34}, "root")
insts1 = [
    ("read", "a"),
    ("ifn", 4)   ,
    ("read", "z"),
    ("jump", 9)  ,
    ("read", "b"),
    ("ifn", 8)   ,
    ("read", "y"),
    ("jump", "9"),
    ("read", "x"),
    ("return", None)
]

print evaluate(insts1, env1)


insts2 = [
    ("read", "a"),
    ("ifn", 4)   ,
    ("read", "b"),
    ("jump", 5)  ,
    ("push", False),
    ("ifn", 8)   ,
    ("read", "z"),
    ("jump", 13) ,
    ("read", "c"),
    ("ifn", 12)  ,
    ("read", "y"),
    ("jump", 13) ,
    ("read", "x"),
    ("return", None)
]
env2 = Env({"a": True, "b": False, "c": True, "y": "hello", "z": "this is z", "x": "and now x"}, "root")

print evaluate(insts2, env2)


insts3 = [
    ("push", 314),
    ("store", "x"),
    ("read", "x")
]
env3 = Env({}, "root")

print evaluate(insts3, env3)
print env3


insts4 = [
    ("read" , "c"),
    ("read" , "d"),
    ("read" , "-"),
    ("apply", 2  ),
    ("read" , "a"),
    ("read" , "b"),
    ("read" , "*"),
    ("apply", 2  ),
    ("read" , "+"),
    ("apply", 2  ),
]
# (+ (* a b) (- c d))
env4 = Env(
    {"c": 32, "d": 29, "a": 9, "b": 7, "+": Op(operator.add), "-": Op(operator.sub), "*": Op(operator.mul)},
    "root")
print evaluate(insts4, env4)
# should be 66 = (9 * 7) + (32 - 29)


env5 = Env({"+": Op(operator.add)}, "root")
insts5 = [
    ("push", 102),
    ("push", Closure(
                Env({}, parent=env5), 
                Func(params=["x"], instructions=[("push", 37), ("read", "x"), ("read", "+"), ("apply", 2)]))),
    ("apply", None),
]
print "5:", evaluate(insts5, env5)


env6 = Env({"+": Op(operator.add)}, "root")
insts6 = [
    ("push", Closure(
                Env({}, parent=env6),
                Func(params=["x"], instructions=[("push", 37), ("read", "x"), ("read", "+"), ("apply", 2)]))),
    ("store", "f"),
    ("push", 13),
    ("read", "f"),
    ("apply", None),
    ("push", 67),
    ("read", "f"),
    ("apply", None),
    ("print", None),
]
print "6:", evaluate(insts6, env6)


env7 = Env({"+": Op(operator.add)}, "root")
insts7 = [
    ("push", Func(params=["z"], instructions=[("read", "z"), ("read", "z"), ("read", "+"), ("apply", 2)])),
    ("func", None),
    ("store", "f"),
    ("push", 21),
    ("read", "f"),
    ("apply", None),
    ("push", 37),
    ("read", "f"),
    ("apply", None),
    ("print", None)
]
print "7:", evaluate(insts7, env7)
