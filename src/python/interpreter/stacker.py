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

class Frame(object):
    def __init__(self, name, env, i, params, instructions):
        self.name = name
        self.env = env
        self.i = i
        self.params = params
        self.instructions = instructions

class Env(object):
    def __init__(self, name, bindings, parent):
        self.name = name
        self.bindings = bindings
        self.parent = parent
    def get(self, key):
        if key in self.bindings:
            return self.bindings[key]
        if parent is None:
            raise Exception("unbound variable {}".format(key))
        return self.parent.get(key)
    def set(self, key, value):
        self.bindings[key] = value
    def __str__(self):
        return json.dumps(self, default=lambda o: o.__dict__)

def evaluate(instructions, env):
    stack = []
    code = [Frame("root", env, 0, [], instructions)]

    while len(code) > 0:
        frame = code[-1]
        i = frame.i
        # finished the current subroutine: pop its frame
        if i == len(frame.instructions):
            code.pop()
            continue
        # continue executing the current subroutine
        ins, arg = frame.instructions[i]
        if ins == "read":
            stack.append(frame.env.get(arg))
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
            env.set(arg, stack.pop())
            i += 1
        elif ins == "apply":
            op = stack.pop()
            args = []
            for _ in range(arg):
                args.append(stack.pop())
            stack.append(op.apply(args[::-1]))
            i += 1
        else:
            raise Exception("unrecognized instruction {}".format(ins))
        frame.i = i
    if len(stack) != 1:
        raise Exception("expected one value on stack, found {}".format(len(stack)))
    ret_val = stack.pop()
#    print "finished with:", ret_val
    return ret_val


env1 = Env("root", {"a": True, "b": False, "x": 13, "z": 34}, None)
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
env2 = Env("root", {"a": True, "b": False, "c": True, "y": "hello", "z": "this is z", "x": "and now x"}, None)

print evaluate(insts2, env2)


insts3 = [
    ("push", 314),
    ("store", "x"),
    ("read", "x")
]
env3 = Env("root", {}, None)

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
    "root",
    {"c": 32, "d": 29, "a": 9, "b": 7, "+": Op(operator.add), "-": Op(operator.sub), "*": Op(operator.mul)},
    None)
print evaluate(insts4, env4)
# should be 66 = (9 * 7) + (32 - 29)
