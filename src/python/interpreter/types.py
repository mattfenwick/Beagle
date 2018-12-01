import json


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
    def __repr__(self):
        return json.dumps(self, default=lambda o: o.__dict__)

class Closure(object):
    def __init__(self, env, func, name):
        self.env = env
        self.func = func
        self.name = name
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
    def __repr__(self):
        if self.parent is not None:
            print "Not none!", self.name
        s = str({'name': self.name, 'keys': list(self.bindings.iterkeys())})
        if self.parent is None:
            return s
        return "{} -> {}".format(s, repr(self.parent))
#    def __str__(self):
#        return json.dumps(self, default=lambda o: o.__dict__)
