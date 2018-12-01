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
            if a.bgl_type() != t:
                raise Exception("{}: expected type {}, found {}".format(self.name, a.bgl_type(), t))
        return self.op(*args)
    def is_builtin(self):
        return True
    def bgl_type(self):
        return "func"

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

list_car = BuiltinFunc("car", ["list"], lambda l: l.car())
list_cdr = BuiltinFunc("cdr", ["list"], lambda l: l.cdr())

class Closure(object):
    def __init__(self, env, func, name):
        self.env = env
        self.func = func # TODO should this be a code pointer?
        self.name = name
    def is_builtin(self):
        return False
    def bgl_type(self):
        return "func"

class Number(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "number"

plus = BuiltinFunc("+", ["number", "number"], lambda x, y: Number(x.value + y.value))
minus = BuiltinFunc("-", ["number", "number"], lambda x, y: Number(x.value - y.value))


class Boolean(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "boolean"

class String(object):
    def __init__(self, value):
        self.value = value
    def bgl_type(self):
        return "string"
