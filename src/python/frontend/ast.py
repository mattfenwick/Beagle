class Number(object):
    def __init__(self, value):
        self.value = value
        self.type = "number"

class Symbol(object):
    def __init__(self, value):
        self.value = value
        self.type = "symbol"

class String(object):
    def __init__(self, value):
        self.value = value
        self.type = "string"

class List(object):
    def __init__(self, elems):
        self.elems = elems
        self.type = "list"

class Dictionary(object):
    def __init__(self, key_vals):
        self.key_vals = key_vals
        self.type = "dictionary"

class Application(object):
    def __init__(self, operator, arguments):
        self.operator = operator
        self.arguments = arguments
        self.type = "application"

class Def(object):
    def __init__(self, symbol, value):
        self.symbol = symbol
        self.value = value
        self.type = "def"

class Set(object):
    def __init__(self, symbol, value):
        self.symbol = symbol
        self.value = value
        self.type = "set"

class Cond(object):
    def __init__(self, branches, else_value):
        self.branches = branches
        self.else_value = else_value
        self.type = "cond"

class Fn(object):
    def __init__(self, params, forms):
        self.params = params
        self.forms = forms
        self.type = "fn"

class Beagle(object):
    def __init__(self, forms):
        self.forms = forms
        self.type = "beagle"
