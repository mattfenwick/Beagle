from .environment import Env

# //////////////// a little prelude

class FunctionError(Exception):
    def __init__(type, expected, actual, funcname, message):
        self.type = type
        self.expected = expected
        self.actual = actual
        self.funcname = funcname
        self.message = message

class SpecialFormError(Exception):
    def __init__(type, expected, actual, sfname, message):
        self.type = type
        self.expected = expected
        self.actual = actual
        self.sfname = sfname
        self.message = message

def typeCheck(expected, actual, fname, message):
    if expected != actual:
        raise SpecialFormError('TypeError', expected, actual, fname, message)

def argsCheck(expected, actual, fname, message):
    if expected != actual:
        raise SpecialFormError('NumArgsError', expected, actual, fname, message)

# //////////////// the data

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

class Boolean(object):
    def __init__(self, value):
        self.value = value
        self.type = "boolean"

class List(object):
    def __init__(self, first, next):
        self.first = first
        self.next = next
        self.type = "list"
    
    def is_empty(self):
        return self.next is None
    
    @staticmethod
    def from_array(elems):
        dummy = List(None, None)
        last = dummy
        for e in elems:
            nxt = List(e, None)
            last.next = nxt
            last = nxt
        return dummy.next

class Nil(object):
    def __init__(self):
        self.type = "nil"

class Dictionary(object):
    def __init__(self, key_vals):
        self.key_vals = key_vals
        self.type = "dictionary"

class UserFunc(object):
    def __init__(self, name, params, forms, env):
        self.name = name
        self.params = params
        self.forms = forms
        self.env = env
        self.type = "userfunc"
    
class BuiltinFunc(object):
    def __init__(self, name, types, function):
        self.name = name
        self.types = types
        self.function = function
        self.type = "builtinfunc"


bg_true = Boolean(True)
bg_false = Boolean(False)
bg_nil = Nil()


# /////////////////// the functions

def cons_body(args):
    first, rest = args
    return List(first, rest)

cons = BuiltinFunc('cons', [None, 'List'], cons_body)

def car_body(args):
    lst = args[0]
    if lst.is_empty():
        raise FunctionError("ValueError", "non-empty list", "empty list", 'car', "1st arg")
    return lst.first

car = constructors.BuiltinFunc('car', ['List'], car_body)

def cdr_body(args):
    lst = args[0]
    if lst.is_empty():
        raise FunctionError("ValueError", "non-empty list", "empty list", 'cdr', '1st arg')
    return lst.next

cdr = constructors.BuiltinFunc('cdr', ['List'], cdr_body)

def emptyQ_body(args):
    lst = args[0]
    return bg_true if lst.is_empty() else bg_false

emptyQ = constructors.BuiltinFunc('empty?', ['List'], emptyQ_body)

plus = constructors.BuiltinFunc('+', ['Number', 'Number'], lambda args: Number(args[0].value + args[1].value))

COMPARABLE = {'number', 'char', 'boolean', 'nil'}

def is_equal_body(args):
    left = args[0]
    right = args[1]
    ltype = left.type
    rtype = right.type
    if ltype != rtype or ltype not in COMPARABLE:
        return bg_false
    # TODO is `value` okay to use?  what about for nil?
    return bg_true if (left.value == right.value) else bg_false

is_equal = constructors.BuiltinFunc('=', [null, null], is_equal_body)


# ///////////

def root_env(name="root"):
    bindings = {
            # TODO empty list?
            'true'   : beagleTrue,
            'false'  : beagleFalse,
            'nil'    : beagleNil,
            'cons'   : cons,
            'car'    : car,
            'cdr'    : cdr,
            'null?'  : nullQ,
            '+'      : plus,
            '='      : isEqual,
        }
    return Env(name, bindings, None)

# evaluation

def evalNumber(node, env):
    return Number(node.numValue)

def evalSymbol(node, env):
    val = env.get_binding(node.value)
    if val is not None:
        return val
    raise SpecialFormError('UndefinedVariableError', '', '', 'evaluateAtom', 'symbol ' + node.strValue + ' is not defined')

def evalString(node, env):
    return String(node.strValue)

def evalList(node, env):
    dummy = List(None, None)
    last = dummy
    for e in elems:
        nxt = List(evaluate(e, env), None)
        last.next = nxt
        last = nxt
    return dummy.next

# TODO dictionary

def eval_Application(node, env):
    func = evaluate(node.operator, env)
    return applyFunction(func, env, node.arguments)

def applyFunction(func, env, args) {
    # TODO can this be cleaned up?
    fname = func.name if (func.datatype == 'BuiltinFunc') else 'user-defined function'
    expectedNumArgs = len(func.types) if (func.datatype == 'BuiltinFunc') else len(func.params)
    if len(args) != expectedNumArgs:
        raise FunctionError("NumArgsError", expectedNumArgs, len(args), fname, '')
    # `func` has already been evaluated
    var evaledArgs = args.map(function (arg) {
        return evaluate(arg, env);
    });

    if (func.datatype == 'UserFunc') {
        var newEnv = new Environment.Environment(env, {}),
            retVal = beagleNil;
        // put parameter bindings into local environment
        for (var j = 0; j < func.params.length; j++) {
            newEnv.addBinding(func.params[j], evaledArgs[j]);
        }
        // evaluate all the body forms
        for(var k = 0; k < func.forms.length; k++) {
            retVal = evaluate(func.forms[k], newEnv);
        }
        // and return the last form
        return retVal;
    } else if (func.datatype == 'BuiltinFunc') {
        for (var i = 0; i < evaledArgs.length; i++) {
            if (func.types[i] && (evaledArgs[i].datatype != func.types[i])) {
                throw new FunctionError("TypeError", func.types[i], evaledArgs[i].datatype, func.name, ' arg ' + (i + 1));
            }
        }
        return func.function(evaledArgs);
    }
    
    throw new Error('invalid node type in applyFunction -- ' + func.datatype);
}

def evalDefine(node, env) {
    var name = node.symbol.strValue;
    if (env.hasOwnBinding(name)) {
        throw new SpecialFormError('ValueError', 'unbound symbol',
                'bound symbol ' + name, 'define', 'cannot redefine symbol');
    }
    var value = evaluate(node.value, env);
    env.addBinding(name, value);
    return beagleNil;
}

def evalSet(node, env) {
    var name = node.symbol.strValue;
    if (!env.hasBinding(name)) {
        throw new SpecialFormError('ValueError', 'bound symbol',
                'unbound symbol ' + name, 'set', 'cannot set undefined symbol');
    }
    var value = evaluate(node.value, env);
    env.setBinding(name, value);
    return beagleNil;
}

def evalCond(node, env) {
    var pairs = node.branches,
        test, i;
    
    for (i = 0; i < pairs.length; i++) {
        test = evaluate(pairs[i][0], env);
        typeCheck('Boolean', test.datatype, 'cond', "condition of argument " + (i + 1));
        
        if (test.value) {
            return evaluate(pairs[i][1], env);
        }
    }
    
    // didn't find a true condition
    return evaluate(node.elseValue, env);
}

def evalFn(node, env) {
    // TODO could put the position into the name
    var params = node.params.map(function(p) {return p.strValue;});
    return constructors.UserFunc('user function', params, node.forms, env);
}

def eval_beagle(node, env):
    var evaled = [];
    // using forEach to emphasize the side effects from passing `env`
    node.forms.forEach(function(form) {
        evaled.push(evaluate(form, env));
    });
    return evaled;
}

actions = {
        'Number'     : evalNumber,
        'Symbol'     : evalSymbol,
        'String'     : evalString,
        'List'       : evalList,
#            'Dictionary'  : evalDictionary, // TODO
        'Application': evalApplication,
        'Def'        : evalDefine,
        'Set'        : evalSet,
        'Cond'       : evalCond,
        'Fn'         : evalFn,
        'Beagle'     : evalBeagle,
    }

def evaluate(node, env):
    return actions[node.asttype](node, env)
