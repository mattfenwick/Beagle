class Number(object):
    def __init__(self, ast, env):
        self.ast = ast
    
    def is_done(self):
        return True
    
    def evaluate(self):
        return True, self.ast.value

class Symbol(object):
    def __init__(self, ast, env):
        self.ast = ast
        self.env = env
    
    def is_done(self):
        return True
    
    def evaluate(self):
        value = self.env.get_binding(self.ast.value)
        if value is None:
            return False, "symbol {} not defined".format(self.ast.value)
        return True, value

class Def(object):
    def __init__(self, ast, env):
        self.ast = ast
        self.env = env
        self.term = None
    
    def is_done(self):
        return self.term is not None
    
    def did_eval(self, term):
        self.term = term
    
    def next(self):
        return self.ast.value
    
    def evaluate(self):
        if self.env.has_own_binding(self.ast.symbol.value):
            return False, "symbol {} already defined".format(self.ast.symbol.value)
        self.env.add_binding(self.ast.symbol.value, self.term)
        return True, None

class App(object):
    def __init__(self, ast, env):
        self.ast = ast
        # TODO create a new environment?
        self.terms = [ast.operator] + ast.arguments
        self.index = 0
    
    def is_done(self):
        return self.index == len(self.terms)
    
    def did_eval(self, term):
        self.terms[self.index] = term
        self.index += 1
    
    def next(self):
        return self.terms[index]
    
    def evaluate(self):
        op = self.terms[0]
        args = self.terms[1:]
        return True, op.apply(args)

class Fn(object):
    def __init__(self, ast, env):
        self.ast = ast
        self.env = env

ast_types = {
    'number': Number,
    'symbol': Symbol,
    'def': Def,
    'app': App,
}
def wrapper(ast, env):
    return ast_types[ast.type](ast, env)


class Evaluator(object):
    def __init__(self, root_env):
        self.stack = []
        self.root_env = root_env
    
    def evaluate(self, ast):
        stack = [wrapper(ast, self.root_env)]
        while len(stack) > 0:
            top = stack[-1]
            if not top.is_done():
                stack.append(wrapper(top.next(), self.root_env))
                continue
            ok, top_val = top.evaluate()
            if not ok:
                return ok, top_val
            if len(stack) == 1:
                return True, top_val
            stack.pop()
            stack[-1].did_eval(top_val)

def example():
    from ..frontend import ast
    from . import environment
    egs = [
        ast.Number(3),
        ast.Symbol('x'),
        ast.Symbol('y'),
        ast.Def(ast.Symbol('z'), ast.Number(4)),
        ast.Def(ast.Symbol('z'), ast.Number(4)),
        ast.Def(ast.Symbol('x'), ast.Number(44)),
    ]
    e = Evaluator(environment.Env('root', {'x': 31}))
    for eg in egs:
        print e.evaluate(eg)#.__dict__


if __name__ == "__main__":
    example()
