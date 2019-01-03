from . import compiler


class Problem(object):
    def __init__(self, severity, description, position):
        self.severity = severity
        self.description = description
        self.position = position
    def human_string(self):
        return "{}: {} at {}".format(self.severity, self.description, self.position)

names = {
    'def': 'special form',
    'set': 'special form',
    'cond': 'special form',
    'fn': 'special form',
    'let': 'macro',
    'and': 'macro',
    'or': 'macro',
    'if': 'macro'
}

def lint(tree):
    """
    - undefined variables
    - redefined variables (multiple defs in same scope)
    - set on undefined variable
    - shadowing
    - using variable with same name as specialform or macro
    - TODO: declared but not used
    - TODO: minimal type checking?
    - improve position reporting -- eg if already defined, give the position at which it was previously defined
    """
    problems = []
    stack = []
    envs = [set(compiler.root_env.bindings)]
    lint_help(tree, stack, envs, problems)
    return problems

def define_variable(name, stack, envs, problems):
    pos = stack[-1][1]
    if name in envs[-1]:
        print(stack)
        problems.append(Problem("error", "symbol {} already defined".format(name), pos))
    else:
        envs[-1].add(name)
    for e in envs[::-1][1:]:
        if name in e:
            print("enclosing:", stack)
            problems.append(Problem("warning", "shadowing: symbol {} defined in enclosing scope".format(name), pos))
    if name in names:
        print(stack)
        problems.append(Problem("warning", "symbol {} is also used as a {}".format(name, names[name]), pos))

def set_variable(name, stack, envs, problems):
    pos = stack[-1][1]
    is_defined = False
    for env in envs[::-1]:
        if name in env:
            is_defined = True
            break
    if not is_defined:
        problems.append(Problem("error", "symbol {} not defined".format(name), pos))

def add_position(node, stack):
    new_stack = stack
    if hasattr(node, '_debug'):
        new_stack = new_stack + [(node.type, node._debug['start'])]
    return new_stack

def lint_help(tree, stack, envs, problems):
#    print(type(tree), dir(tree))

    new_stack = add_position(tree, stack)
    pos = new_stack[-1][1]

    if tree.type == "def":
        define_variable(tree.symbol.value, add_position(tree.symbol, new_stack), envs, problems)
        lint_help(tree.value, new_stack, envs, problems)
    elif tree.type == "set":
        set_variable(tree.symbol.value, add_position(tree.symbol, new_stack), envs, problems)
        lint_help(tree.value, new_stack, envs, problems)
    elif tree.type == "cond":
        forms = []
        if len(tree.branches) == 0:
            problems.append(Problem("warning", "cond with 0 branches can be omitted", pos))
        for (pred, result) in tree.branches:
            forms.extend([pred, result])
        forms.append(tree.else_value)
        for f in forms:
            lint_help(f, new_stack, envs, problems)
    elif tree.type == "application":
        forms = [tree.operator] + tree.arguments
        for f in forms:
            lint_help(f, new_stack, envs, problems)
    elif tree.type == "number":
        pass
    elif tree.type == "symbol":
        is_defined = False
        for env in envs[::-1]:
            if tree.value in env:
                is_defined = True
                break
        if not is_defined:
            problems.append(Problem("error", "symbol {} not defined".format(tree.value), pos))
    elif tree.type == "beagle":
        for f in tree.forms:
            lint_help(f, new_stack, envs, problems)
    elif tree.type == "fn":
        new_envs = envs + [set()]
        for p in tree.params:
            define_variable(p.value, add_position(p, new_stack), new_envs, problems)
        for f in tree.forms:
            lint_help(f, new_stack, new_envs, problems)
    elif tree.type == "list":
        for e in tree.elems:
            lint_help(e, new_stack, envs, problems)
    else:
        raise Exception("unrecognized ast type {}".format(tree.type))
