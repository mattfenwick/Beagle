from . import core

def interpret(ast):
    return core.evaluate(ast, core.getDefaultEnv())
