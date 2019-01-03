from __future__ import print_function
from . import frontend
import unparse.maybeerror as me
import json
from . import compiler
from . import linter


def parse(input):
    # parsing
    ast_error = frontend.parse_ast(input)
    if ast_error.status != "success":
        print("failed parse!")
        print("ast:", ast_error, ast_error.__dict__, "\n", json.dumps(ast_error, default=lambda o: o.__dict__), "\n")
        return
    ast = ast_error.value
    # linting
    problems = linter.lint(ast)
    for p in problems:
        print("\tproblem:", p.human_string())
    # compilation
    insts = compiler.bgl_compile_wrapper(ast)
    for (ix, instruction) in enumerate(insts):
        print("\t", ix, "\t", instruction)
#    print("instructions:", insts)
    # execution
    env = compiler.root_env
    compiler.evaluate(insts, env)
    # done
    print("after eval:", env)

if __name__ == "__main__":
    import sys
    with open(sys.argv[1], 'r') as f:
        parse(f.read())
