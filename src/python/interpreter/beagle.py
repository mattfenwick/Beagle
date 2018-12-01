from ..frontend import parser
from .. import frontend
import unparse.maybeerror as me
import json
from . import compiler
from . import stackvm
from . import types


def parse(input):
    ast_error = frontend.parse_ast(input)
    print "ast:", ast_error, ast_error.__dict__, "\n", json.dumps(ast_error, default=lambda o: o.__dict__), "\n"
    if ast_error.status != "success":
        print "failed parse!"
        return
    ast = ast_error.value
    insts = compiler.compile(ast)
    print "instructions:", insts
    env = stackvm.root_env
    val = stackvm.evaluate(insts, env)
    print "eval:", val, env


parse("""4
{def x 31}
{def f {fn {x} x}}
(f x)
(f 4)
(f 3)
{def fst {fn {q r} q}}
{def snd {fn {q r} r}}

{def um
  {fn {a b x y}
    {cond {{a x}
           {b y}}
        (fst x y)}}}
(um true false 32 29)
(um false true 27 74)
(um false false 99 100)
""")
