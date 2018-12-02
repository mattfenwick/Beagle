from ..frontend import parser
from .. import frontend
import unparse.maybeerror as me
import json
from . import compiler


def parse(input):
    ast_error = frontend.parse_ast(input)
#    print "ast:", ast_error, ast_error.__dict__, "\n", json.dumps(ast_error, default=lambda o: o.__dict__), "\n"
    if ast_error.status != "success":
        print "failed parse!"
        return
    ast = ast_error.value
    insts = compiler.bgl_compile_wrapper(ast)
    for (ix, instruction) in enumerate(insts):
        print "\t", ix, "\t", instruction
#    print "instructions:", insts
    env = compiler.root_env
    compiler.evaluate(insts, env)
    print "after eval:", env


a = """4
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
(+ 32 29)
[1 2 3]
"""

# parse(a)

b = """
{def xs [1 2 3]}
(car xs)
(cdr xs)
(cons 34 xs)
cons
{def flip
  {fn {f}
    {fn {x y} (f y x)}}}
flip
(flip cons)
{def snoc (flip cons)}
(snoc xs 100)
(print 117)
"""
# parse(b)

c = """
(+ 1 14)
{def length
  {fn {xs}
    {cond {{(nil? xs) 0}}
      (+ 1 (length (cdr xs)))}}}
(length nil)
(length [])
(length [1])
(length [1 2 3 4 5])
{def pee
  {fn {x} (* x 7)}}
{def poop
  {fn {x}
    (+ x (pee 3))}}
;(poop 9)
(nil? [1])
{cond {{(nil? []) 3}}
  4}
;z
{def z 41}
{def z 31}
z
"""
parse(c)
