from .types import Env, Op, Closure, Func
from .stackvm import evaluate
import operator
from ..frontend import ast
from . import compiler


eg1 = ast.Beagle([
        ast.Def(ast.Symbol("a"), ast.Number(3)),
        ast.Application(
            ast.Fn(["q", "r"], [ast.Application(ast.Symbol("+"), [ast.Symbol("q"), ast.Symbol("r")])]),
            [ast.Symbol("x"), ast.Application(ast.Symbol("f"), [ast.Number(31), ast.Number(72)])])
    ])

eg2 = ast.Cond([
        [ast.Symbol("a"), ast.Number(32)],
        [ast.Application(ast.Symbol("g"), [ast.Symbol("y")]), ast.Number(99)]
    ],
    ast.Application(ast.Symbol("f"), [ast.Symbol("x")]))

def compile_example():
    insts = compiler.compile(eg1)
    print eg1
    for i in insts:
        print "\t", i
    print
    print eg2
    for i in compiler.compile(eg2):
        print "\t", i


def stack_execute_example():
    env1 = Env({"a": True, "b": False, "x": 13, "z": 34}, "root")
    insts1 = [
        ("read", "a"),
        ("ifn", 3)   ,
        ("read", "z"),
        ("jump", 6)  ,
        ("read", "b"),
        ("ifn", 3)   ,
        ("read", "y"),
        ("jump", "9"),
        ("read", "x"),
        ("return", None)
    ]

    print evaluate(insts1, env1)


    insts2 = [
        ("read", "a"),
        ("ifn" , 3  ),
        ("read", "b"),
        ("jump", 2  ),
        ("push", False),
        ("ifn" , 3  ),
        ("read", "z"),
        ("jump", 6  ),
        ("read", "c"),
        ("ifn" , 3  ),
        ("read", "y"),
        ("jump", 2  ),
        ("read", "x"),
        ("return", None)
    ]
    env2 = Env({"a": True, "b": False, "c": True, "y": "hello", "z": "this is z", "x": "and now x"}, "root")

    print evaluate(insts2, env2)


    insts3 = [
        ("push", 314),
        ("store", "x"),
        ("read", "x")
    ]
    env3 = Env({}, "root")

    print evaluate(insts3, env3)
    print env3


    insts4 = [
        ("read" , "c"),
        ("read" , "d"),
        ("read" , "-"),
        ("apply", 2  ),
        ("read" , "a"),
        ("read" , "b"),
        ("read" , "*"),
        ("apply", 2  ),
        ("read" , "+"),
        ("apply", 2  ),
    ]
    # (+ (* a b) (- c d))
    env4 = Env(
        {"c": 32, "d": 29, "a": 9, "b": 7, "+": Op(operator.add), "-": Op(operator.sub), "*": Op(operator.mul)},
        "root")
    print evaluate(insts4, env4)
    # should be 66 = (9 * 7) + (32 - 29)


    env5 = Env({"+": Op(operator.add)}, "root")
    insts5 = [
        ("push", 102),
        ("push", Closure(
                    Env({}, parent=env5), 
                    Func(params=["x"], instructions=[("push", 37), ("read", "x"), ("read", "+"), ("apply", 2)]))),
        ("apply", None),
    ]
    print "5:", evaluate(insts5, env5)


    env6 = Env({"+": Op(operator.add)}, "root")
    insts6 = [
        ("push", Closure(
                    Env({}, parent=env6),
                    Func(params=["x"], instructions=[("push", 37), ("read", "x"), ("read", "+"), ("apply", 2)]))),
        ("store", "f"),
        ("push", 13),
        ("read", "f"),
        ("apply", None),
        ("push", 67),
        ("read", "f"),
        ("apply", None),
        ("print", None),
    ]
    print "6:", evaluate(insts6, env6)


    env7 = Env({"+": Op(operator.add)}, "root")
    insts7 = [
        ("push", Func(params=["z"], instructions=[("read", "z"), ("read", "z"), ("read", "+"), ("apply", 2)])),
        ("func", None),
        ("store", "f"),
        ("push", 21),
        ("read", "f"),
        ("apply", None),
        ("push", 37),
        ("read", "f"),
        ("apply", None),
        ("print", None)
    ]
    print "7:", evaluate(insts7, env7)


compile_example()
stack_execute_example()
