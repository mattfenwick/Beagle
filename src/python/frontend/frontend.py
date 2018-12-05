from . import parser
from . import builder
from unparse import run
import json


def parse(text):
    return run(parser.beagle, text, (1,1))

def build_ast(cst):
    return builder.build_ast(cst)

def parse_ast(text):
    maybe_cst = parse(text)
#    console.log('maybeCST: ' + JSON.stringify(maybeCST));
    if maybe_cst.status != 'success':
        return maybe_cst
    ast = builder.build_ast(maybe_cst.value['result'])
#    console.log('ast: ' + JSON.stringify(ast));
    return ast

def parse_ast_from_file(path):
    with open(path, 'r') as f:
            text = f.read()
    return parse_ast(text)

def dump(ast, indent=None):
    return json.dumps(ast, default=default_json_serializer, indent=indent)

def default_json_serializer(o):
    if isinstance(o, set):
        return list(o)
#    if isinstance(o, datetime.datetime):
#        return str(o)
    return o.__dict__
