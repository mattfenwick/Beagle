import frontend as f
import sys

text = sys.stdin.read()

print f.dump(f.parse_ast(text), indent=2)

example = """
echo '{def x [(a b c) 1 "qrs" [true] abc]} {cond {{x y}} z}' | python f.py
"""
