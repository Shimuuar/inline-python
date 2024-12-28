"""
Extract variable names to be bound by haskell code
"""
import ast
import sys
import re

mode  = sys.argv[1]
is_hs = re.compile('.*_hs$')
code  = ast.parse(sys.stdin.read(), '<interactive>', mode)

def extract_hs_vars(code):
    for node in ast.walk(code):
        if isinstance(node, ast.Name) and is_hs.match(node.id):
            yield node.id

for nm in extract_hs_vars(code):
    print(nm)
