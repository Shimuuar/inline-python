"""
Extract variable names to be bound by haskell code
"""
import ast
import sys
import re
import base64

mode  = sys.argv[1]
is_hs = re.compile('.*_hs$')

def extract_hs_vars(code):
    for node in ast.walk(code):
        if isinstance(node, ast.Name) and is_hs.match(node.id):
            yield node.id

def print_hs_vars(src):
    code = ast.parse(src, '<interactive>', mode)
    for nm in set(extract_hs_vars(code)):
        print(nm)

def decode_and_print(codeB64):
    print_hs_vars(base64.b16decode(codeB64, casefold=True).decode('utf8'))
