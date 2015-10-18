""" sexp.py
  ref: http://probablyprogramming.com/2009/11/23/a-simple-lisp-parser-in-python
"""
from string import whitespace

atom_end = set('()"\'') | set(whitespace)

def parse(sexp):
 stack, i, length = [[]], 0, len(sexp)
 while i < length:
  c = sexp[i]

  #print c, stack
  reading = type(stack[-1])
  if reading == list:
   if   c == '(': stack.append([])
   elif c == ')': 
    try:
     stack[-2].append(stack.pop())
    except:
     print "sexp ERROR with i=",i,"and c=",c
     print "sexp=\n",sexp
     exit(1)
    if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
   elif c == '"': stack.append('')
   elif c == "'": stack.append([('quote',)])
   elif c in whitespace: pass
   else: stack.append((c,))
  elif reading == str:
   if   c == '"': 
    stack[-2].append(stack.pop())
    if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
   elif c == '\\': 
    i += 1
    stack[-1] += sexp[i]
   else: stack[-1] += c
  elif reading == tuple:
   if c in atom_end:
    atom = stack.pop()
    if atom[0][0].isdigit(): 
     # ejf no special treatment
     #stack[-1].append(eval(atom[0])) 
     stack[-1].append(atom)
    else: stack[-1].append(atom)
    if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
    continue
   else: stack[-1] = ((stack[-1][0] + c),)
  i += 1
 return stack.pop()

if __name__ == "__main__":
 import sys
 s = sys.argv[1]
 x = parse(s)
 #print x
 x
