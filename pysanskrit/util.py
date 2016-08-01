
#global flatten
import init

def flattengen(L):
 """ Flatten a list.
  Ref = http://stackoverflow.com/questions/2158395/flatten-an-irregular-list-of-lists-in-python
 """
 if not isinstance(L,list):
  yield L
 for item in L:
   if not isinstance(item,list):
    yield item
    continue
   try:
     #yield from flatten(item)
     #http://stackoverflow.com/questions/17581332/converting-yield-from-statement-to-python-2-7-code
     for bar in flattengen(item):
      yield bar
   except TypeError:
     yield item


def flatten(L):
 # flattens a list
 # If L is not a list, just returns L
 if not isinstance(L,list):
  return L
 try:
  return list(flattengen(L))
 except RuntimeError as err:
  print "flatten error:",err
  print "L=",L
  raise NameError('flatten')

def word_parts(citation):
 """ from gram2.el
   return a pair (x,y) where
   x is a list of strings, each of which is either all vowels or all 
    consonants, and whose concatenation is citation and
   y is a string of 'v' and 'c' corresponding.
   Examples: rAma -> [['r','A','m','a'],'cvcv']
   akza -> [['a','kz','a'],'vcv']
   strI -> [['str','I'],'cv']
 """
 parts=[]
 types=[]
 def typeof(x):
  if x in init.consonant_set:
   return "c"
  else:
   return "v"
 prevtype=None
 prevpart=''
 for x in citation:
  t = typeof(x)
  if not prevtype:
   prevpart=x
   prevtype=t
  elif prevtype == t:
   prevpart = prevpart+x
  else:
   parts.append(prevpart)
   types.append(prevtype)
   prevpart=x
   prevtype=t
 # last one
 parts.append(prevpart)
 types.append(prevtype)
 return [parts,''.join(types)]

