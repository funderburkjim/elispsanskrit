from test1mod import *

def test1main(x):
 print "test1main: x=",x

if __name__ == "__main__":
 import sys
 x = sys.argv[1]
 test1mod(x)
