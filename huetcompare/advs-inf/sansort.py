import string
# Note 'L' and '|' and 'Z' and 'V' are not present
# Not sure where they go
tranfrom="aAiIuUfFxXeEoOMHkKgGNcCjJYwWqQRtTdDnpPbBmyrlvSzsh"
tranto = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw"
trantable = string.maketrans(tranfrom,tranto)

def slp_cmp_pairs(a,b):
 return slp_cmp(a[1],b[1]) # normalized

def slp_cmp(a,b):
 try:
  a1 = string.translate(a,trantable)
 except:
  print "sansort.san_cmp. Problem with translate. a=",a.encode('utf-8')
  exit(1)
 b1 = string.translate(b,trantable)
 return cmp(a1,b1)

def slp_cmp_key(a):
 return string.translate(a,trantable)

# Assume 'a' is list of strings
#b = sorted(a,cmp=slp_cmp)
