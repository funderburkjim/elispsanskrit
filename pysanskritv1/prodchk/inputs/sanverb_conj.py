"""sanverb_conj.py
   Aug 30, 2016
   modify dhaval/conjtab/conj_xxx.txt to outputs/sanverb_pre.txt
   and simultaneously generate inputs/sanverb_pre.txt.
   The main difficulty is to associate 
      verbwithanubandha+gana+number+pada (as in conj_xxx.txt) to
      verbwithoutanubandha+gana+pada 
"""
import codecs,sys,re

