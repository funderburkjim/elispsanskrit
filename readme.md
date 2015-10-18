
This repository contains a library of Emacs Elisp functions that perform
various computations of Sanskrit Grammar.  The system was begun in about 2002,
with some revisions in 2006, and currently, in 2015.

The principles of grammar were adapted from reprints of two texts:
* Robert Antoine, A Sanskrit manual for high schools, 1914
* M.R. Kale, A Higher Sanskrit Grammar, 1894.


The general approach was to develop functions which take as inputs material
drawn from a Sanskrit dictionary and which generate inflected forms (declensions
of nominals and conjugations of verbs).  The Sanskrit dictionary used was
that developed in the 1990s by Thomas Malten of Cologne University. 

The Sanskrit texts mentioned above present Sanskrit grammar in what I think 
of as a model-based form.  The lexicon provides information to identify the
model to be applied, and the Sanskrit grammars present the algorithms that
pertain to a given model.  This is likely conceptually different from the
approach of Panini's grammar, although, as I understand it, this approach is
based upon an interpretation of Panini.

The choice of Elisp was made because of its free availability, ease of 
installation, and computational completeness.  

At this current time in 2015, it seems desireable to revisit this earlier work.
There have been substantial improvements in Malten's digitization of lexicons.
I would like to adapt this Elisp work using the Python programming language.
In this adaptation, I would hope to take advantage of the dictionary 
improvements and also to improve and extend the algorithms. (One notable
extension would be for various verb forms: aorist, desiderative, intensives.)
Such improvements would help improve the grammatical accuracy of the
computed forms.

In the present form of this repository, the most accessible parts reside
in the grammar/prod/outputs directory.  This directory contains a large 
number of computed forms in the various text files.  All of the Sanskrit
words in these files are spelled using the SLP1 transliteration system for
Sanskrit. The file readme-batch.org explains how these files were 
constructed.  These constructed forms are generally believed to be accurate,
but it is likely that many undiscovered errors are present. 

The readme-interactive.org file provides several examples of using some of the
Elisp functions interactively, to generate various declensions and
conjugations.

For someone interested in examining the code base itself, the 
files in the static-code-analysis directory would be useful; a brief
explanation appears in the readme file therein.



