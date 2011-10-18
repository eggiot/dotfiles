#!/bin/bash
# requires: wget cvs

## SLIME

rm -R -i slime

cvs -d :pserver:anonymous@common-lisp.net:/project/slime/cvsroot login

# password is "anonymous"

cvs -d :pserver:anonymous@common-lisp.net:/project/slime/cvsroot checkout slime


## anything

wget http://www.emacswiki.org/emacs/download/anything.el

wget http://www.emacswiki.org/emacs/download/anything-show-completion.el

wget http://www.emacswiki.org/emacs/download/anything-ipython.el


## autopair

wget http://autopair.googlecode.com/svn/trunk/autopair.el


## ido

wget http://www.cua.dk/ido.el


## Python

wget http://ipython.scipy.org/dist/ipython.el #ipython

wget http://dishevelled.net/elisp/lambda-mode.el #lambda

wget https://raw.github.com/gist/302847/3331473995b55cc578e7d63dd82474749367c29c/python-pep8.el #pep8

wget https://raw.github.com/gist/302848/60961ad1134e7bec5d836857fb67109245548dad/python-pylint.el #pylint


## mc

wget http://www.emacswiki.org/emacs-fr/download/mc.el

## maxframe

wget https://github.com/rmm5t/maxframe.el/raw/master/maxframe.el