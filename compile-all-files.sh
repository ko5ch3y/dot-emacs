#!/bin/sh

FTC=files-to-compile.txt

# adapted from org-mode Makefile
EMACS=emacs
prefix=/usr
lispdir=$(prefix)/share/emacs/site-lisp

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \"./lisp/\") (cons \"$(lispdir)\" load-path)))"

find -type f -name '*.el' > $FTC
# emacs -batch -f batch-byte-compile `cat $FTC`
$(BATH) `cat $FTC`
rm -f $FTC

