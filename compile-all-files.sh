#!/bin/sh

FTC=files-to-compile.txt

find -type f -name '*.el' > $FTC
emacs -batch -f batch-byte-compile `cat $FTC`
rm -f $FTC

