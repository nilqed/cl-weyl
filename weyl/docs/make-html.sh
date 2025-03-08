#!/bin/sh
for a in *.txt; do txt2html -p 0 $a > $(basename $a .txt).html ; done

