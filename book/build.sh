#!/bin/sh
xelatex -no-pdf master \
&& makeindex master \
&& bibtex master \
&& xelatex -no-pdf master \
&& xelatex master
