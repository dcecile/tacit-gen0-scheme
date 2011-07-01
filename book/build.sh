#!/bin/sh
# Requires texlive-core and texlive-latexextra
xelatex -no-pdf master \
&& makeindex master \
&& bibtex master \
&& xelatex -no-pdf master \
&& xelatex master
