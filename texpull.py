#!/usr/bin/python

"""
This script takes one command-line argument which is the name of a
python source code file. Text in the triple quote comment blocks is
removed and written to a temporary file which is then converted to PDF
via LaTeX.
"""

import sys,os
import subprocess
assert len(sys.argv)==2

lines = open(sys.argv[1],'r').readlines()

header = r"""
\documentclass[]{article}
\usepackage{graphicx}
\usepackage{amssymb}
\begin{document}
"""

footer=r"""
\end{document}
"""

base_name = "_"+os.path.splitext(sys.argv[1])[0]
new_filename = base_name + '.tmp_latex'

outfile = open(new_filename,'w')

print >> outfile, header

in_block_comment = 0
for line in lines:
    if line.startswith('"""'):
        if not in_block_comment: in_block_comment =1
        else:                    in_block_comment =0
        continue
    if in_block_comment:
        print >> outfile, line,

print >> outfile, footer
outfile.close()

retcode = subprocess.call(["pdflatex", "-interaction",  "nonstopmode",
                           new_filename])

os.remove(base_name + '.log')
os.remove(base_name + '.aux')
if retcode==0:
    print "pdflatex returned OK"
else:
    print "pdflatex returned an error"
sys.exit(retcode)
