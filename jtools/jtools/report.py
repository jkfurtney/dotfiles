__all__ = ['latex_report']

import sys
import os
import re
import subprocess

header = r"""
\documentclass[]{article}
\usepackage{graphicx}
\usepackage{amssymb}
\usepackage[parfill]{parskip}
\setlength{\parindent}{0pt}
\begin{document}
"""

footer=r"""
\end{document}
"""

start = re.compile('\s?r?"""\s?latex')
end = re.compile('\s?"""')


def _extract_latex(lines):
    latex_lines = []
    in_block_comment = 0
    for line in lines:
        if start.match(line):
            in_block_comment = 1
            continue
        elif end.match(line):
            in_block_comment = 0
            continue
        if in_block_comment:
            latex_lines.append(line)
    return latex_lines

def latex_report(name, sources):
    if not name.endswith(".tex"):
        name += ".tex"

    base_name = os.path.splitext(name)[0]
    outfile = open(name,'w')
    print >> outfile, header

    for source in sources:
        for line in _extract_latex(open(source, "r")):
            print >> outfile, line,

    print >> outfile, footer
    outfile.close()


    retcode = subprocess.call(["pdflatex", "-interaction",  "nonstopmode",
                               name])

    os.remove(base_name + '.log')
    os.remove(base_name + '.aux')

    if retcode==0:
        print "pdflatex returned OK"
        os.remove(name)
    else:
        print "pdflatex returned an error"
    return retcode




