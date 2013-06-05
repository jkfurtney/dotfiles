#!/usr/bin/python
import sys, os


str = 'mencoder "mf://' + sys.argv[1] + '*.png" -mf fps=13 -o '+ sys.argv[1] + '.avi -ovc lavc -lavcopts vcodec=mpeg4'

os.system(str)
