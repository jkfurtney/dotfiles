"""This is a helper program for using git as a front end to SVN.

The SubWCRev program is used to embed the revision number into a
source code file. This is called by the build process in some
environments. If you are using git as an svn front end execuiting this
command gives an error and stops the build.

This program wraps the SubWCRev program to work for both svn and git
repos. When called it detects if the file is in a git or svn
repository and does the appropriate thing. In svn it calls the
SubWCRev command, in git it just substitutes a dummy value for the
revision number.

in
C:\Program Files\TortoiseSVN\bin rename SubWCRev.exe to SubWCRev_old.exe

create the file C:\Program Files\TortoiseSVN\bin\SubWCRev.bat
with the single line:
python "c:\Program Files\TortoiseSVN\bin\smart_rev.py" %*

copy this file (smart_rev.py) to C:\Program Files\TortoiseSVN\bin

"""

import sys
import os

GIT, SVN = 0, 1
mode = 0

print "smart_rev.py got {}".format(sys.argv)
test_path = sys.argv[1]
source_file = sys.argv[2]
destination_file = sys.argv[3]

assert os.path.isdir(test_path), "specified path is not a directory."

test_path = os.path.join(test_path, ".svn")

if os.path.isdir(test_path):
    mode = SVN
else:
    mode = GIT


if mode == SVN:
    print "SVN mode"

elif mode == GIT:
    print "GIT mode"

    with open(source_file, "r") as infile:
        with open(destination_file, "w") as outfile:
            for line in infile.readlines():
                line = line.replace("$WCMODS?true:false$", "true")
                line = line.replace("$WCREV$", "123456")
                print >> outfile, line
