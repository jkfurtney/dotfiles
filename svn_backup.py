
# look for modified or added files in SVN, make a copy of them

import subprocess
import shutil
import os
import datetime

new_dir = '{:%Y-%m-%d_%H_%M%S}'.format(datetime.datetime.now())
print new_dir
os.mkdir(new_dir)

for line in subprocess.check_output("svn status").split("\r\n"):
    if line.startswith("M") or line.startswith("A") or line.startswith("?"):
        fname = line[1:].strip()
        if os.path.isdir(fname):
            continue
        new_name = os.path.join(new_dir, fname.replace("\\","_"))
        print fname, new_name
        shutil.copyfile(fname, new_name)
