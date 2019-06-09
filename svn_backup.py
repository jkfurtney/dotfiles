
# look for modified or added files in SVN, make a copy of them

import subprocess
import shutil
import os
import datetime

new_dir = '{:%Y-%m-%d_%H_%M%S}'.format(datetime.datetime.now())
print (new_dir)
os.mkdir(new_dir)

apply_patch = ""
for line in subprocess.check_output("svn status").decode("utf-8").split("\r\n"):
    if line.startswith("M") or line.startswith("A") or line.startswith("?"):
        fname = line[1:].strip()
        if os.path.isdir(fname):
            continue
        new_name = os.path.join(new_dir, fname.replace("\\","_"))
        print( fname, new_name)
        apply_patch += "cp {} {}\n".format(os.path.abspath(new_name),
                                         os.path.abspath(fname))
        shutil.copyfile(fname, new_name)

with open(os.path.join(new_dir, "apply.sh"), "w") as f:
    f.write(apply_patch)
