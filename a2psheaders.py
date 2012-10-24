import glob

template = "a2ps --columns=2 -o j%i.ps -M letter --portrait %s"

for i,f in  enumerate(glob.glob("*.h")):
    print template % (i,f)
