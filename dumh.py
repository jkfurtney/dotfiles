import re
from collections import namedtuple
# aws s3 ls --human-readable --summarize s3://icgprojects --recursive > data.txt

S3Object = namedtuple("S3Object", ['date', 'time', 'size', 'name'])

size_map = {'Bytes' : 1,
            'KiB' : 1024,
            'MiB' : 1024**2,
            'GiB' : 1024**3,
            'TiB' : 1024**4,
            'PiB' : 1024**5}

# "2017-01-24 10:06:17    0 Bytes 2-2978-04-task-2/"
data_re = "(....-..-..) (..:..:..)\s+([0-9\.]+) (Bytes|KiB|MiB|GiB|TiB|PiB) (.*)"
data_re = re.compile(data_re)

data = []
count_check = None
size_check = None
with open("data.txt", "r") as f:
    for line in f:
        matches = data_re.match(line)
        if matches:
            date, time, s0, s1, name = matches.groups()
            size = float(s0) * size_map[s1]
            data.append(S3Object(date, time, size, name.strip()))
        else:
            if line.startswith("Total Objects:"):
                count_check = int(line.split()[-1])
            if line.strip().startswith("Total"):
                size_check = line.split(":")[-1]
            else:
                print "Warning non matching line:", line

assert len(data) == count_check
print size_check

top_folders = {}
total_size = 0
for s3obj in data:
    if s3obj.name.count("/") == 1 and s3obj.name[-1] == "/":
        print "top level folder", s3obj.name
        assert not s3obj.name in top_folders
        top_folders[s3obj.name[:-1]] = 0
    elif s3obj.size > 0:
        folder = s3obj.name.split("/")[0]
        assert folder in top_folders, "top level folder error"
        top_folders[folder] += int(s3obj.size)
        total_size += int(s3obj.size)

def data_size(size):
    def fmt(i, scale, suffix):
        return "{:.01f} {}".format(i/float(scale), suffix)
    if size > 1e12:
        return fmt(size, 1e12, "TB")
    if size > 1e9:
        return fmt(size, 1e9, "GB")
    if size > 1e6:
        return fmt(size, 1e6, "MB")
    if size > 1e3:
        return fmt(size, 1e3, "kB")

for k,v in top_folders.iteritems():
    print k, "{:.02f}%".format(100.0*v/total_size), data_size(v)
