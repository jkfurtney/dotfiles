import contextlib

@contextlib.contextmanager
def capture():
    """This function is a context manager which captures what is printed
    to standard out and returns it as a string."""
    import sys
    from cStringIO import StringIO
    oldout,olderr = sys.stdout, sys.stderr
    try:
        out=[StringIO(), StringIO()]
        sys.stdout,sys.stderr = out
        yield out
    finally:
        sys.stdout,sys.stderr = oldout, olderr
        out[0] = out[0].getvalue()
        out[1] = out[1].getvalue()


with capture() as out:
    print "part 1"
print "from 1", out[0]

with capture() as out:
    print "part 2"

print "from 2", out[0]
