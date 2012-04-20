import sys

def main():
    sys.stdout.write('%s: Put code in me! :D\n\n' % __file__)


def prefix():
    import sys, os
    return os.path.splitext(os.path.basename(sys.argv[0]))[0]


if __name__ == '__main__':
    main()
