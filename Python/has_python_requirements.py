import sys


if __name__ == '__main__':
    try:
        import cffi
        import numpy
    except ImportError:
        sys.stdout.write('FALSE')
    else:
        sys.stdout.write('TRUE')
