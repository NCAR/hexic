import sys


if __name__ == '__main__':
    try:
        import cffi
        import numpy
        sys.stdout.write('TRUE')
    except ImportError:
        sys.stdout.write('FALSE')
