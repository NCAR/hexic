import sys


if __name__ == '__main__':
    try:
        import cffi
        sys.stdout.write('TRUE')
    except:
        sys.stdout.write('FALSE')
