from cffi import FFI


ffi = FFI()
ffi.set_source("hexic_bindings", None, library_dirs='../src', include_dirs='../include')
ffi.cdef("""
    int printf(const char *format, ...);
""")

if __name__ == "__main__":
    ffi.compile()