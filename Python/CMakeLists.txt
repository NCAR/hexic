find_package(PythonInterp)

if (PYTHONINTERP_FOUND)
  execute_process(
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/find_cffi.py
    OUTPUT_VARIABLE CFFI_FOUND
  )
  if (CFFI_FOUND)
    message(STATUS "Building Python bindings")
    set(BUILD_PYTHON_BINDINGS "TRUE")
  else ()
    message(STATUS "Not building Python bindings because cffi Python package not found")
  endif ()
else ()
  message(STATUS "Not building Python bindings because Python not found")
endif ()

if (BUILD_PYTHON_BINDINGS)
  # TODO: call build_bindings.py
endif ()
