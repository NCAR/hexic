find_package(PythonInterp)

if (PYTHONINTERP_FOUND)
  find_package(PythonLibs)
  include(${CMAKE_SOURCE_DIR}/cmake/FindPythonModules.cmake)
  find_python_module(numpy REQUIRED)

  if (PY_NUMPY_FOUND)
    message(STATUS "Building Python bindings")
    set(BUILD_PYTHON_BINDINGS "TRUE")
  else ()
    message(STATUS "Not building Python bindings because NumPy not found")
  endif ()
else ()
  message(STATUS "Not building Python bindings because Python not found")
endif ()

if (BUILD_PYTHON_BINDINGS)
  include_directories(${PYTHON_INCLUDE_DIRS} ${PY_NUMPY}/core/include)
  add_library(hexic-Python-bindings SHARED hexic.c)
  target_link_libraries(hexic-Python-bindings chexic-static)

  if (APPLE)
    set_target_properties(hexic-Python-bindings
      PROPERTIES
        LINK_FLAGS "-undefined dynamic_lookup"
        SUFFIX ".so"
    )
  endif ()
  set_target_properties(hexic-Python-bindings
    PROPERTIES
      OUTPUT_NAME "hexic"
      PREFIX ""
  )

  install(TARGETS hexic-Python-bindings
          RUNTIME DESTINATION lib
          LIBRARY DESTINATION lib
  )
  install(FILES hexic_example.py
          DESTINATION examples)
endif ()


