find_package(PythonInterp)

if (PYTHONINTERP_FOUND)
  execute_process(
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/has_python_requirements.py
    OUTPUT_VARIABLE PYTHON_REQUIREMENTS_FOUND
  )
  if (PYTHON_REQUIREMENTS_FOUND)
    message(STATUS "Building Python bindings")
    set(BUILD_PYTHON_BINDINGS "TRUE")
  else ()
    message(STATUS "Not building Python bindings because required Python packages not found")
  endif ()
else ()
  message(STATUS "Not building Python bindings because Python not found")
endif ()

if (BUILD_PYTHON_BINDINGS)
  configure_file(build_bindings.py.in build_bindings.py)
  add_custom_command(
    OUTPUT   hexic_bindings.py
    COMMAND  ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/build_bindings.py)
  add_custom_target(hexic-Python-bindings ALL
    DEPENDS  hexic_bindings.py)
  install(FILES hexic_bindings.py
          DESTINATION lib)
endif ()
