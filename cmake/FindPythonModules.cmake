function(find_python_module module)
  string(TOUPPER ${module} module_upper)

  if (NOT PY_${module_upper})
    if(ARGC GREATER 1 AND ARGV1 STREQUAL "REQUIRED")
      set(${module}_FIND_REQUIRED TRUE)
    endif ()

    # A module's location is usually a directory, but for binary modules it's a
    # .so or .dll file.
    execute_process(
      COMMAND "${PYTHON_EXECUTABLE}" "-c" "import re, ${module}; print re.compile('/__init__.py.*').sub('',${module}.__file__)"
      RESULT_VARIABLE _${module}_status
      OUTPUT_VARIABLE _${module}_location
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    if (NOT _${module}_status)
      set(PY_${module_upper} ${_${module}_location} CACHE STRING
          "Location of Python module ${module}")
    endif ()
  endif ()

  find_package_handle_standard_args(PY_${module}
                                    FOUND_VAR PY_${module_upper}_FOUND
                                    REQUIRED_VARS PY_${module_upper}
                                    FAIL_MESSAGE "${module} not found")
  # not sure why I really need this, but PY_${module_upper}_FOUND is not found
  # in the calling scope without it
  set(PY_${module_upper}_FOUND ${PY_${module_upper}_FOUND} PARENT_SCOPE)
endfunction ()
