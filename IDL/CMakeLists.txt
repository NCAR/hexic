set(HEXIC_IDL_BINDINGS idlhexic)
configure_file("hexic.dlm.in" "${HEXIC_IDL_BINDINGS}.dlm")

find_package(IDL)

if (IDL_FOUND)
  if (IDL_LICENSED)
    message(STATUS "Building IDL bindings")
    set(BUILD_IDL_BINDINGS "TRUE")
  else ()
    message(STATUS "Not building IDL bindings because IDL not licensed")
  endif ()
else ()
  message(STATUS "Not building IDL bindings because IDL not found")
endif ()


if (BUILD_IDL_BINDINGS)
  add_library(hexic-IDL-bindings SHARED hexic.c)

  if (UNIX)
    set_target_properties(hexic-IDL-bindings
      PROPERTIES
        SUFFIX ".${IDL_PLATFORM_EXT}.so"
    )
  endif ()

  set_target_properties(hexic-IDL-bindings
    PROPERTIES
      OUTPUT_NAME "${HEXIC_IDL_BINDINGS}"
      PREFIX ""
  )

  include_directories(${IDL_INCLUDE_DIR} ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries(hexic-IDL-bindings chexic-static ${IDL_LIBRARY})

  install(TARGETS hexic-IDL-bindings
          RUNTIME DESTINATION lib
          LIBRARY DESTINATION lib
  )
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${HEXIC_IDL_BINDINGS}.dlm"
          DESTINATION lib)
  install(FILES hexic_invert_example.pro
          DESTINATION examples)
  install(FILES observations.sav
          DESTINATION examples)
endif ()
