set(HEXIC_IDL_BINDINGS idlhexic)
configure_file("hexic.dlm.in" "${HEXIC_IDL_BINDINGS}.dlm")
add_library(hexic-IDL-bindings SHARED hexic.c)

find_package(IDL REQUIRED)

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

include_directories(${IDL_INCLUDE_DIR})
target_link_libraries(hexic-IDL-bindings ${IDL_LIBRARY})

install(TARGETS hexic-IDL-bindings
        RUNTIME DESTINATION lib
        LIBRARY DESTINATION lib
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${HEXIC_IDL_BINDINGS}.dlm"
        DESTINATION lib)