set(BINDINGS_NAME idlhexic)
configure_file("hexic.dlm.in" "${BINDINGS_NAME}.dlm")
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
    OUTPUT_NAME "${BINDINGS_NAME}"
    PREFIX ""
)

include_directories(${IDL_INCLUDE_DIR})
target_link_libraries(hexic-IDL-bindings ${IDL_LIBRARY})

install(TARGETS hexic-IDL-bindings
  RUNTIME DESTINATION lib
  LIBRARY DESTINATION lib
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/hexic.dlm" DESTINATION lib)