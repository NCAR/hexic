include(FortranCInterface)
FortranCInterface_HEADER(FC.h
                         MACRO_NAMESPACE "FC_"
                         SYMBOLS run_hexic)

install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/hexic.h"
        DESTINATION include)
