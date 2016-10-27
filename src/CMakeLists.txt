find_package(LAPACK REQUIRED)
find_package(OpenMP_Fortran)

set(main_src
    main.f90)

set(f90_srcs
    cons_param.f90
    factorial.f90
    forward.f90
    free_init.f90
    input_param.f90
    inv_init.f90
    inv_param.f90
    inv_utils.f90
    line_param.f90
    new_invert.f90
    ran_mod.f90
    read_filt.f90
#    read_free.f90
    read_input.f90
    read_line.f90
#    read_model.f90
    read_noise.f90
#    read_obs.f90
    read_scat.f90
    read_weights.f90
    run_hexic.f90
    solve_svd.f90
    wave_init.f90
    wfa_guess.f90
#    write_atmos.f90
    write_filters.f90
#    write_syn.f90
    zeeman.f90)

set(f_srcs
    voigt.f)

set(c_wrapper
    hexic.c)

link_libraries(${LAPACK_LIBRARIES})

add_library(hexic-objects OBJECT
            ${f_srcs}
            ${f90_srcs})

set_target_properties(hexic-objects
  PROPERTIES
    POSITION_INDEPENDENT_CODE True
    COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
    LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
)

add_library(chexic-shared SHARED ${c_wrapper} $<TARGET_OBJECTS:hexic-objects>)
set_target_properties(chexic-shared
  PROPERTIES
    OUTPUT_NAME libchexic
    PREFIX ""
    COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
    LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
)

add_library(chexic-static STATIC ${c_wrapper} $<TARGET_OBJECTS:hexic-objects>)
set_target_properties(chexic-static
  PROPERTIES
    OUTPUT_NAME libchexic
    PREFIX ""
    POSITION_INDEPENDENT_CODE True
    COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
    LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
)

add_library(hexic-shared SHARED $<TARGET_OBJECTS:hexic-objects>)
set_target_properties(hexic-shared
  PROPERTIES
    OUTPUT_NAME libhexic
    PREFIX ""
    COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
    LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
)

add_library(hexic-static STATIC $<TARGET_OBJECTS:hexic-objects>)
set_target_properties(hexic-static
  PROPERTIES
    OUTPUT_NAME libhexic
    PREFIX ""
    COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
    LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
)

install(TARGETS chexic-shared
        DESTINATION lib)
install(TARGETS chexic-static
        DESTINATION lib)
install(TARGETS hexic-shared
        DESTINATION lib)
install(TARGETS hexic-static
        DESTINATION lib)
