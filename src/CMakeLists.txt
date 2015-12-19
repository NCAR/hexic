find_package(LAPACK REQUIRED)

enable_language(Fortran)

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
    main.f90
    new_invert.f90
    ran_mod.f90
    read_filt.f90
    read_free.f90
    read_input.f90
    read_line.f90
    read_model.f90
    read_noise.f90
    read_obs.f90
    read_scat.f90
    read_weights.f90
    solve_svd.f90
    wave_init.f90
    wfa_guess.f90
    write_atmos.f90
    write_filters.f90
    write_syn.f90
    zeeman.f90)

set(f_srcs
    voigt.f)

link_libraries(${LAPACK_LIBRARIES})

add_executable(hexic ${f_srcs} ${f90_srcs})
