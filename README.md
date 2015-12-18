# HEXIC

## December 10, 2015

I am preparing a version of HEXIC to go into the ChroMag pipeline. I will be
working with Mike Galloy to implement it, but first I want to strip the part
that allows the code to work with spectrograph data. It's going to imply
changing the `read_line`, `main`, and the `vmac` convolution part in the synthesis
code!

The original code was copied from `/Users/rce/HMI/test_standalone/HEXIC` I have
made the following changes:

1. Remove `./USER_FILES/input.txt` the line for `filtergraph/spectrograph`
2. Remove filtergraph/spectrograph question in `read_input.f90`
3. Remove `INSTRUMENT` variable from `input_param.f90`
4. Remove `IF INSTRUMENT .EQ. 'f'` statement from `main.f90` (twice)
5. Remove `IF INSTRUMENT .EQ. 'f'` statements from `wfa_guess.f90`
6. Remove all instances of convolution with `vmac` from `forward.f90`
7. Remove convmac routine from `forward.f90`
8. Remove parameter number 11 from model atmosphere.
9. Remove references to parameter number 11 in `forward.f90`
10. Change dimensions of `MODEL` and `DSYN` from 11 to 10.
11. Change dimensions of `GUESS`, `RES`, `BESTMODEL`, `MODELG`, `LASTGOODMODEL`, `DMODEL`, `BESTMINMODEL`, `DSYN`, `LASTGOODDSYN` from 11 to 10 in `new_invert.f90`
12. Change dimensions from 11 to 10 in inv_param.f90
13. Remove 11th dimension in all vectors in inv_init.f90
14. Change dimensions from 11 to 10 in `free_init.f90`. Change also in loops (`DO` statements)
15. Change all dimensions from 11 to 10 in `inv_utils`. Change SIGMA from 17 to 16. Change the indices of the ERR values in `new_invert` accordingly.
16. Change dimensions in `read_model.f90`
17. Change dimensions and loop in `write_atmos.f90`
18. Change `USER_FILES/atmos_in.txt`, remove last parameter.
19. change dimensions in `read_free.f90`
