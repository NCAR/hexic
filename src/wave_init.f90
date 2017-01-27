SUBROUTINE WAVE_INIT
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  USE CONS_PARAM
  USE LINE_PARAM

  IMPLICIT NONE

  INTEGER              :: I

  ! deallocate wave if already allocated from a previous run
  if (allocated(wave)) then
    deallocate(wave)
  endif

  ALLOCATE(WAVE(NUMW))

  DO I=1,NUMW
     WAVE(I)=LAM_START+DBLE(I-1)*STEPW
  ENDDO
  !
END SUBROUTINE WAVE_INIT
