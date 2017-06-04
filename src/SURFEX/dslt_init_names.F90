!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DSLT_INIT_NAMES (KLUOUT, HRC1, HSV, KPMODE, &
                            KEQ, KSV_BEG, KSV_END, OVARSIG, ORGFIX)
!!    ###########################################
!!
!!*** *DSLT_INIT_NAMES*
!!
!!    PURPOSE
!!    -------
!!      Read and filter all chemical species into the CSV array
!!     initialize NSV_CHSBEG and  NSV_CHSEND index for the begin and the ending chemical index
!!     
!!
!!    REFERENCE
!!    ---------
!!    Modified ch_init_names (february 2005)    
!!
!!    AUTHOR
!!    ------
!!    Alf Grini <alf.grini@cnrm.meteo.fr>
!!
!!    MODIFICATIONS
!!    -------------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,                         INTENT(IN)  :: KLUOUT   ! output listing channel
 CHARACTER(LEN=4),                INTENT(IN)  :: HRC1
 CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN)  :: HSV      ! name of chemical species
                                                         ! with character # for chemistry
INTEGER,                         INTENT(OUT) :: KPMODE
INTEGER,                         INTENT(OUT) :: KEQ         ! number of dust related variables
INTEGER,                         INTENT(OUT) :: KSV_BEG     ! first dust related scalar
INTEGER,                         INTENT(OUT) :: KSV_END     ! last  dust related scalar
LOGICAL,                         INTENT(INOUT) :: OVARSIG   ! type of standard deviation
LOGICAL,                         INTENT(INOUT) :: ORGFIX    ! type of mean radius
!
!*      0.2    declarations of local variables
INTEGER :: JSV  !! loop on scalar variables
 CHARACTER(LEN=4) :: YRC1
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

!Initialize output variables
IF (LHOOK) CALL DR_HOOK('DSLT_INIT_NAMES',0,ZHOOK_HANDLE)
!
KEQ  = 0
KSV_BEG = 0
KSV_END = 0
!
DO JSV=1, SIZE(HSV)
  !
  YRC1= HSV(JSV)(1:4)
  !
  IF (YRC1 == HRC1) THEN
    !
    IF (HSV(JSV)(5:5) == '6') OVARSIG = .TRUE.
    IF (HSV(JSV)(5:5) == '0') ORGFIX  = .FALSE.
    !
    KEQ = KEQ + 1
    IF (KEQ == 1) KSV_BEG = JSV
    !
  ENDIF
  !
ENDDO
!
! Set the output list of scalar to the input list of scalars
!
! Get the index of the last dust relevant tracer
KSV_END = KSV_BEG + KEQ - 1
!
! Get number of dust modes. Each mode represents
! 3 moments, so 9 dust tracers represents 3 modes.
! 3 dust tracers represents 1 mode
KPMODE = KSV_END - KSV_BEG + 1
IF (OVARSIG) THEN
  KPMODE = INT(KPMODE / 3.)
ELSE IF (.NOT.ORGFIX) THEN
  KPMODE = INT(KPMODE / 2.)
END IF
!
IF (LHOOK) CALL DR_HOOK('DSLT_INIT_NAMES',1,ZHOOK_HANDLE)
!
END SUBROUTINE DSLT_INIT_NAMES
