!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DSLT_INIT_MODES (KEQ, KSV_BEG, KSV_END, OVARSIG, ORGFIX, &
                            KMDEBEG, KMDE)
!!    ###########################################
!!
!!*** *DSLT_INIT_MODES*
!!
!!    PURPOSE
!!    -------
!!    Find the number of dust modes to be transported
!!    Each mode needs 3 moments to be described, so logically, the number of modes is
!!    The number of dust tracers divided by 3
!!     
!!
!!    REFERENCE
!!    ---------
!!    Modified dst_init_names (march 2005)    
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
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!

INTEGER,                         INTENT(IN) :: KEQ       ! number of dust variables
INTEGER,                         INTENT(IN) :: KSV_BEG   ! First number of dust tracer
INTEGER,                         INTENT(IN) :: KSV_END   ! Last number of dust tracer
LOGICAL,                         INTENT(IN) :: OVARSIG   ! type of standard deviation (fixed or variable)
LOGICAL,                         INTENT(IN) :: ORGFIX    ! type of mean radius
INTEGER,                         INTENT(OUT) :: KMDEBEG  ! Place in scalar list of dustmass in first mode
INTEGER,                         INTENT(OUT) :: KMDE     ! Number of dust modes
REAL(KIND=JPRB) :: ZHOOK_HANDLE


!Check if you have a multiple of 3 dust related variables, and 
!Set the number of modes to the number of dust related variables
!divided by 3
IF (LHOOK) CALL DR_HOOK('DSLT_INIT_MODES',0,ZHOOK_HANDLE)
!
KMDEBEG = KSV_BEG
KMDE    = KSV_END - KSV_BEG + 1
!
IF (OVARSIG) THEN !case three moments by modes
  IF(MOD(KMDE,3).NE.0.) THEN
    CALL ABOR1_SFX('DST_INIT_MODES: (1) WRONG NUMBER OF DUST VARIABLES')
  ELSE
    KMDE = KMDE / 3
  ENDIF
ELSE IF (.NOT.ORGFIX) THEN ! case two moment by modes
  IF(MOD(KMDE,2).ne.0.)THEN
   CALL ABOR1_SFX('DST_INIT_MODES: (1) WRONG NUMBER OF DUST VARIABLES')
  ELSE
    KMDE = KMDE / 2
  END IF
END IF
!
IF (LHOOK) CALL DR_HOOK('DSLT_INIT_MODES',1,ZHOOK_HANDLE)
!
END SUBROUTINE DSLT_INIT_MODES
