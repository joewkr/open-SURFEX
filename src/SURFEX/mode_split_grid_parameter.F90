!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!##################
MODULE MODE_SPLIT_GRID_PARAMETER
!##################
!
CONTAINS
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERX1(HPROGRAM,HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT,KIMAX_ll,KJMAX_ll,KHALO)
!     #############################################################
!
!!****  * - routine to split a real array on the splitted grid
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_OL
USE MODE_SPLIT_GRID_PARAMETER_OL
#endif
#ifdef SFX_MNH
USE MODI_SPLIT_GRID_PARAMETERX1_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),       INTENT(IN) :: HPROGRAM    ! calling program
 CHARACTER(LEN=10),      INTENT(IN) :: HGRID       ! grid type
 CHARACTER(LEN=6),       INTENT(IN) :: HREC        ! name of the parameter
INTEGER,                INTENT(IN) :: KDIM        ! size of PFIELD
INTEGER,                INTENT(IN) :: KSIZE       ! size of PFIELD_SPLIT
REAL, DIMENSION(KDIM ), INTENT(IN) :: PFIELD      ! real field for complete grid
REAL, DIMENSION(KSIZE), INTENT(OUT):: PFIELD_SPLIT! real field for splitted grid
!
INTEGER, OPTIONAL,      INTENT(IN) :: KIMAX_ll    !(global) dimension of the domain - X direction
INTEGER, OPTIONAL,      INTENT(IN) :: KJMAX_ll    !(global) dimension of the domain - Y direction
INTEGER, OPTIONAL,      INTENT(IN) :: KHALO ! size of the Halo
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERX1',0,ZHOOK_HANDLE)
!
IF (HPROGRAM=='MESONH') THEN
  IF (PRESENT(KIMAX_ll).AND.PRESENT(KJMAX_ll).AND.PRESENT(KHALO)) THEN
#ifdef MNH_PARALLEL
    CALL SPLIT_GRID_PARAMETERX1_MNH(HGRID,HREC,KDIM,KSIZE,KIMAX_ll,KJMAX_ll,KHALO,PFIELD,PFIELD_SPLIT)
#endif
  ELSE
#ifdef SFX_MNH
    CALL SPLIT_GRID_PARAMETERX1_MNH(HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
#endif
  ENDIF
ENDIF
!
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL SPLIT_GRID_PARAMETERX1_OL(HPROGRAM,HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERX1',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERX1
!
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERN0(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_SPLIT,KHALO)
!     #############################################################
!
!!****  * - routine to define an integer related to splitted grid
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_OL
USE MODE_SPLIT_GRID_PARAMETER_OL
#endif
#ifdef SFX_MNH
USE MODI_SPLIT_GRID_PARAMETERN0_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=10), INTENT(IN) :: HGRID        ! grid type
 CHARACTER(LEN=6),  INTENT(IN) :: HREC         ! name of the parameter
INTEGER,            INTENT(IN) :: KFIELD       ! integer scalar for complete grid
INTEGER,            INTENT(OUT):: KFIELD_SPLIT ! integer scalar for splitted grid
!
INTEGER, OPTIONAL,  INTENT(IN) :: KHALO ! size of the Halo

!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERN0',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
IF (HPROGRAM=='MESONH') THEN
  IF (PRESENT(KHALO)) THEN
#ifdef MNH_PARALLEL
    CALL SPLIT_GRID_PARAMETERN0_MNH(HGRID,HREC,KHALO,KFIELD,KFIELD_SPLIT)
#endif
  ELSE
#ifdef SFX_MNH
    CALL SPLIT_GRID_PARAMETERN0_MNH(HGRID,HREC,KFIELD,KFIELD_SPLIT)
#endif
  ENDIF
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef SFX_OL
  CALL SPLIT_GRID_PARAMETERN0_OL(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_SPLIT)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER:SPLIT_GRID_PARAMETERN0',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERN0
!
END MODULE MODE_SPLIT_GRID_PARAMETER
