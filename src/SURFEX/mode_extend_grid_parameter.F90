!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODE_EXTEND_GRID_PARAMETER
!##################
!
CONTAINS
!
!    Author
!  M.Moge  01/03/2015 
!
!     #############################################################
      SUBROUTINE EXTEND_GRID_PARAMETERX1(HPROGRAM,HGRID,HREC,KDIM,KSIZE,KIMAX_ll,KJMAX_ll,PFIELD,PFIELD_EXTEND)
!     #############################################################
!
!!****  * - routine to extend a real splitted array on SURFEX halo
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
!USE MODE_EXTEND_GRID_PARAMETER_OL
#endif
#ifdef MNH
USE MODI_EXTEND_GRID_PARAMETERX1_MNH
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
INTEGER,                INTENT(IN) :: KSIZE       ! size of PFIELD_EXTEND
INTEGER,                INTENT(IN) :: KIMAX_ll    !(global) dimension of the domain - X direction
INTEGER,                INTENT(IN) :: KJMAX_ll    !(global) dimension of the domain - Y direction
REAL, DIMENSION(KDIM ), INTENT(IN) :: PFIELD      ! real field for complete grid
REAL, DIMENSION(KSIZE), INTENT(OUT):: PFIELD_EXTEND! real field for splitted grid
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_EXTEND_GRID_PARAMETER:EXTEND_GRID_PARAMETERX1',0,ZHOOK_HANDLE)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL EXTEND_GRID_PARAMETERX1_MNH(HGRID,HREC,KDIM,KSIZE,KIMAX_ll,KJMAX_ll,PFIELD,PFIELD_EXTEND)
#endif
ENDIF
!
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
!  CALL EXTEND_GRID_PARAMETERX1_OL(HPROGRAM,HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_EXTEND)
!TODO : write subroutine EXTEND_GRID_PARAMETERX1_OL
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_EXTEND_GRID_PARAMETER:EXTEND_GRID_PARAMETERX1',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE EXTEND_GRID_PARAMETERX1
!
!
!     #############################################################
      SUBROUTINE EXTEND_GRID_PARAMETERN0(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_EXTEND)
!     #############################################################
!
!!****  * - routine to "extend" a integer related to splitted grid on SURFEX halo
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef OL
!USE MODE_EXTEND_GRID_PARAMETER_OL
#endif
#ifdef MNH
USE MODI_EXTEND_GRID_PARAMETERN0_MNH
#endif
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=10), INTENT(IN) :: HGRID        ! grid type
 CHARACTER(LEN=6),  INTENT(IN) :: HREC         ! name of the parameter
INTEGER,           INTENT(IN) :: KFIELD       ! integer scalar for complete grid
INTEGER,           INTENT(OUT):: KFIELD_EXTEND ! integer scalar for splitted grid
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_EXTEND_GRID_PARAMETER:EXTEND_GRID_PARAMETERN0',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
IF (HPROGRAM=='MESONH') THEN
#ifdef MNH
  CALL EXTEND_GRID_PARAMETERN0_MNH(HGRID,HREC,KFIELD,KFIELD_EXTEND)
#endif
ENDIF
!
IF (HPROGRAM=='OFFLIN') THEN
#ifdef OL
!  CALL EXTEND_GRID_PARAMETERN0_OL(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_EXTEND)
!TODO : write subroutine EXTEND_GRID_PARAMETERN0_OL
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_EXTEND_GRID_PARAMETER:EXTEND_GRID_PARAMETERN0',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE EXTEND_GRID_PARAMETERN0
!
END MODULE MODE_EXTEND_GRID_PARAMETER
