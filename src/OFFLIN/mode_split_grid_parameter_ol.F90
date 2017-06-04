!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODE_SPLIT_GRID_PARAMETER_OL
!##################
!
CONTAINS
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERX1_OL(HPROGRAM,HGRID,HREC,KDIM,KSIZE,&
                                           PFIELD,PFIELD_SPLIT            )
!     #############################################################
!
!!****  * - routine to split a real array on the splitted grid 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
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
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER_OL:SPLIT_GRID_PARAMETERX1_OL',0,ZHOOK_HANDLE)
!
IF (KDIM==KSIZE) THEN
  PFIELD_SPLIT = PFIELD
ELSE
  CALL ABOR1_SFX('ERROR in SPLIT_GRID_OL for grid, '//HGRID//', case need to be coded')
END IF
!
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER_OL:SPLIT_GRID_PARAMETERX1_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERX1_OL
!
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERN0_OL(HPROGRAM,HGRID,HREC,KFIELD,KFIELD_SPLIT)
!     #############################################################
!
!!****  * - routine to define an integer related to splitted grid
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM     ! calling program
 CHARACTER(LEN=10), INTENT(IN) :: HGRID        ! grid type
 CHARACTER(LEN=6),  INTENT(IN) :: HREC         ! name of the parameter
INTEGER,           INTENT(IN) :: KFIELD       ! integer scalar for complete grid
INTEGER,           INTENT(OUT):: KFIELD_SPLIT ! integer scalar for splitted grid
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER_OL:SPLIT_GRID_PARAMETERN0_OL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!* define the integer value for the splitted grid
!
KFIELD_SPLIT = KFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_SPLIT_GRID_PARAMETER_OL:SPLIT_GRID_PARAMETERN0_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERN0_OL
!
END MODULE MODE_SPLIT_GRID_PARAMETER_OL
