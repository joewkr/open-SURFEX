!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######################
MODULE MODI_INTERP_GRID_NAT
!######################

INTERFACE INTERP_GRID_NAT

SUBROUTINE INTERP_GRID_NAT_1D(PDG1,PT1,PDG2,PT2)
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PDG1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1   ! input temperatures
REAL, DIMENSION(:),   INTENT(IN)  :: PDG2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)  :: PT2  ! output temperatures
!
END SUBROUTINE INTERP_GRID_NAT_1D
!
SUBROUTINE INTERP_GRID_NAT_2D(PDG1,PT1,PDG2,PT2)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PDG1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN) :: PT1   ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN) :: PDG2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
END SUBROUTINE INTERP_GRID_NAT_2D
!
END INTERFACE

END MODULE MODI_INTERP_GRID_NAT

!     ##########################################
      SUBROUTINE INTERP_GRID_NAT_1D(PDG1,PT1,PDG2,PT2)
!     ##########################################
!
USE MODI_VERTICAL_GRID_NAT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PDG1 ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)   :: PT1  ! input temperatures
REAL, DIMENSION(:),   INTENT(IN)   :: PDG2 ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)  :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
REAL, DIMENSION(SIZE(PT2,1),SIZE(PT2,2)) :: ZDG2
!
INTEGER :: JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID_NAT:INTERP_GRID_NAT_1D',0,ZHOOK_HANDLE)
!
DO JL=1,SIZE(PT2,2)
  ZDG2(:,JL) = PDG2(JL)
ENDDO
!
CALL VERTICAL_GRID_NAT(PDG1,PT1,ZDG2,PT2)
!
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID_NAT:INTERP_GRID_NAT_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_NAT_1D
!
!     ##########################################
      SUBROUTINE INTERP_GRID_NAT_2D(PDG1,PT1,PDG2,PT2)
!     ##########################################
!
USE MODI_VERTICAL_GRID_NAT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PDG1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1  ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN)  :: PDG2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID_NAT:INTERP_GRID_NAT_2D',0,ZHOOK_HANDLE)
!
CALL VERTICAL_GRID_NAT(PDG1,PT1,PDG2,PT2)
!
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID_NAT:INTERP_GRID_NAT_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_NAT_2D
