!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!######################
MODULE MODI_INTERP_GRID
!######################

INTERFACE INTERP_GRID

MODULE PROCEDURE INTERP_GRID_1D
MODULE PROCEDURE INTERP_GRID_2D
!
END INTERFACE

CONTAINS
!     ##########################################
      SUBROUTINE INTERP_GRID_1D(PZ1,PT1,PZ2,PT2)
!     ##########################################
!!
!!****  *INTERP_GRID* - interpolation on the vertical
!!
!!    PURPOSE
!!    -------
!!
!! input  grid/data is (x,z1)
!! output grid/data is (x,z2)
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!     V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)   :: PT1  ! input temperatures
REAL, DIMENSION(:),   INTENT(IN)   :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)  :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
REAL, DIMENSION(SIZE(PZ1,2)-1) :: ZDIZ1
INTEGER :: JL, JI, JK, JK2 ! loop counter
REAL :: ZTHR
REAL :: ZEPS ! a small number
REAL :: ZCOEFLIN ! interpolation coefficients
INTEGER :: IKLIN    ! lower interpolating level of
INTEGER :: ILEVEL, IS1
!                                                     ! grid 1 for each level of grid 2
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D_1',0,ZHOOK_HANDLE)
!
IS1 = SIZE(PZ1,2)
!
ZEPS=1.E-12
!
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JI,JK,ZDIZ1,JK2,ZTHR,ILEVEL,IKLIN,ZCOEFLIN)
DO JI = 1,SIZE(PZ1,1)
  !
  IF (ANY(PT1(JI,:)==XUNDEF)) THEN
    !
    PT2(JI,:)=XUNDEF
    !
  ELSE
    !
    DO JK = 1,SIZE(PZ1,2)-1
      IF (PZ1(JI,JK)==PZ1(JI,JK+1)) THEN
        ZDIZ1(JK) = 0.
      ELSE
        ZDIZ1(JK) = 1./(PZ1(JI,JK)-PZ1(JI,JK+1))
      ENDIF
    ENDDO
    !
    DO JK2 = 1,SIZE(PZ2)
      !
      ZTHR = PZ2(JK2) * (1.-ZEPS)
      ILEVEL = COUNT(PZ1(JI,:)<=ZTHR)
      !
      IF (ILEVEL < 1 ) THEN    ! no extrapolation
        !
        IKLIN = 1
        ZCOEFLIN = 1.
        !
      ELSE
        !
        !* linear extrapolation
        ILEVEL = MIN(ILEVEL,IS1-1)
        !
        IKLIN = ILEVEL
        !
        ZCOEFLIN = ( PZ2(JK2)-PZ1(JI,ILEVEL+1) ) * ZDIZ1(ILEVEL)
        IF (ILEVEL==IS1-1) ZCOEFLIN = MAX(ZCOEFLIN,0.) ! no extrapolation
        !
      ENDIF
      !
      PT2(JI,JK2) = ZCOEFLIN * PT1(JI,IKLIN) + (1.-ZCOEFLIN) * PT1(JI,IKLIN+1)
      !
    END DO
    !
  ENDIF
!-------------------------------------------------------------------------------
ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_1D
!
!     ##########################################
      SUBROUTINE INTERP_GRID_2D(PZ1,PT1,PZ2,PT2)
!     ##########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1  ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
REAL, DIMENSION(SIZE(PZ1,2)-1) :: ZDIZ1
REAL :: ZTHR
REAL :: ZEPS ! a small number
REAL :: ZCOEFLIN ! interpolation coefficients
INTEGER :: JI, JK, JK2 ! loop counter
INTEGER :: IKLIN    ! lower interpolating level of
INTEGER :: ILEVEL, IS1
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D_1',0,ZHOOK_HANDLE)
!
IS1 = SIZE(PZ1,2)
!
ZEPS=1.E-12
!
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JI,JK,ZDIZ1,JK2,ZTHR,ILEVEL,IKLIN,ZCOEFLIN)
DO JI = 1,SIZE(PZ1,1)
  !
  IF (ANY(PT1(JI,:)==XUNDEF)) THEN
    !
    PT2(JI,:)=XUNDEF
    !
  ELSE
    !
    DO JK = 1,SIZE(PZ1,2)-1
      IF (PZ1(JI,JK)==PZ1(JI,JK+1)) THEN
        ZDIZ1(JK) = 0.
      ELSE
        ZDIZ1(JK) = 1./(PZ1(JI,JK)-PZ1(JI,JK+1))
      ENDIF
    ENDDO
    !
    DO JK2 = 1,SIZE(PZ2,2)
      !
      ZTHR = PZ2(JI,JK2) * (1.-ZEPS)
      ILEVEL = COUNT(PZ1(JI,:)<=ZTHR)
      !
      IF (ILEVEL < 1 ) THEN
        !
        IKLIN = 1
        ZCOEFLIN = 1.                       ! no extrapolation
        !
      ELSE
        !
        !* linear extrapolation
        ILEVEL = MIN(ILEVEL,IS1-1)
        IKLIN = ILEVEL
        !
        ZCOEFLIN = ( PZ2(JI,JK2)-PZ1(JI,ILEVEL+1) ) * ZDIZ1(ILEVEL)
        IF (ILEVEL==IS1-1) ZCOEFLIN = MAX(ZCOEFLIN,0.) ! no extrapolation
        !
      ENDIF
      !
      PT2(JI,JK2) = ZCOEFLIN * PT1(JI,IKLIN) + (1.-ZCOEFLIN) * PT1(JI,IKLIN+1)
      !
    END DO
    !
  ENDIF
!-------------------------------------------------------------------------------
ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_2D
!
END MODULE MODI_INTERP_GRID
