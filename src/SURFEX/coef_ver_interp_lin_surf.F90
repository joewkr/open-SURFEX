!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE COEF_VER_INTERP_LIN_SURF(PZ1,PZ2,KKLIN,PCOEFLIN)
!     ###############################################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!     This function computes the interpolation coefficient XCOEFLIN
!     of the level XKLIN of grid PZ1 which is just under the points of
!     grid PZ2 (respectively called hereafter 'initial' and 'target'),
!     in order to perform linear interpolations between these 2 grids.
!
!     CAUTION:
!     * The interpolation occurs on the WHOLE grid. Therefore, one must
!     only give as argument to this function the inner points of the domain,
!     particularly for the vertical grid, where there is no physical information
!     under the ground or at and over H.
!     * The level numbers must increase from bottom to top.
!!
!!**  METHOD
!!    ------
!!    two extrapolations are possible: with the two or four nearest points.
!!
!!   Interpolation with 2 points:
!!
!!    If there is less than two points on one side, the interpolation is linear.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/07/97
!!                  20/01/98  use explicit arguments
!!      P Jabouille 20/12/02  no extrapolation under the ground
!!      S. Malardel 11/2003   bug of no extrapolation under the ground
!!      V. Masson   10/2003   no extrapolation above top
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:), INTENT(IN)   :: PZ1   ! altitudes of the points of the
!                                             ! initial grid
REAL,   DIMENSION(:,:), INTENT(IN)   :: PZ2   ! altitudes of the points of the
!                                             ! target grid
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KKLIN ! number of the level
                                              ! of the data to be interpolated
!
REAL,    DIMENSION(:,:), INTENT(OUT):: PCOEFLIN ! interpolation
                                              ! coefficient
!
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL, DIMENSION(SIZE(PZ1,1),SIZE(PZ1,2)-1) :: ZDIZ1
INTEGER, DIMENSION(SIZE(PZ1,1),SIZE(PZ2,2)) :: ILEVEL
INTEGER  :: JK, JK2,JI, IS1
INTEGER  :: ILEV
REAL :: ZTHR
REAL :: ZEPS ! a small number
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
ZEPS=1.E-12
!
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_1',0,ZHOOK_HANDLE)
!$OMP PARALLEL DO PRIVATE(JI,JK)
DO JK = 1,SIZE(PZ1,2)-1
  DO JI = 1,SIZE(PZ1,1)
    IF (PZ1(JI,JK)==PZ1(JI,JK+1)) THEN
      ZDIZ1(JI,JK) = 0.
    ELSE
      ZDIZ1(JI,JK) = 1./(PZ1(JI,JK)-PZ1(JI,JK+1))
    ENDIF
  ENDDO
ENDDO
!$OMP END PARALLEL DO
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_1',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       2.    LOOP ON THE TARGET VERTICAL GRID
!              --------------------------------
!
IS1 = SIZE(PZ1,2)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JK2,JI,ZTHR)
DO JK2 = 1,SIZE(PZ2,2)
  !
  DO JI = 1,SIZE(PZ1,1)
    !
    ZTHR = PZ2(JI,JK2) * (1.-ZEPS)
    ILEVEL(JI,JK2) = COUNT(PZ1(JI,:)<=ZTHR)
    !
  ENDDO
ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JK2,JI)
DO JK2 = 1,SIZE(PZ2,2)
  !
  DO JI = 1,SIZE(PZ1,1)

   IF (ILEVEL(JI,JK2) < 1 ) THEN
     KKLIN(JI,JK2) = 1
     PCOEFLIN(JI,JK2) = 1.                       ! no extrapolation
     CYCLE
   ENDIF

!* linear extrapolation above the uppest level
!* linear extrapolation under the ground
    ILEVEL(JI,JK2) = MIN(ILEVEL(JI,JK2),IS1-1)
    KKLIN (JI,JK2) = ILEVEL(JI,JK2)
!
!*       4.    Linear interpolation coefficients
!              ---------------------------------
!
    PCOEFLIN(JI,JK2) = (PZ2(JI,JK2)-PZ1(JI,ILEVEL(JI,JK2)+1))*ZDIZ1(JI,ILEVEL(JI,JK2))
    IF (ILEVEL(JI,JK2)==IS1-1) PCOEFLIN(JI,JK2) = MAX(PCOEFLIN(JI,JK2),0.) ! no extrapolation
  !
  ENDDO
!-------------------------------------------------------------------------------
END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF_3',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE COEF_VER_INTERP_LIN_SURF
