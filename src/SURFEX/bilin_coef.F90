!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE BILIN_COEF (KLUOUT,PX1,PY1,PX2,PY2,PCX,PCY,KCI,KCJ)
!     #########################################################################
!
!!****  *BILIN_COEFEAR * - subroutine to interpolate surface FIELD
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!     Interpolation is bilinear, and uses 9 grid points, located in the
!!   center of model 1 grid mesh, and at the boundaries of this grid
!!   mesh (2 X limits, 2 Y limits and 4 corners).
!!     This implies that the grid mesh values located around the model 1
!!   grid mesh are not used directly. The values at the boundaries of the
!!   grid mesh are defined by the average between the middle point
!!   (this grid mesh value), and the one in the considered direction.
!!   So the eight grid meshes around the considered grid mesh are used
!!   equally.
!!     This is important to note that these average values are erased
!!   and replaced by zero if they are at the limit of any grid
!!   mesh with the zero value. This allows to insure zero value in model 2
!!   grid meshes where there was not the considered class in corresponding
!!   model 1 grid mesh, and to insure continuity of the FIELD type
!!   at such boundaries.
!!
!!
!!    The arrays and array index are defined on the following (model1) grid:
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j+1                    i,j+1                    i+1,j+1
!!
!!
!!
!!                   ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                     i,j+1        i,j+1       i+1,j+1
!!
!!
!!
!!        XFIELD      ZFIELD_X      XFIELD      ZFIELD_X      XFIELD
!!          *            *            *            *            *
!!        i-1,j         i,j          i,j         i+1,j        i+1,j
!!
!!
!!
!!                    ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                      i,j          i,j         i+1,j
!!
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j-1                    i,j-1                    i+1,j-1
!!
!!
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     01/2004
! TD&DD: added OpenMP directives

!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,              INTENT(IN)  :: KLUOUT  ! output listing logical unit
REAL, DIMENSION(:),   INTENT(IN)  :: PX1     ! X coordinate of the regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY1     ! Y coordinate of the regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PX2     ! X coordinate of all points of output grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY2     ! Y coordinate of all points of output grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PCX, PCY
INTEGER, DIMENSION(:), INTENT(OUT):: KCI, KCJ

!
!*       0.2    Declarations of local variables for print on FM file
!
! 
REAL, DIMENSION (SIZE(PX1)+1)        :: ZX       ! X coordinate of left   limit of input meshes
REAL, DIMENSION (SIZE(PY1)+1)        :: ZY       ! Y coordinate of bottom limit of input meshes
!
!
INTEGER                              :: IIU       ! model 1 X size
INTEGER                              :: IJU       ! model 1 Y size
!
INTEGER                              :: IDMIN, IDMAX
INTEGER                              :: JI        ! grid 1 x index
INTEGER                              :: JJ        ! grid 1 y index
INTEGER                              :: IIN       ! loop counter on all input points
!
INTEGER                              :: JL        ! grid 2 index
!
REAL                                 :: ZEPS=1.E-3
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_1',0,ZHOOK_HANDLE)

IIU=SIZE(PX1)
IJU=SIZE(PY1)
!
!*       4.     Coordinates of points between input grid points
!               -----------------------------------------------
!
IF (IIU>1) THEN
  ZX(1)     = 1.5*PX1(1)  -0.5*PX1(2)
  ZX(IIU+1) = 1.5*PX1(IIU)-0.5*PX1(IIU-1)
  DO JJ = 2,IIU
    ZX(JJ) = 0.5*(PX1(JJ-1)+PX1(JJ))
  ENDDO  
ELSE
  ZX(1)     = PX1(1) - 1.E6 ! uniform field in X direction if only 1 point is
  ZX(2)     = PX1(1) + 1.E6 ! available. Arbitrary mesh length of 2000km assumed
END IF
!
IF (IJU>1) THEN
  ZY(1)     = 1.5*PY1(1)  -0.5*PY1(2)
  ZY(IJU+1) = 1.5*PY1(IJU)-0.5*PY1(IJU-1)
  DO JJ = 2,IJU
    ZY(JJ) = 0.5*(PY1(JJ-1)+PY1(JJ))
  ENDDO
ELSE
  ZY(1)     = PY1(1) - 1.E6 ! uniform field in Y direction if only 1 point is
  ZY(2)     = PY1(1) + 1.E6 ! available. Arbitrary mesh length of 2000km assumed
END IF
!
!-------------------------------------------------------------------------------
!
!*       5.     Interpolation
!               -------------
!
!* loop on all output grid points
!
KCI(:) = 1
KCJ(:) = 1
!
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JL,JJ)
DO JL=1,SIZE(PX2)
  DO JJ=SIZE(ZX),1,-1
    IF (ZX(JJ)<=PX2(JL)) THEN
      KCI(JL) = JJ
      EXIT
    ENDIF
  ENDDO
  DO JJ=SIZE(ZY),1,-1
    IF (ZY(JJ)<=PY2(JL)) THEN
      KCJ(JL) = JJ
      EXIT
    ENDIF
  ENDDO
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_3',0,ZHOOK_HANDLE)

DO JL=1,SIZE(PX2)
!
!* interpolation weights in X direction
!
  JI=KCI(JL)
  JI=MAX(MIN(JI,IIU),0)
  IF (PX1(JI)<=PX2(JL)) THEN
    PCX(JL,2) = (PX2(JL)-ZX(JI+1))/(PX1(JI)-ZX(JI+1))
    PCX(JL,2) = MAX(MIN(PCX(JL,2),1.),0.)
    PCX(JL,3) = 1. - PCX(JL,2)
    PCX(JL,1) = 0.
  ELSE
    PCX(JL,2) = (PX2(JL)-ZX(JI))/(PX1(JI)-ZX(JI))
    PCX(JL,2) = MAX(MIN(PCX(JL,2),1.),0.)
    PCX(JL,1) = 1. - PCX(JL,2)
    PCX(JL,3) = 0.
  END IF
!
!  interpolation weights in Y direction
!
  JJ=KCJ(JL)
  JJ=MAX(MIN(JJ,IJU),0)
  IF (PY1(JJ)<=PY2(JL)) THEN
    PCY(JL,2) = (PY2(JL)-ZY(JJ+1))/(PY1(JJ)-ZY(JJ+1))
    PCY(JL,2) = MAX(MIN(PCY(JL,2),1.),0.)
    PCY(JL,3) = 1. - PCY(JL,2)
    PCY(JL,1) = 0.
  ELSE
    PCY(JL,2) = (PY2(JL)-ZY(JJ))/(PY1(JJ)-ZY(JJ))
    PCY(JL,2) = MAX(MIN(PCY(JL,2),1.),0.)
    PCY(JL,1) = 1. - PCY(JL,2)
    PCY(JL,3) = 0.
  END IF
!
END DO
!
IF (LHOOK) CALL DR_HOOK('BILIN_COEF_3',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE BILIN_COEF
