!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE BILIN_EXTRAP (KLUOUT,KX,KY,KCIJ,PX1,PY1,PFIELD1,PX2,PY2,PFIELD2,OINTERP)
!     #########################################################################
!
!!****  *BILIN_EXTRAPEAR * - subroutine to interpolate surface FIELD
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
USE MODD_SURFEX_MPI, ONLY : NCOMM,NPIO,NRANK,NPROC
USE MODI_HOR_EXTRAPOL_SURF
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,              INTENT(IN)  :: KLUOUT  ! output listing logical unit
INTEGER, INTENT(IN) :: KX, KY
INTEGER, DIMENSION(:,:), INTENT(IN) :: KCIJ
REAL, DIMENSION(:),   INTENT(IN)  :: PX1     ! X coordinate of the regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY1     ! Y coordinate of the regular input grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD1 ! FIELD on regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PX2     ! X coordinate of all points of output grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY2     ! Y coordinate of all points of output grid
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PFIELD2 ! FIELD on model 2
LOGICAL, DIMENSION(:),INTENT(IN)  :: OINTERP ! .true. where physical value is needed
!
!
!*       0.2    Declarations of local variables for print on FM file
!
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IP
INTEGER, DIMENSION(KY) :: IX
! Variables implied in the extension procedure
INTEGER :: ICOUNT
 ! Loop counters
INTEGER :: INFOMPI, JL, JI, INL, INO
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BILIN_EXTRAP',0,ZHOOK_HANDLE)
!
INO = SIZE(PFIELD2,1)
INL = SIZE(PFIELD2,2)
!
!*     6.   EXTRAPOLATIONS IF SOME POINTS WERE NOT INTERPOLATED
!           ---------------------------------------------------
!
!* no data point
IF (NRANK==NPIO) ICOUNT = COUNT(PFIELD1(:,:)/=XUNDEF)
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(ICOUNT,KIND(ICOUNT)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
IF (ICOUNT==0 .AND. LHOOK) CALL DR_HOOK('BILIN_EXTRAP',1,ZHOOK_HANDLE)
IF (ICOUNT==0) RETURN
!
DO JL=1,INL
  WRITE(KLUOUT,*) ' Remaining horizontal extrapolations'
  WRITE(KLUOUT,*) ' Total number of input data     : ',ICOUNT,' /  ',SIZE(PFIELD2(:,JL))
  WRITE(KLUOUT,*) ' Number of points to interpolate: ',COUNT(PFIELD2(:,JL)==XUNDEF .AND. OINTERP(:))
ENDDO
!
ALLOCATE(IP(INO,1))
DO JI=1,INO
  IP(JI,1) = KX*(KCIJ(JI,2)-1)+KCIJ(JI,1)
ENDDO
!* input grid coordinates
!
IX(:) = KX
 CALL HOR_EXTRAPOL_SURF(KLUOUT,'XY  ',KX*KY,PY1(1),PY1(KY),PX1(1),PX1(KX),KY,IX,&
                        IP,PFIELD1,PY2,PX2,PFIELD2,OINTERP)
!
DEALLOCATE(IP)
!
IF (LHOOK) CALL DR_HOOK('BILIN_EXTRAP',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE BILIN_EXTRAP
