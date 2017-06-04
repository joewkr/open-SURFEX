!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE SM10(PZ,PBLD_HEIGHT,PLAMBDA_F,PL)
!     ###############################################################################
!
!!****  *SM10* computes the shape for the mixing length according to Santiago and Martilli 2010
!!
!!    PURPOSE
!!    -------
!
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
!!      Original    06/2010
!!---------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ          ! canopy levels        (m)
REAL, DIMENSION(:),   INTENT(IN)  :: PBLD_HEIGHT ! building height      (m)
REAL, DIMENSION(:),   INTENT(IN)  :: PLAMBDA_F   ! frontal area density (-)
REAL, DIMENSION(:,:), INTENT(OUT) :: PL          ! base profile for mixing 
!                                                ! length computations  (m)
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PZ,1))           :: ZZ_CAN             ! mixing length generic profile in canopy
REAL, DIMENSION(SIZE(PZ,1),SIZE(PZ,2)):: ZZ_SURF            ! Mixing length generic profile at mid levels (near the surface)
REAL, DIMENSION(SIZE(PZ,1),SIZE(PZ,2)):: ZZ_ISBL            ! Mixing length generic profile at mid levels (in the inertial SBL)
REAL, DIMENSION(SIZE(PZ,1))           :: ZZ_BASE_ISBL       ! Mixing length generic at the base of the ISBL
REAL, DIMENSION(SIZE(PZ,1))           :: ZDISP_H            ! displacement height
REAL, PARAMETER                       :: ZALPHA_CAN = 1.12  ! value to compute lengths in the canyon

INTEGER                               :: JLAYER             ! vertical loop counter
INTEGER                               :: ILVL               ! number of layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!* Preliminaries:
IF (LHOOK) CALL DR_HOOK('SM10',0,ZHOOK_HANDLE)
ILVL = SIZE(PZ,2)
!
!* Typical uniform value in the canopy (after Santiago and Martilli 2010)
!  Threshold at 3/4 of hte height of the building is added to avoid unphysical
!  values for large lambda_f.
!
ZDISP_H(:) = MIN ( PLAMBDA_F(:)**0.13 * PBLD_HEIGHT(:) , 0.75 * PBLD_HEIGHT )
ZZ_CAN(:)  = ZALPHA_CAN * (PBLD_HEIGHT(:) - ZDISP_H(:))
!
!* Lengths near the surface (road, gardens)
ZZ_SURF(:,:) = PZ(:,:)
!
!* Lengths in the inertial sublayer (z/h>1.5)
DO JLAYER=1,ILVL
  ZZ_ISBL(:,JLAYER) = MAX(ZZ_CAN(:), PZ(:,JLAYER) - ZDISP_H(:))
END DO
! first point is used to compute the value at the base of the ISBL (z/h=1.5)
ZZ_BASE_ISBL(:) = MAX (PZ(:,1),  1.5 * PBLD_HEIGHT(:) )
!
!* Composition of all these mixing lengths
DO JLAYER=1,ILVL
  WHERE (PZ(:,JLAYER)<=PBLD_HEIGHT(:))
!* inside canopy, lengths are equal to minimum between uniform value and value limited by the surface
    PL(:,JLAYER) = MIN(ZZ_SURF(:,JLAYER), ZZ_CAN(:))
  END WHERE
  WHERE (PZ(:,JLAYER)>ZZ_BASE_ISBL(:) )
!* in the inertial sublayer
    PL(:,JLAYER) = ZZ_ISBL(:,JLAYER)
  END WHERE
  WHERE (PZ(:,JLAYER)>PBLD_HEIGHT(:) .AND. PZ(:,JLAYER)<=1.5*PBLD_HEIGHT(:))
!* in the transition sublayer
    PL(:,JLAYER) = ZZ_CAN(:) + (ZZ_ISBL(:,JLAYER)-ZZ_CAN(:)) &
                               * (PZ(:,JLAYER)-PBLD_HEIGHT(:)) / (ZZ_BASE_ISBL(:) - PBLD_HEIGHT(:))
  END WHERE
END DO
!
! check if mixing length scale increases with height
!
DO JLAYER=2,ILVL
  PL(:,JLAYER) = MAX(PL(:,JLAYER-1),PL(:,JLAYER))
END DO
IF (LHOOK) CALL DR_HOOK('SM10',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SM10
