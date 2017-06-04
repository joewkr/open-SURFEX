!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_OCEAN_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_OCEAN_UNIF* - prepares oceanic field from prescribed values
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
!!     C. Lebeaupin Brossier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!------------------------------------------------------------------
!

!
USE MODD_CSTS,       ONLY : XTT
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_PREP_SEAFLUX,   ONLY : XSST_UNIF
USE MODD_OCEAN_CSTS
USE MODD_OCEAN_GRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
REAL, POINTER, DIMENSION(:,:,:)   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
REAL :: ZSSS_UNIF=37.6
REAL :: ZGRADT,ZGRADS
INTEGER :: JLEV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.1    oceanic temperature
!
  CASE('TEMP_OC')
    ZGRADT=1.2/1000.
    ALLOCATE(PFIELD(1,NOCKMAX-NOCKMIN+1,1))
    PFIELD(:,1,:) = XSST_UNIF-XTT
    DO JLEV=1,NOCKMAX
      PFIELD(:,JLEV+1,:) = XSST_UNIF-XTT-ZGRADT*(XZHOC(JLEV)+1)
    ENDDO
!
!*      3.2    oceanic salinity
!
  CASE('SALT_OC')
    ZGRADS=-0.5/1000.
    ALLOCATE(PFIELD(1,NOCKMAX-NOCKMIN+1,1))
    PFIELD(:,1,:) = ZSSS_UNIF
    DO JLEV=1,NOCKMAX
      PFIELD(:,JLEV+1,:) = ZSSS_UNIF+ZGRADS*(XZHOC(JLEV)+1)
    ENDDO
!
!*      3.3    oceanic currents
!
  CASE('UCUR_OC')
    ALLOCATE(PFIELD(1,NOCKMAX-NOCKMIN+1,1))
    PFIELD = 0. 
!
  CASE('VCUR_OC')
    ALLOCATE(PFIELD(1,NOCKMAX-NOCKMIN+1,1))
    PFIELD = 0. 
!
END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_UNIF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_OCEAN_UNIF
