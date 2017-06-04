!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_SEAFLUX_UNIF* - prepares SEAFLUX field from prescribed values
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    09/2013 : S. Senesi : extends to SSS and SIC variables
!!------------------------------------------------------------------
!

!
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_PREP_SEAFLUX,   ONLY : XSST_UNIF, XSSS_UNIF, XSIC_UNIF
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
REAL, POINTER, DIMENSION(:,:)   :: PFIELD    ! field to interpolate horizontally
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = 0.
!
!*      3.1    Sea surface temperature
!
  CASE('SST    ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XSST_UNIF
!
!*      3.2    Sea surface salinity
!
  CASE('SSS    ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XSSS_UNIF
!
!
!*      3.3    Sea Ice Cover
!
  CASE('SIC    ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XSIC_UNIF
!
END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_UNIF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SEAFLUX_UNIF
