!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_FLAKE_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_FLAKE_UNIF* - prepares FLAKE field from prescribed values
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
!!    09/2010 E. Kourzeneva: Renamed the lake surface temperature 
!!                           from the namelist
!!------------------------------------------------------------------
!
!
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_PREP_FLAKE,   ONLY : XTS_UNIF, &
                                  XUNIF_T_SNOW  , &
                                  XUNIF_T_ICE   , &
                                  XUNIF_T_MNW   , &
                                  XUNIF_T_WML   , &
                                  XUNIF_T_BOT   , &
                                  XUNIF_T_B1    , &
                                  XUNIF_CT      , &
                                  XUNIF_H_SNOW  , &
                                  XUNIF_H_ICE   , &
                                  XUNIF_H_ML    , &
                                  XUNIF_H_B1         
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
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = 0.
!
!*      3.1    FLake variables
!
  CASE('TS     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XTS_UNIF
!
  CASE('T_SNOW ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_SNOW
!
  CASE('T_ICE  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_ICE
!
  CASE('T_MNW  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_MNW
!
  CASE('T_WML  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_WML
!
  CASE('T_BOT  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_BOT
!
  CASE('T_B1   ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_T_B1
!
  CASE('CT     ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_CT
!
  CASE('H_SNOW ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_H_SNOW
!
  CASE('H_ICE  ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_H_ICE
!
  CASE('H_ML   ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_H_ML
!
  CASE('H_B1   ')
    ALLOCATE(PFIELD(1,1))
    PFIELD = XUNIF_H_B1
!
!
END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_UNIF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_FLAKE_UNIF
