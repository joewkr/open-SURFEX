!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_PREP_FLAKE
!     ###########################
!
!!****  *DEFAULT_PREP_FLAKE* - routine to set default values for the configuration for FLAKE fields preparation
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
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
!!      S. Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_FLAKE,   ONLY : CFILE_FLAKE, CTYPE, CFILEPGD_FLAKE, CTYPEPGD, XTS_UNIF, &
                                XUNIF_T_SNOW, XUNIF_T_ICE, XUNIF_T_MNW, XUNIF_T_WML,  &
                                XUNIF_T_BOT, XUNIF_T_B1, XUNIF_CT, XUNIF_H_SNOW,      &
                                XUNIF_H_ICE, XUNIF_H_ML, XUNIF_H_B1 !, &  
!                              XUNIF_T_SNOW_DEF, &
!                              XUNIF_T_ICE_DEF,  &
!                              XUNIF_T_MNW_DEF,  &
!                              XUNIF_T_WML_DEF,  &
!                              XUNIF_T_BOT_DEF,  &
!                              XUNIF_T_B1_DEF,   &
!                              XUNIF_CT_DEF,     &
!                              XUNIF_H_SNOW_DEF, &
!                              XUNIF_H_ICE_DEF,  &
!                              XUNIF_H_ML_DEF,   &
!                              XUNIF_H_B1_DEF 
!
USE MODN_PREP_FLAKE, ONLY : LWAT_SBL, LCLIM_LAKE
!
USE MODD_SURF_PAR,   ONLY : XUNDEF 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_FLAKE',0,ZHOOK_HANDLE)
CFILE_FLAKE  = '                          '
CTYPE        = '      '
!
CFILEPGD_FLAKE = '                          '
CTYPEPGD       = '      '
!
XTS_UNIF = XUNDEF
XUNIF_T_SNOW   = XUNDEF
XUNIF_T_ICE    = XUNDEF
XUNIF_T_MNW    = XUNDEF
XUNIF_T_WML    = XUNDEF
XUNIF_T_BOT    = XUNDEF
XUNIF_T_B1     = XUNDEF
XUNIF_CT       = XUNDEF
XUNIF_H_SNOW   = XUNDEF
XUNIF_H_ICE    = XUNDEF
XUNIF_H_ML     = XUNDEF
XUNIF_H_B1     = XUNDEF

LCLIM_LAKE = .FALSE.
LWAT_SBL = .FALSE. 
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_FLAKE',1,ZHOOK_HANDLE)
!XUNIF_T_SNOW_DEF =
!XUNIF_T_ICE_DEF =
!XUNIF_T_MNW_DEF =
!XUNIF_T_WML_DEF =
!XUNIF_T_BOT_DEF =
!XUNIF_T_B1_DEF =
!XUNIF_CT_DEF =
!XUNIF_H_SNOW_DEF =
!XUNIF_H_ICE_DEF =
!XUNIF_H_ML_DEF =
!XUNIF_H_B1_DEF =
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_FLAKE
