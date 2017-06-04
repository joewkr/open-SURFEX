!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_SLT
!     ########################################################################
!
!!****  *DEFAULT_SLT* - routine to set default values for the configuration for SLT
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
!!      Alf Grini CNRM
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SLT_SURF,   ONLY : CEMISPARAM_SLT, JPMODE_SLT, LVARSIG_SLT, LRGFIX_SLT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! Set initial values of variables. These are modified by namelist
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SLT',0,ZHOOK_HANDLE)
CEMISPARAM_SLT = 'Vig01'
JPMODE_SLT     = 3
LVARSIG_SLT    = .FALSE.
LRGFIX_SLT     = .TRUE.
IF (LHOOK) CALL DR_HOOK('DEFAULT_SLT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SLT
