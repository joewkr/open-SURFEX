!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_DST
!     ########################################################################
!
!!****  *DEFAULT_DST* - routine to set default values for the configuration for DST
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
USE MODD_DST_SURF,   ONLY : CEMISPARAM_DST, JPMODE_DST, LVARSIG_DST, LRGFIX_DST, &
                            CVERMOD, XFLX_MSS_FDG_FCT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! Set initial values of variables. These are modified by namelist


REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEFAULT_DST',0,ZHOOK_HANDLE)
CEMISPARAM_DST = "AMMA "
JPMODE_DST     = 3
LVARSIG_DST    = .FALSE.
LRGFIX_DST     = .TRUE.
CVERMOD = 'NONE  '
XFLX_MSS_FDG_FCT = 12.0e-4 
IF (LHOOK) CALL DR_HOOK('DEFAULT_DST',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_DST
