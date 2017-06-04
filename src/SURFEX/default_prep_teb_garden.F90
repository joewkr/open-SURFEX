!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_PREP_TEB_GARDEN
!     ###########################
!
!!****  *DEFAULT_PREP_TEB_GARDEN* - routine to set default values for the configuration for ISBA fields preparation
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      Modified    03/2007   P. Le Moigne 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_TEB_GARDEN, ONLY : CFILE_GD, CTYPE, CFILEPGD_GD, CTYPEPGD,       &
                                   CFILE_HUG_GD, CTYPE_HUG,                           &
                                   CFILE_HUG_SURF_GD, CFILE_HUG_ROOT_GD, CFILE_HUG_DEEP_GD, &
                                   XHUG_SURF_GD, XHUG_ROOT_GD, XHUG_DEEP_GD,                &
                                   XHUGI_SURF_GD, XHUGI_ROOT_GD, XHUGI_DEEP_GD,              &
                                   CFILE_TG_GD, CTYPE_TG,                             &
                                   CFILE_TG_SURF_GD, CFILE_TG_ROOT_GD, CFILE_TG_DEEP_GD,    &
                                   XTG_SURF_GD, XTG_ROOT_GD, XTG_DEEP_GD,                   &
                                   XWR_DEF  

USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XANSMIN, XRHOSMAX
USE MODD_CSTS,       ONLY : XTT
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

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GARDEN',0,ZHOOK_HANDLE)
CFILE_GD = '                          '
CTYPE      = 'GRIB  '
CFILEPGD_GD = '                          '
CTYPEPGD      = '      '
CFILE_HUG_GD  = '                          '
CTYPE_HUG  = '      '
CFILE_TG_GD   = '                          '
CTYPE_TG   = '      '
!
CFILE_HUG_SURF_GD = '                          '
CFILE_HUG_ROOT_GD = '                          '
CFILE_HUG_DEEP_GD = '                          '
CFILE_TG_SURF_GD  = '                          '
CFILE_TG_ROOT_GD  = '                          '
CFILE_TG_DEEP_GD  = '                          '
!
XHUG_SURF_GD = XUNDEF
XHUG_ROOT_GD = XUNDEF
XHUG_DEEP_GD = XUNDEF
XHUGI_SURF_GD= XUNDEF
XHUGI_ROOT_GD= XUNDEF
XHUGI_DEEP_GD= XUNDEF
XTG_SURF_GD  = XUNDEF
XTG_ROOT_GD  = XUNDEF
XTG_DEEP_GD  = XUNDEF
!
XWR_DEF   = 0.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GARDEN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_TEB_GARDEN
