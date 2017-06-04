!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_PREP_TEB_GREENROOF
!     ###########################
!
!!****  *DEFAULT_PREP_TEB_GREENROOF* - routine to set default values for the configuration for ISBA fields preparation
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    Based on "default_prep_teb_greenroof"
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
!!    A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_TEB_GREENROOF, ONLY : CFILE_GR, CTYPE, CFILEPGD_GR, CTYPEPGD,       &
                                    CFILE_HUG_GR, CTYPE_HUG,          &
                                    CFILE_HUG_SURF_GR, CFILE_HUG_ROOT_GR, CFILE_HUG_DEEP_GR,   &
                                    XHUG_SURF_GR, XHUG_ROOT_GR, XHUG_DEEP_GR,                  &
                                    XHUGI_SURF_GR, XHUGI_ROOT_GR, XHUGI_DEEP_GR,                &
                                    CFILE_TG_GR, CTYPE_TG,                               &
                                    CFILE_TG_SURF_GR, CFILE_TG_ROOT_GR, CFILE_TG_DEEP_GR,      &
                                    XTG_SURF_GR, XTG_ROOT_GR, XTG_DEEP_GR,                     &
                                    XWR_DEF  

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

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GREENROOF',0,ZHOOK_HANDLE)
CFILE_GR      = '                          '
CTYPE           = 'GRIB  '
CFILEPGD_GR   = '                          '
CTYPEPGD        = 'GRIB  '
CFILE_HUG_GR       = '                          '
CTYPE_HUG       = '      '
CFILE_TG_GR        = '                          '
CTYPE_TG        = '      '
!
CFILE_HUG_SURF_GR = '                          '
CFILE_HUG_ROOT_GR = '                          '
CFILE_HUG_DEEP_GR = '                          '
CFILE_TG_SURF_GR  = '                          '
CFILE_TG_ROOT_GR  = '                          '
CFILE_TG_DEEP_GR  = '                          '
!
XHUG_SURF_GR = XUNDEF
XHUG_ROOT_GR = XUNDEF
XHUG_DEEP_GR = XUNDEF
XHUGI_SURF_GR= 0.
XHUGI_ROOT_GR= 0.
XHUGI_DEEP_GR= 0.
XTG_SURF_GR  = XUNDEF
XTG_ROOT_GR  = XUNDEF
XTG_DEEP_GR  = XUNDEF
!
XWR_DEF   = 0.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GREENROOF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_TEB_GREENROOF
