!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_PREP_ISBA
!     ###########################
!
!!****  *DEFAULT_PREP_ISBA* - routine to set default values for the configuration for ISBA fields preparation
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
!!      MEB         10/2014   P. Samuelsson
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_ISBA,  ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,       &
                              CFILE_HUG, CTYPE_HUG,                           &
                              CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                              XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                 &
                              XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,             &
                              CFILE_TG, CTYPE_TG,                             &
                              CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                              XTG_SURF, XTG_ROOT, XTG_DEEP,                   &
                              XWR_DEF, LEXTRAP_TG, LEXTRAP_WG, LEXTRAP_WGI,   &
                              LEXTRAP_SN,                                     &
                              XWRV_DEF, XWRVN_DEF, XQC_DEF
!
USE MODN_PREP_ISBA,  ONLY : LISBA_CANOPY
!
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

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_ISBA',0,ZHOOK_HANDLE)
CFILE_ISBA = '                          '
CTYPE      = 'GRIB  '
CFILEPGD_ISBA = '                          '
CTYPEPGD      = '      '
CFILE_HUG  = '                          '
CTYPE_HUG  = '      '
CFILE_TG   = '                          '
CTYPE_TG   = '      '
!
CFILE_HUG_SURF = '                          '
CFILE_HUG_ROOT = '                          '
CFILE_HUG_DEEP = '                          '
CFILE_TG_SURF  = '                          '
CFILE_TG_ROOT  = '                          '
CFILE_TG_DEEP  = '                          '
!
XHUG_SURF = XUNDEF
XHUG_ROOT = XUNDEF
XHUG_DEEP = XUNDEF
XHUGI_SURF= XUNDEF
XHUGI_ROOT= XUNDEF
XHUGI_DEEP= XUNDEF
XTG_SURF  = XUNDEF
XTG_ROOT  = XUNDEF
XTG_DEEP  = XUNDEF
!
XWR_DEF   = 0.
XWRV_DEF  = 0.
XWRVN_DEF = 0.
XQC_DEF   = 0.
!
LISBA_CANOPY = .FALSE.
LEXTRAP_TG   = .FALSE.
LEXTRAP_WG   = .FALSE.
LEXTRAP_WGI  = .FALSE.
LEXTRAP_SN   = .FALSE.
 
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_ISBA',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_ISBA
