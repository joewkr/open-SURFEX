!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_PREP_TEB
!     ###########################
!
!!****  *DEFAULT_PREP_TEB* - routine to set default values for the configuration for TEB fields preparation
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_TEB,   ONLY : CFILE_TEB, CTYPE, CFILEPGD_TEB, CTYPEPGD,                   &
                              CFILE_WS, CTYPE_WS, XWS_ROOF, XWS_ROAD,                   &
                              CFILE_TS, CTYPE_TS, XTS_ROOF, XTS_ROAD, XTS_WALL,         &
                              XTI_BLD, XTI_ROAD, XQ_CAN, XHUI_BLD_DEF, XHUI_BLD,        &
                              XWS_ROAD_DEF, XWS_ROOF_DEF, XTI_BLD_DEF, XT_CAN  

USE MODN_PREP_TEB,   ONLY : LTEB_CANOPY, CROAD_DIR, CWALL_OPT

USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT
USE MODD_SNOW_PAR,   ONLY : XANSMIN, XRHOSMAX
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

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB',0,ZHOOK_HANDLE)
CFILE_TEB= '                          '
CTYPE    = 'GRIB  '
CFILEPGD_TEB = '                          '
CTYPEPGD      = '      '
CFILE_WS = '                          '
CTYPE_WS = '      '
CFILE_TS = '                          '
CTYPE_TS = '      '
!
XWS_ROOF = XUNDEF
XWS_ROAD = XUNDEF
XTS_ROOF = XUNDEF
XTS_ROAD = XUNDEF
XTS_WALL = XUNDEF
XTI_ROAD = XUNDEF
XTI_BLD  = XUNDEF
XHUI_BLD = XUNDEF
XT_CAN   = XUNDEF
!
XWS_ROOF_DEF = 0.
XWS_ROAD_DEF = 0.
XTI_BLD_DEF  = 19. + XTT
XHUI_BLD_DEF = 0.5
!
XQ_CAN = 0.
!
LTEB_CANOPY = .FALSE.
CROAD_DIR   = 'UNIF'
CWALL_OPT   = 'UNIF'
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_TEB
