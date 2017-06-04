!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_TEB
!     ##################
!
!!****  *MODN_PREP_TEB* - declaration of namelist NAM_PREP_TEB
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_TEB
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_TEB, ONLY : CFILE_TEB, CTYPE, CFILEPGD_TEB, CTYPEPGD,          &
                            CFILE_WS, CTYPE_WS, CFILE_TS, CTYPE_TS,          &
                            XWS_ROOF, XWS_ROAD, XHUI_BLD,                    &
                            XTS_ROOF, XTS_ROAD, XTS_WALL, XTI_BLD, XTI_ROAD  

!
IMPLICIT NONE
!
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface
LOGICAL           :: LTEB_CANOPY  ! flag to use air layers inside the canopy
 CHARACTER(LEN=4)  :: CROAD_DIR    ! flag to use uniform road orientation or not
 CHARACTER(LEN=4)  :: CWALL_OPT    ! flag to use uniform walls or 2 separated walls
!
NAMELIST/NAM_PREP_TEB/CFILE_TEB, CTYPE, CFILEPGD_TEB, CTYPEPGD,  &
                      CFILE_WS, CTYPE_WS, XWS_ROOF, XWS_ROAD,    &
                      CFILE_TS, CTYPE_TS, XTS_ROOF, XTS_ROAD,    &
                      XTS_WALL, XTI_BLD, XTI_ROAD, XHUI_BLD,     &
                      NYEAR, NMONTH, NDAY, XTIME, LTEB_CANOPY,   &
                      CROAD_DIR, CWALL_OPT  
!
END MODULE MODN_PREP_TEB
