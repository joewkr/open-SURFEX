!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_TEB
!     ################
!
!!****  *MODD_PREP - declaration for field interpolations
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
SAVE
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_TEB      ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_TEB   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD       ! input file type
 CHARACTER(LEN=28) :: CFILE_SNOW_TEB ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPE_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILEPGD_SNOW_TEB ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPEPGD_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILE_WS       ! input file name for Ws
 CHARACTER(LEN=6)  :: CTYPE_WS       ! input file type for Ws
 CHARACTER(LEN=28) :: CFILE_TS       ! input file name for Ts
 CHARACTER(LEN=6)  :: CTYPE_TS       ! input file type for Ts
!
REAL              :: XWS_ROOF       ! roof uniform water content (kg/m2)
REAL              :: XWS_ROAD       ! road uniform water content (kg/m2)
REAL              :: XTS_ROOF       ! roof uniform temperature   (K)
REAL              :: XTS_ROAD       ! road uniform temperature   (K)
REAL              :: XTS_WALL       ! wall uniform temperature   (K)
REAL              :: XTI_BLD        ! uniform building interior T(K)
REAL              :: XTI_ROAD       ! uniform deep road Temp.    (K)
REAL              :: XHUI_BLD       ! uniform building relative hum (between 0-1)
!
REAL              :: XT_CAN         ! uniform canyon air Temp.   (K)
REAL              :: XQ_CAN         ! uniform canyon air Humidity(kg/kg)

!
REAL              :: XWS_ROOF_DEF   ! default roof uniform water content (kg/m2)
REAL              :: XWS_ROAD_DEF   ! default road uniform water content (kg/m2)
REAL              :: XTI_BLD_DEF    ! default uniform building interior T(K)
REAL              :: XHUI_BLD_DEF   ! default uniform building interior relative humidity (between 0-1)
!
! Snow variables
!
LOGICAL :: LSNOW_IDEAL_TEB 
!
REAL, DIMENSION(:), POINTER :: XWSNOW_ROOF      ! snow reservoir   for roofs
REAL, DIMENSION(:), POINTER :: XTSNOW_ROOF      ! snow density     for roofs
REAL, DIMENSION(:), POINTER :: XLWCSNOW_ROOF      ! snow liquid water content     for roofs
REAL, DIMENSION(:), POINTER :: XRSNOW_ROOF      ! snow temperature for roofs
REAL              :: XASNOW_ROOF      ! snow albedo      for roofs
!
REAL, DIMENSION(:), POINTER :: XWSNOW_ROAD      ! snow reservoir   for roads
REAL, DIMENSION(:), POINTER :: XTSNOW_ROAD      ! snow temperature for roads
REAL, DIMENSION(:), POINTER :: XLWCSNOW_ROAD      ! snow liquid water content     for roads
REAL, DIMENSION(:), POINTER :: XRSNOW_ROAD      ! snow density     for roads
REAL              :: XASNOW_ROAD      ! snow albedo      for roads
!
!--------------------------------------------------------------------------
!
!* normalized dimensions for interpolation grids for roof, wall, and roads
REAL, DIMENSION(10) :: XGRID_ROOF  = (/ 0., 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.7, 0.9, 1. /)
REAL, DIMENSION(10) :: XGRID_WALL  = (/ 0., 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.7, 0.9, 1. /)
REAL, DIMENSION(10) :: XGRID_ROAD  = (/ 0., 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.7, 0.9, 1. /)
REAL, DIMENSION(10) :: XGRID_FLOOR = (/ 0., 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.7, 0.9, 1. /)
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_TEB
