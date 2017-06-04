!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_TEB_GARDEN
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
 CHARACTER(LEN=28) :: CFILE_GD     ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_GD  ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD       ! input file type
 CHARACTER(LEN=28) :: CFILE_SNOW_GD     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPE_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILEPGD_SNOW_GD     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPEPGD_SNOW     ! input file type for Snow 
 CHARACTER(LEN=28) :: CFILE_HUG_GD      ! input file name for Wg, Wgi
 CHARACTER(LEN=6)  :: CTYPE_HUG      ! input file type for Wg, Wgi
 CHARACTER(LEN=28) :: CFILE_TG_GD       ! input file name for Tg
 CHARACTER(LEN=6)  :: CTYPE_TG       ! input file type for Tg
 CHARACTER(LEN=28) :: CFILE_HUG_SURF_GD ! input file name for HUG_SURF
 CHARACTER(LEN=28) :: CFILE_HUG_ROOT_GD ! input file name for HUG_ROOT
 CHARACTER(LEN=28) :: CFILE_HUG_DEEP_GD ! input file name for HUG_DEEP
 CHARACTER(LEN=28) :: CFILE_TG_SURF_GD  ! input file name for TG_SURF
 CHARACTER(LEN=28) :: CFILE_TG_ROOT_GD  ! input file name for TG_ROOT
 CHARACTER(LEN=28) :: CFILE_TG_DEEP_GD  ! input file name for TG_DEEP
!
REAL              :: XHUG_SURF_GD      ! surface relative soil humidity
REAL              :: XHUG_ROOT_GD      ! root layer relative soil humidity
REAL              :: XHUG_DEEP_GD      ! deep layer relative soil humidity
REAL              :: XHUGI_SURF_GD     ! surf layer relative ice content
REAL              :: XHUGI_ROOT_GD     ! root layer relative ice content
REAL              :: XHUGI_DEEP_GD     ! deep layer relative ice content
REAL              :: XTG_SURF_GD       ! surface temperature
REAL              :: XTG_ROOT_GD       ! root layer temperature
REAL              :: XTG_DEEP_GD       ! deep layer temperature
!
LOGICAL :: LSNOW_IDEAL_GD 
!
REAL, DIMENSION(:), POINTER :: XWSNOW_GD         ! Snow reservoir
REAL, DIMENSION(:), POINTER :: XRSNOW_GD         ! snow density
REAL, DIMENSION(:), POINTER :: XTSNOW_GD         ! snow temperature
REAL, DIMENSION(:), POINTER :: XAGESNOW_GD         ! snow age
REAL, DIMENSION(:), POINTER :: XLWCSNOW_GD         ! snow liquid water content
REAL              :: XASNOW_GD         ! snow albedo
!
REAL              :: XWR_DEF        ! default for leaves interception reservoir
!--------------------------------------------------------------------------
!
!* dimensions for interpolation grids for soil
INTEGER, PARAMETER           :: NGRID_LEVEL = 22
REAL, DIMENSION(NGRID_LEVEL) :: XGRID_SOIL = &
      (/0.01,0.04,0.10,0.20,0.40,0.60,0.80,1.0,1.25,1.5,1.75,2.0,2.5,3.0,4.0,5.0,8.00,12.0,17.,23.,30.,100./)
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_TEB_GARDEN


