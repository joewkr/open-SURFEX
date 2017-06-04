!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_ISBA
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
!!      P Samuelsson   02/2012  MEB
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
SAVE
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_ISBA     ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_ISBA  ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD       ! input file type
 CHARACTER(LEN=28) :: CFILE_SNOW     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPE_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILEPGD_SNOW  ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPEPGD_SNOW  ! input file type for Snow
 CHARACTER(LEN=28) :: CFILE_HUG      ! input file name for Wg, Wgi
 CHARACTER(LEN=6)  :: CTYPE_HUG      ! input file type for Wg, Wgi
 CHARACTER(LEN=28) :: CFILE_TG       ! input file name for Tg
 CHARACTER(LEN=6)  :: CTYPE_TG       ! input file type for Tg
 CHARACTER(LEN=28) :: CFILE_HUG_SURF ! input file name for HUG_SURF
 CHARACTER(LEN=28) :: CFILE_HUG_ROOT ! input file name for HUG_ROOT
 CHARACTER(LEN=28) :: CFILE_HUG_DEEP ! input file name for HUG_DEEP
 CHARACTER(LEN=28) :: CFILE_TG_SURF  ! input file name for TG_SURF
 CHARACTER(LEN=28) :: CFILE_TG_ROOT  ! input file name for TG_ROOT
 CHARACTER(LEN=28) :: CFILE_TG_DEEP  ! input file name for TG_DEEP
!
REAL              :: XHUG_SURF      ! surface relative soil humidity
REAL              :: XHUG_ROOT      ! root layer relative soil humidity
REAL              :: XHUG_DEEP      ! deep layer relative soil humidity
REAL              :: XHUGI_SURF     ! surf layer relative ice content
REAL              :: XHUGI_ROOT     ! root layer relative ice content
REAL              :: XHUGI_DEEP     ! deep layer relative ice content
REAL              :: XTG_SURF       ! surface temperature
REAL              :: XTG_ROOT       ! root layer temperature
REAL              :: XTG_DEEP       ! deep layer temperature
!
REAL              :: XWR_DEF        ! default for leaves interception reservoir
REAL              :: XWRV_DEF       ! default for canopy vegetation leaves interception reservoir
REAL              :: XWRVN_DEF      ! default for canopy vegetation leaves snow interception reservoir
REAL              :: XQC_DEF        ! default for canopy air specific humidity
!--------------------------------------------------------------------------
!
!* dimensions for interpolation grids for soil  
!
INTEGER, PARAMETER           :: NGRID_LEVEL = 22
REAL, DIMENSION(NGRID_LEVEL) :: XGRID_SOIL = &
      (/0.01,0.04,0.10,0.20,0.40,0.60,0.80,1.0,1.25,1.5,1.75,2.0,2.5,3.0,4.0,5.0,8.00,12.0,17.,23.,30.,100./)
!
!--------------------------------------------------------------------------
!
! Parameter for snow field uniform initialization
!
LOGICAL :: LSNOW_IDEAL 
!
REAL, DIMENSION(:), POINTER :: XWSNOW         ! Snow reservoir
REAL, DIMENSION(:), POINTER :: XRSNOW         ! snow density
REAL, DIMENSION(:), POINTER :: XTSNOW         ! snow temperature
REAL, DIMENSION(:), POINTER :: XLWCSNOW       ! snow liquid water content
REAL, DIMENSION(:), POINTER :: XSG1SNOW
REAL, DIMENSION(:), POINTER :: XSG2SNOW
REAL, DIMENSION(:), POINTER :: XHISTSNOW
REAL, DIMENSION(:), POINTER :: XAGESNOW
REAL                  :: XASNOW         ! snow albedo
!
!--------------------------------------------------------------------------
!
LOGICAL           :: LEXTRAP_TG     ! extrapolate TG points where LSM < 0.5 (buffer only)         
LOGICAL           :: LEXTRAP_WG     ! extrapolate WG points where LSM < 0.5 (buffer only)  
LOGICAL           :: LEXTRAP_WGI    ! extrapolate WGI points where LSM < 0.5 (buffer only) 
LOGICAL           :: LEXTRAP_SN     ! extrapolate SNOW (SWE/depth) points where LSM < 0.5 (buffer only) 

END MODULE MODD_PREP_ISBA


