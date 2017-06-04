!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_TEB_GREENROOF
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
!!      A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2011
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_GR     ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_GR   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD       ! input file type
 CHARACTER(LEN=28) :: CFILE_SNOW_GR     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPE_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILEPGD_SNOW_GR     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPEPGD_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILE_HUG_GR      ! input file name for Wg, Wgi
 CHARACTER(LEN=6)  :: CTYPE_HUG      ! input file type for Wg, Wgi
 CHARACTER(LEN=28) :: CFILE_TG_GR       ! input file name for Tg
 CHARACTER(LEN=6)  :: CTYPE_TG       ! input file type for Tg
 CHARACTER(LEN=28) :: CFILE_HUG_SURF_GR ! input file name for HUG_SURF
 CHARACTER(LEN=28) :: CFILE_HUG_ROOT_GR ! input file name for HUG_ROOT
 CHARACTER(LEN=28) :: CFILE_HUG_DEEP_GR ! input file name for HUG_DEEP
 CHARACTER(LEN=28) :: CFILE_TG_SURF_GR  ! input file name for TG_SURF
 CHARACTER(LEN=28) :: CFILE_TG_ROOT_GR  ! input file name for TG_ROOT
 CHARACTER(LEN=28) :: CFILE_TG_DEEP_GR  ! input file name for TG_DEEP
!
REAL              :: XHUG_SURF_GR      ! surface relative soil humidity
REAL              :: XHUG_ROOT_GR      ! root layer relative soil humidity
REAL              :: XHUG_DEEP_GR      ! deep layer relative soil humidity
REAL              :: XHUGI_SURF_GR     ! surf layer relative ice content
REAL              :: XHUGI_ROOT_GR     ! root layer relative ice content
REAL              :: XHUGI_DEEP_GR     ! deep layer relative ice content
REAL              :: XTG_SURF_GR       ! surface temperature
REAL              :: XTG_ROOT_GR       ! root layer temperature
REAL              :: XTG_DEEP_GR       ! deep layer temperature
!
LOGICAL :: LSNOW_IDEAL_GR 
!
REAL, DIMENSION(:), POINTER :: XWSNOW_GR         ! Snow reservoir
REAL, DIMENSION(:), POINTER :: XRSNOW_GR         ! snow density
REAL, DIMENSION(:), POINTER :: XTSNOW_GR         ! snow temperature
REAL, DIMENSION(:), POINTER :: XLWCSNOW_GR         ! snow liquid water conten
REAL, DIMENSION(:), POINTER :: XAGESNOW_GR         ! snow age
REAL                        :: XASNOW_GR         ! snow albedo
!
REAL                        :: XWR_DEF        ! default for leaves interception reservoir
!--------------------------------------------------------------------------
!
!* normalized dimensions for interpolation grids for soil
!
INTEGER, PARAMETER           :: NGRID_LEVEL = 6 
REAL, DIMENSION(NGRID_LEVEL) :: XGRID_SOIL = (/ 0.004, 0.036, 0.068, 0.100, 0.125, 0.150/)
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_TEB_GREENROOF


