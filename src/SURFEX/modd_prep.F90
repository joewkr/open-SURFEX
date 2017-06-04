!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP
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
!!      P. Le Moigne   10/2005, Phasage Arome      
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=10)  :: CINGRID_TYPE = "          "   ! type of input grid
 CHARACTER(LEN=6)   :: CINTERP_TYPE = "      "   ! type of interpolation
 CHARACTER(LEN=6)   :: CMASK = "      "         ! type of surface
!
LOGICAL, DIMENSION(:), ALLOCATABLE :: LINTERP ! .true. where interpolation must be done
!
REAL, DIMENSION(:), ALLOCATABLE :: XZS_LS   ! Large scale orography interpolated on output grid
!
REAL, DIMENSION(:), ALLOCATABLE :: XLAT_OUT ! Output grid latitudes
REAL, DIMENSION(:), ALLOCATABLE :: XLON_OUT ! Output grid longitudes
!
REAL, DIMENSION(:), ALLOCATABLE :: XX_OUT   ! Output grid 1st coordinate
REAL, DIMENSION(:), ALLOCATABLE :: XY_OUT   ! Output grid 2nd coordinate
!--------------------------------------------------------------------------
REAL, PARAMETER :: XT_CLIM_GRAD = -0.0065   ! climatological vertical temperature gradient
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP
