!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_GRID_GRIB
!     ################
!
!!****  *MODD_GRID_GRIB - declaration of GRIB grid characteristics
!!
!!    PURPOSE
!!    -------
!     Used if CINGRID_TYPE = 'GRIB '
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
USE GRIB_API, ONLY : kindOfInt
!
IMPLICIT NONE
!
INTEGER :: NNI ! total number of physical points
!
 CHARACTER(LEN=6)  :: CINMODEL!
 CHARACTER(LEN=28) :: CGRIB_FILE
INTEGER(KIND=kindOfInt) :: NIDX
!
END MODULE MODD_GRID_GRIB
