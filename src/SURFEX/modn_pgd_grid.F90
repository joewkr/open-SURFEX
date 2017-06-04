!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PGD_GRID
!     ##################
!
!!
!!    PURPOSE
!!    -------
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
!!      Original    10/2003                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PGD_GRID, ONLY : CGRID
!
USE MODD_POINT_OVERLAY, ONLY : NOVMX
!
IMPLICIT NONE
!
 CHARACTER(LEN=28):: YINIFILE ! name of input file
 CHARACTER(LEN=6) :: YINIFILETYPE! type of input file
!
!
NAMELIST/NAM_PGD_GRID/CGRID,NOVMX,YINIFILE,YINIFILETYPE
!
END MODULE MODN_PGD_GRID
