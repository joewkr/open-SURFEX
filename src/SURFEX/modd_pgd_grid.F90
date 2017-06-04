!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_PGD_GRID
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
IMPLICIT NONE
!
!*       0.   DECLARATIONS
!             ------------
!
 CHARACTER(LEN=10)           :: CGRID       ! type of grid
INTEGER                     :: NL          ! number of points of the surface fields
LOGICAL, DIMENSION(720,360) :: LLATLONMASK ! mask where data are to be read
REAL, POINTER, DIMENSION(:) :: XGRID_PAR   ! lits of parameters used to define the grid
INTEGER                     :: NGRID_PAR   ! size of XGRID_PAR
REAL                        :: XMESHLENGTH ! average mesh length/width (decimal degre)
!
END MODULE MODD_PGD_GRID
