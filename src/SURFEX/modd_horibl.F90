!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_HORIBL
!     ################
!
!!****  *MODD_HORIBL - declaration for field interpolations
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
LOGICAL :: LGLOBLON=.FALSE., LGLOBS=.FALSE., LGLOBN=.FALSE.
REAL :: XILO1H=0., XILO2H=0.

INTEGER, DIMENSION(:,:), ALLOCATABLE :: NO
INTEGER, DIMENSION(:), ALLOCATABLE :: NINLOH
REAL, DIMENSION(:,:), ALLOCATABLE :: XLA
REAL, DIMENSION(:), ALLOCATABLE :: XOLA, XOLO
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NP
REAL, DIMENSION(:,:), ALLOCATABLE :: XLOPH
!
END MODULE MODD_HORIBL
