!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_TIMING
!     ####################
!
!!****  *MODD_TIMING - declaration of surface ATM
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
!!      P. Le Moigne *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2007
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
LOGICAL  :: LTIMING
INTEGER  :: NSEC_SEA
INTEGER  :: NSEC_WAT
INTEGER  :: NSEC_NAT
INTEGER  :: NSEC_TWN
!
END MODULE MODD_TIMING
