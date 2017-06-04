!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!####################
MODULE MODD_UTCI
!####################
!
!!****  *MODD_UTCI - declaration of surface parameters for thermal index
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
!!      V. Masson *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2013
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
INTEGER,   PARAMETER :: NUTCI_STRESS = 10  ! number of stress ranges
!
REAL,             PARAMETER, DIMENSION(0:NUTCI_STRESS) :: XUTCI_STRESS_LIMITS = &
                                 (/ -999., -40., -28., -12., 0., 9., 26., 32., 38., 46., 999. /)
CHARACTER(LEN=3), PARAMETER, DIMENSION(NUTCI_STRESS)   :: CUTCI_STRESS_NAMES  = &
                                 (/ 'ECS', 'VCS', 'SCS', 'MCS', 'LCS', 'NHS', 'MHS', 'SHS', 'VHS', 'EHS' /)
!-----------------------------------------------------------------------------------------------------
!
END MODULE MODD_UTCI
