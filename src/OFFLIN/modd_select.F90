!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_SELECT
!     ####################
!
!!****  *MODD_SELECT - declaration of surface ATM
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
!!      Original       3/2009
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
LOGICAL    :: LSELECT = .FALSE.
              ! activates output selection from namelist
!
 CHARACTER(LEN=12), DIMENSION(200)    :: CNAME_SELECT
              ! name of output fields in namelist
!
LOGICAL    :: LSELECT_USER
 CHARACTER(LEN=12), DIMENSION(:), POINTER    :: CNAME_USER
!
!-----------------------------------------------------------------------------------------------------
!
END MODULE MODD_SELECT
