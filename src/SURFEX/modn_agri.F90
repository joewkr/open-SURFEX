!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_AGRI
!     ##################
!
!!****  *MODN_AGRI - Agricultural Practices
!!
!!    PURPOSE
!!    -------
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2006
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_AGRI, ONLY:  LAGRIP
!
IMPLICIT NONE
!
NAMELIST/NAM_AGRI/LAGRIP
!
END MODULE MODN_AGRI
