!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############
      MODULE MODD_AGRI
!     ###############
!
!!****  *MODD_AGRI* - declaration of agricultural practices constants

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     typical constants for agricultural practices
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
!!      P. LE MOIGNE *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2006
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
LOGICAL                             :: LAGRIP  
                                        ! General switch for agricultural practices
                                        ! (seeding and irrigation)
!
INTEGER, PARAMETER                  :: JPSTAGE = 4   
                                        ! Number of stages for Irrigation
!
REAL, PARAMETER, DIMENSION(JPSTAGE) :: XTHRESHOLD=(/0.70,0.55,0.40,0.25/)
                                        ! Threshold on f2 for irrigation
!
END MODULE MODD_AGRI
