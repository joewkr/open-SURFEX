!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############
      MODULE MODD_DEEPSOIL
!     ###############
!
!!****  *MODD_DEEPSOIL* - declaration of deep soil characteristics

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     calibrated constants for deep soil fields 
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
!!      Original    05/2008
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
LOGICAL                             :: LPHYSDOMC
                                        ! General switch to impose CT
                                        ! and soil water/ice contents
LOGICAL                             :: LDEEPSOIL
                                        ! General switch for deep soil fields
                                        ! (temperature and relaxation time)
!
REAL, PARAMETER, DIMENSION(12)      :: XTDEEP_CLI =(/236.,236.,220.,209.,206.,211.,214.,210.,207.,212.,220.,229./)
                                        ! deep soil temperature (K)
!                                        
REAL, PARAMETER, DIMENSION(12)      :: XGAMMAT_CLI=(/4.,4.,4.,3.,1.,2.,3.,1.,1.,1.,1.,2./)
                                        ! deep soil relaxation time (days)
!
END MODULE MODD_DEEPSOIL
