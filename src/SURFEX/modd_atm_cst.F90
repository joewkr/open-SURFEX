!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      MODULE MODD_ATM_CST
!     ###############
!
!!****  *MODD_CST* - declaration of typical characteristics of the atmosphere

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     typical characteristics of the atmosphere
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
!!      Original    july 2003
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
REAL :: XCLIM_T_GRAD = -0.0065 ! climatological vertical temperature gradient
!
END MODULE MODD_ATM_CST
