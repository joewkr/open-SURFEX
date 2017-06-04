!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_ISBA_SNOW
!     ##################
!
!!****  *MODN_PREP_ISBA_SNOW* - declaration of namelist NAM_PREP_ISBA
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_ISBA
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
!!      Original    01/2004                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
CHARACTER(LEN=3)  :: CSNOW          ! snow scheme
INTEGER           :: NSNOW_LAYER    ! number of snow layers
LOGICAL           :: LSWEMAX        ! logical switch to set an upper limit on initial snow water equivalent
REAL              :: XSWEMAX        ! upper limit of initial snow water equivalent
!
END MODULE MODN_PREP_ISBA_SNOW
