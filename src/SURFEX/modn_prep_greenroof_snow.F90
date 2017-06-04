!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_GREENROOF_SNOW
!     ##################
!
!!****  *MODN_PREP_GREENROOF_SNOW* - declaration of namelist NAM_PREP_GREENROOF_SNOW
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
!!    Based on modn_prep_garden_snow
!!       
!!    AUTHOR
!!    ------
!!      C. de Munck    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2011
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
 CHARACTER(LEN=3)  :: CSNOW_GR         ! snow scheme
INTEGER           :: NSNOW_LAYER_GR    ! number of snow layers
!
END MODULE MODN_PREP_GREENROOF_SNOW
