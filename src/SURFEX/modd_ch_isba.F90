!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_CH_ISBA
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!  16/07/03 (P. Tulet)  restructured for externalization
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL, SAVE  :: XRCSANDSO2            ! SO2 sand surface resistance
REAL, SAVE  :: XRCSANDO3             ! O3  sand surface resistance
REAL, SAVE  :: XRCCLAYSO2            ! SO2 clay surface resistance
REAL, SAVE  :: XRCCLAYO3             ! O3  clay surface resistance
REAL, SAVE  :: XRCSNOWSO2            ! SO2 snow surface resistance
REAL, SAVE  :: XRCSNOWO3             ! O3  snow surface resistance
REAL, SAVE  :: XLANDREXT             ! land type for external leaf resistance
!
END MODULE MODD_CH_ISBA


