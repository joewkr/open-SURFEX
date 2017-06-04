!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###################
      MODULE MODD_TEB_VEG
!     ###################
!
!!****  *MODD_TEB_VEG * - declaration of constant parameters

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare the 
!       constant flags for agricultural practices, assimilation scheme,
!       ST and soil water ice contents & deep soil fields
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
!!      C. de Munck & A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
INTEGER, PARAMETER       :: NTIME_GR_MAX  = 12
INTEGER, PARAMETER       :: NLAYER_GR_MAX = 6
!
END MODULE MODD_TEB_VEG
