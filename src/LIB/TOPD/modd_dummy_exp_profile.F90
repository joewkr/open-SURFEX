!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################
      MODULE MODD_DUMMY_EXP_PROFILE
!     ###########################
!
!!****  *MODD_DUMMY_EXP_PROFILE - declaration For special f, dc exponential profile
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
!!     B. Vincendon
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/08/11
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TOPD_PAR, ONLY : JPCAT
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
! **** For special f, dc exponential profile
! values for each isba_mesh
REAL, DIMENSION(:), ALLOCATABLE :: XF_PARAM
REAL, DIMENSION(:), ALLOCATABLE :: XC_DEPTH_RATIO
!values for each catchment
REAL, DIMENSION(JPCAT) :: XF_PARAM_BV
REAL, DIMENSION(JPCAT) :: XC_DEPTH_RATIO_BV
!-------------------------------------------------------------------------------------
!
END MODULE MODD_DUMMY_EXP_PROFILE

