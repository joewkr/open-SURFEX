!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     ######################
      MODULE MODD_TOPD_PAR
!     ######################
!
!!****  *MODD_TOPD_PAR* - declaration of parameter variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the variables 
!     which have the PARAMETER attribute   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_PARAMETER)
!!          
!!    AUTHOR
!!    ------
!!      V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    4/07/94                      
!!      Modification 10/03/95 (I.Mallet)   add the coupling files maximum number
!!      Modification 10/04/95 (Ph. Hereil) add the budget related informations
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
REAL,    PARAMETER :: XSTEPK = 0.05   ! discretization step of the saturation 
                                      ! index KAPPA
INTEGER, PARAMETER :: NDIM = 20       ! dimension of the XCONN array third 
                                      ! index 
INTEGER, PARAMETER :: JPCAT = 10      ! number max of catchments
!
INTEGER :: NUNIT = 19
!
!values for each catchment
REAL, DIMENSION(JPCAT) :: XF_PARAM_BV
REAL, DIMENSION(JPCAT) :: XC_DEPTH_RATIO_BV
!
END MODULE MODD_TOPD_PAR
