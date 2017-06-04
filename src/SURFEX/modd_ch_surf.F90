!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_CH_SURF
!     ######################
!
!!
!!    PURPOSE
!!    -------
!  this module is for the surface scheme only     
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
!!  P. Tulet  (16/01/01) *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER,PARAMETER :: JPEMISMAX_F = 10000
INTEGER,PARAMETER :: JPEMISMAX_S = 1000
INTEGER,PARAMETER :: JPSNAPMAX = 50
!
REAL, SAVE, DIMENSION(:),   ALLOCATABLE :: XSREALMASSMOLVAL ! final molecular
                                                            ! diffusivity value
REAL, SAVE, DIMENSION(:),   ALLOCATABLE :: XSREALREACTVAL   ! final chemical
                                                            ! reactivity factor
                                                            ! with biology
REAL, SAVE, DIMENSION(:,:), ALLOCATABLE :: XSREALHENRYVAL   ! chemical Henry
                                                            ! constant value
!
END MODULE MODD_CH_SURF


