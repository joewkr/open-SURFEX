!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_TREEDRAG
!!    #####################
!!
!!*** *MODD_TREEDRAG*
!!
!!    PURPOSE
!!    -------
!       Declaration to take into account tree drag in the atmospheric model
!              instead of SURFEX. The Z0 forest is therefore reduced to
!              the Z0 grass
!!
!!**  AUTHOR
!!    ------
!!    C.Lac                   *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 30/06/11
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
LOGICAL    ::     LTREEDRAG=.FALSE.    ! flag used to  take into account tree drag in 
!                                      ! the atmospheric model instead of SURFEX.
!
END MODULE MODD_TREEDRAG
