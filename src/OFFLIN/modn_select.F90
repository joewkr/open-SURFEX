!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_SELECT
!!    #####################
!!
!!*** *MODN_SELECT*
!!
!!    PURPOSE
!!    -------
!       namelist for output writing selection
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 3/2009
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_SELECT, ONLY : LSELECT,CNAME_SELECT
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_SELECT/LSELECT,CNAME_SELECT
!
END MODULE MODN_SELECT
