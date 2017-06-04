!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_WRITE_SURF_ATM
!!    #####################
!!
!!*** *MODN_WRITE_SURF_ATM*
!!
!!    PURPOSE
!!    -------
!       Namelist for writing into output files
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 02/2008
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE, LSPLIT_PATCH
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_WRITE_SURF_ATM/LNOWRITE_CANOPY, LNOWRITE_TEXFILE, LSPLIT_PATCH
!
END MODULE MODN_WRITE_SURF_ATM
