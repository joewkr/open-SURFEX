!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_WRITE_COVER_TEX
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Choice of language for tex file
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/2009
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_WRITE_COVER_TEX, ONLY : CLANG
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_WRITE_COVER_TEX/CLANG
!
END MODULE MODN_WRITE_COVER_TEX
