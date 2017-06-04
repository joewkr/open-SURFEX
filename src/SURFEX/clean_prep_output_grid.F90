!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################
      SUBROUTINE CLEAN_PREP_OUTPUT_GRID
!     #######################################
!!
!!    PURPOSE
!!    -------
!!    Computes variables used for interpolation
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PREP, ONLY : XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT,LINTERP 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
!------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CLEAN_PREP_OUTPUT_GRID',0,ZHOOK_HANDLE)
DEALLOCATE(XLAT_OUT)
DEALLOCATE(XLON_OUT)
DEALLOCATE(XX_OUT  )
DEALLOCATE(XY_OUT  )
!
DEALLOCATE(LINTERP )
IF (LHOOK) CALL DR_HOOK('CLEAN_PREP_OUTPUT_GRID',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLEAN_PREP_OUTPUT_GRID
