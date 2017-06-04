!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_CTRL(DGO,KLUOUT)  
!     #################################################################################################################
!
!!****  *PREP_CTRL* - routine to check that diagnostics are switched off
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
!
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL',0,ZHOOK_HANDLE)
DGO%N2M = 0
!
DGO%LSURF_BUDGET  = .FALSE.
DGO%L2M_MIN_ZS    = .FALSE.
DGO%LRAD_BUDGET   = .FALSE.
DGO%LCOEF         = .FALSE.
DGO%LSURF_VARS    = .FALSE.
DGO%LSURF_BUDGETC = .FALSE.
!
WRITE(KLUOUT,*)'DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL
