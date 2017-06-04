!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_CTRL_ISBA(DGO,OSURF_EVAP_BUDGET,OSURF_MISC_BUDGET,OSURF_MISC_DIF,KLUOUT )  
!     #################################################################################################################
!
!!****  *PREP_CTRL_ISBA* - routine to check that diagnostics are switched off
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
!!      Modified by A.L. Gibelin, 04/2009: add carbon spinup
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODI_PREP_CTRL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
!
LOGICAL,  INTENT(INOUT) :: OSURF_EVAP_BUDGET  ! flag for surface evaporation budget
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_BUDGET  ! flag for surface miscellaneous budget
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_DIF     ! flag for surface miscellaneous dif variables
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',0,ZHOOK_HANDLE)
!
 CALL PREP_CTRL(DGO,KLUOUT)
!
DGO%N2M = 0
!
DGO%LPATCH_BUDGET     = .FALSE.
!
OSURF_EVAP_BUDGET = .FALSE.
OSURF_MISC_BUDGET = .FALSE.
OSURF_MISC_DIF    = .FALSE.
!
WRITE(KLUOUT,*)'ISBA DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_ISBA
