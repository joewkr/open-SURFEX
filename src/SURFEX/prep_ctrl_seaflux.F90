!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_CTRL_SEAFLUX(DGO,ODIAG_OCEAN,ODIAG_MISC_SEAICE,KLUOUT)  
!     #################################################################################################################
!
!!****  *PREP_CTRL_SEAFLUX* - routine to check that diagnostics are switched off
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
!!      Modified    09/2013 : S. Senesi : manage ODIAG_SEAICE
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
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
!
LOGICAL, INTENT(INOUT) :: ODIAG_OCEAN
LOGICAL,  INTENT(INOUT) :: ODIAG_MISC_SEAICE       ! flag for seaice variables
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_SEAFLUX',0,ZHOOK_HANDLE)
!
 CALL PREP_CTRL(DGO,KLUOUT)
!
ODIAG_OCEAN   = .FALSE.
!
ODIAG_MISC_SEAICE  = .FALSE.
!
WRITE(KLUOUT,*)'SEAFLUX DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_SEAFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_SEAFLUX
