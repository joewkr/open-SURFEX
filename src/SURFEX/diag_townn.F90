!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_TOWN_n (DLO, DL, DLC, TD, HTOWN, HPROGRAM, DUP, DUPC, KMASK )
!     ######################################################################
!
!!****  *DIAG_TOWN_n * - Chooses the surface schemes for town diagnostics
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : new diag
!!      Modified    09/2012 : new PLEI diag required by atmospheric model
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_DIAG_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XTT, XLSTT, XLVTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DLO
TYPE(DIAG_t), INTENT(INOUT) :: DL
TYPE(DIAG_t), INTENT(INOUT) :: DLC
TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
!
 CHARACTER(LEN=*), INTENT(IN) :: HTOWN
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
TYPE(DIAG_t), INTENT(INOUT) :: DUP
TYPE(DIAG_t), INTENT(INOUT) :: DUPC
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(DUP%XRN)) :: ZDELTA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_TOWN_N',0,ZHOOK_HANDLE)
IF (HTOWN=='TEB   ') THEN

  CALL DIAG(TD%O, TD%D, HPROGRAM, DUP, KMASK)
!
!!!!! important, diagd should be computed in teb !!!!!!
!
! diag not yet inplemeted for TEB (these diag are required for the climate model)
!
! Ok with atmospheric model but LEI (latent heat of sublimation w/m2), EVAP (total evapotranspiration kg/m2/s),
! and SUBL (sublimation kg/m2/s) must by implemented in TEB as well as theirs cumulative values
! Not good if LCPL_ARP = TRUE in ISBA (ALARO)
!
  IF (SIZE(DUP%XLEI)>0) THEN
    DUP%XLEI (:) = XUNDEF
    DUP%XEVAP(:) = XUNDEF
    DUP%XSUBL(:) = XUNDEF
    WHERE(DUP%XLE(:)/=XUNDEF)
      ZDELTA(:) = MAX(0.0,SIGN(1.0,XTT-DUP%XTS(:)))
      DUP%XEVAP (:) = (DUP%XLE(:) * ZDELTA(:))/XLSTT + (DUP%XLE(:) * (1.0-ZDELTA(:)))/XLVTT
      DUP%XLEI  (:) = DUP%XLE(:) * ZDELTA(:)
      DUP%XSUBL (:) = DUP%XLEI(:)/XLSTT
    ENDWHERE
  ENDIF
!
  IF (TD%O%LSURF_BUDGETC) THEN
    CALL INIT_SURF_BUD(DUPC,XUNDEF)
    DUPC%XEVAP = XUNDEF
    DUPC%XSUBL = XUNDEF
  ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      
ELSE IF (HTOWN=='FLUX  ') THEN
  CALL DIAG_EVAP(DLO, DL, DLC, HPROGRAM, DUP, DUPC, KMASK)          
ELSE IF (HTOWN=='NONE  ') THEN
  CALL INIT_BUD(TD%O, DUP, DUPC, XUNDEF)         
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_TOWN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TOWN_n
