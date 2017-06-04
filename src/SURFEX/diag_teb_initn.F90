!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_TEB_INIT_n (DGO, D, DUT, HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_TEB_INIT_n* - routine to initialize TEB diagnostic variables
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!       V. Masson  10/2013 Adds integrated UTCI diagnostics
!       B. decharme 04/2013 : Add DIAG_TS
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF
!
USE MODD_UTCI,              ONLY : NUTCI_STRESS

!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DUT
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',0,ZHOOK_HANDLE)
!
 CALL ALLOC_BUD(DGO,D,KLU,KSW)
!
IF (DGO%LSURF_BUDGET) THEN
  ALLOCATE(D%XSFCO2(KLU))
  D%XSFCO2   = XUNDEF
ELSE
  ALLOCATE(D%XSFCO2(0))  
END IF
!
!* miscellaneous fields
!
IF (DGO%N2M>0 .AND. DUT%LUTCI) THEN
  !
  ALLOCATE(DUT%XUTCI_IN       (KLU))
  ALLOCATE(DUT%XUTCI_OUTSUN   (KLU))
  ALLOCATE(DUT%XUTCI_OUTSHADE (KLU))
  ALLOCATE(DUT%XTRAD_SUN      (KLU))
  ALLOCATE(DUT%XTRAD_SHADE    (KLU))
  ALLOCATE(DUT%XUTCIC_IN      (KLU,NUTCI_STRESS))
  ALLOCATE(DUT%XUTCIC_OUTSUN  (KLU,NUTCI_STRESS))
  ALLOCATE(DUT%XUTCIC_OUTSHADE(KLU,NUTCI_STRESS))
  !
  DUT%XUTCI_IN        = XUNDEF
  DUT%XUTCI_OUTSUN    = XUNDEF
  DUT%XUTCI_OUTSHADE  = XUNDEF
  DUT%XTRAD_SUN       = XUNDEF
  DUT%XTRAD_SHADE     = XUNDEF
  DUT%XUTCIC_IN       = 0.
  DUT%XUTCIC_OUTSUN   = 0.
  DUT%XUTCIC_OUTSHADE = 0.
  !  
ELSE
  ALLOCATE(DUT%XUTCI_IN       (0))
  ALLOCATE(DUT%XUTCI_OUTSUN   (0))
  ALLOCATE(DUT%XUTCI_OUTSHADE (0))
  ALLOCATE(DUT%XTRAD_SUN      (0))
  ALLOCATE(DUT%XTRAD_SHADE    (0))        
  ALLOCATE(DUT%XUTCIC_IN      (0,0))
  ALLOCATE(DUT%XUTCIC_OUTSUN  (0,0))
  ALLOCATE(DUT%XUTCIC_OUTSHADE(0,0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_INIT_n
