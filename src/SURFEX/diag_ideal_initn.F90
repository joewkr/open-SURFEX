!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_IDEAL_INIT_n (DGO, D, DC, HPROGRAM, OREAD_BUDGETC, KLU, KSW)
!     #####################
!
!!****  *DIAG_IDEAL_INIT_n* - routine to initialize IDEAL diagnostic variables
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
!!      Original    04/2009 
!!      P. Le Moigne 03/2015: add diagnostics IDEAL case
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_READ_SURF
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
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
!
CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! spectral bands
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_INIT_N',0,ZHOOK_HANDLE)
!
 CALL ALLOC_BUD(DGO,D,KLU,KSW)
!
IF (DGO%LSURF_BUDGET .OR. DGO%LSURF_BUDGETC) THEN
  !
  ALLOCATE(D%XEVAP   (KLU))
  ALLOCATE(D%XSUBL   (KLU))
  ALLOCATE(D%XALBT   (KLU))
  ALLOCATE(D%XSWE    (KLU))  
  !
  D%XEVAP    = XUNDEF
  D%XSUBL    = XUNDEF    
  D%XALBT    = XUNDEF
  D%XSWE     = XUNDEF 
  ! 
ELSE
  !
  ALLOCATE(D%XEVAP   (0))
  ALLOCATE(D%XSUBL   (0))   
  ALLOCATE(D%XALBT   (0))
  ALLOCATE(D%XSWE    (0)) 
  ! 
END IF
!
!* cumulative surface energy budget
!
IF (DGO%LSURF_BUDGETC) THEN
  !   
  CALL ALLOC_SURF_BUD(DC,0,KLU,0)
  !
  ALLOCATE(DC%XEVAP (KLU))
  ALLOCATE(DC%XSUBL (KLU))  
  !
  IF (.NOT. OREAD_BUDGETC .OR. OREAD_BUDGETC.AND.DGO%LRESET_BUDGETC) THEN 
    CALL INIT_SURF_BUD(DC,0.)
    DC%XEVAP = 0.
    DC%XSUBL = 0.
  ELSE
    CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
    IF (IVERSION<8)THEN
      CALL INIT_SURF_BUD(DC,0.) 
      DC%XEVAP = 0.
      DC%XSUBL = 0.
    ELSE
      YREC='RNC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XRN,IRESP)
      YREC='HC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XH,IRESP)
      YREC='LEC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XLE,IRESP)
      YREC='LEIC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XLEI,IRESP)     
      YREC='GFLUXC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XGFLUX,IRESP)
      YREC='SWDC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XSWD,IRESP)
      YREC='SWUC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XSWU,IRESP)
      YREC='LWDC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XLWD,IRESP)
      YREC='LWUC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XLWU,IRESP)
      YREC='FMUC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XFMU,IRESP)
      YREC='FMVC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XFMV,IRESP)
      YREC='EVAPC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XEVAP,IRESP)
      YREC='SUBLC_WAT'
      CALL READ_SURF(HPROGRAM,YREC,DC%XSUBL,IRESP)              
    ENDIF
    !
  ENDIF   
ELSE
  CALL ALLOC_SURF_BUD(DC,0,0,0)
  ALLOCATE(DC%XEVAP (0))
  ALLOCATE(DC%XSUBL (0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_IDEAL_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_IDEAL_INIT_n
