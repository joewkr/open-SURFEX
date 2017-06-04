!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_FLAKE_INIT_n (OREAD_BUDGETC, DGO, D, DC, DMF, F, &
                                    HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_FLAKE_INIT_n* - routine to initialize FLAKE diagnostic variables
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
!!       V.Masson   10/2013 Adds min and max 2m parameters
!!      B. Decharme  04/2013 new diag
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
#ifdef SFX_OL
USE MODN_IO_OFFLINE,     ONLY : LRESTART
#endif
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_LAKE
!
!
!
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
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! number of SW spectral bands
CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_INIT_N',0,ZHOOK_HANDLE)
!
 CALL ALLOC_BUD(DGO,D,KLU,KSW)
!
IF (DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
  !
  ALLOCATE(D%XEVAP   (KLU))
  ALLOCATE(D%XSUBL   (KLU))  
  ALLOCATE(D%XALBT   (KLU))
  ALLOCATE(D%XSWE    (KLU))
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
#ifdef SFX_OL
IF (DGO%LSURF_BUDGETC .OR. (LRESTART .AND. .NOT.DGO%LRESET_BUDGETC)) THEN
#else
IF (DGO%LSURF_BUDGETC .OR. .NOT.DGO%LRESET_BUDGETC) THEN
#endif
  !
  CALL ALLOC_SURF_BUD(DC,0,KLU,0)  
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
  !  
ELSE
  CALL ALLOC_SURF_BUD(DC,0,0,0)
  ALLOCATE(DC%XEVAP (0))
  ALLOCATE(DC%XSUBL (0))   
ENDIF
!
!* Flake temperature profile
!
IF (DMF%LWATER_PROFILE) THEN
   ALLOCATE (DMF%XZW_PROFILE(COUNT(DMF%XZWAT_PROFILE/= XUNDEF))) 
   ALLOCATE (DMF%XTW_PROFILE(COUNT(DMF%XZWAT_PROFILE/= XUNDEF),KLU)) 
   DMF%XZW_PROFILE=DMF%XZWAT_PROFILE(:COUNT(DMF%XZWAT_PROFILE /= XUNDEF))
 ELSE
   ALLOCATE (DMF%XZW_PROFILE(0)) 
   ALLOCATE (DMF%XTW_PROFILE(0,0)) 
 END IF
!
!* Coupling field with earth systme model
!
IF(LCPL_LAKE)THEN
  !
  ALLOCATE(F%XCPL_FLAKE_EVAP(KLU))
  ALLOCATE(F%XCPL_FLAKE_RAIN(KLU))
  ALLOCATE(F%XCPL_FLAKE_SNOW(KLU))
  F%XCPL_FLAKE_EVAP(:) = 0.0
  F%XCPL_FLAKE_RAIN(:) = 0.0
  F%XCPL_FLAKE_SNOW(:) = 0.0
  !
ELSE
  !
  ALLOCATE(F%XCPL_FLAKE_EVAP(0))
  ALLOCATE(F%XCPL_FLAKE_RAIN(0))
  ALLOCATE(F%XCPL_FLAKE_SNOW(0))
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_FLAKE_INIT_n
