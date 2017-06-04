!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_SEAFLUX_INIT_n (DOC, DGO, D, DC, OREAD_BUDGETC, S, &
                                      HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_SEAFLUX_INIT_n* - routine to initialize SEAFLUX diagnostic variables
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : cumulative sea flux 
!!      B. decharme 04/2013 : Add EVAP and SUBL diag
!!      S.Senesi    01/2014 : introduce fractional seaice 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_DIAG
!
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
#ifdef SFX_OL
USE MODN_IO_OFFLINE,     ONLY : LRESTART
#endif
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_SEA,LCPL_SEAICE
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
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DOC
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
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
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_INIT_N',0,ZHOOK_HANDLE)
!
!* surface energy budget
!
 CALL ALLOC_BUD(DGO,D,KLU,KSW)
!
IF (DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
  !
  ALLOCATE(D%XEVAP   (KLU))
  ALLOCATE(D%XSUBL   (KLU))
  ALLOCATE(D%XALBT   (KLU))  
  !
  D%XEVAP    = XUNDEF
  D%XSUBL    = XUNDEF  
  D%XALBT    = XUNDEF  
  !
ELSE
  !
  ALLOCATE(D%XEVAP   (0))
  ALLOCATE(D%XSUBL   (0))  
  ALLOCATE(D%XALBT   (0)) 
  ! 
ENDIF
!
ALLOCATE(D%XTSRAD(KLU))
D%XTSRAD = XUNDEF
!
!* cumulative surface energy budget
!
#ifdef SFX_OL
IF (DGO%LSURF_BUDGETC .OR. (LRESTART .AND. .NOT.DGO%LRESET_BUDGETC)) THEN
#else
IF (DGO%LSURF_BUDGETC .OR. .NOT.DGO%LRESET_BUDGETC) THEN
#endif
  ! 
  CALL ALLOC_SURF_BUD(DC,KLU,KLU,KSW)  
  ALLOCATE(DC%XEVAP (KLU))
  ALLOCATE(DC%XSUBL (KLU))  
  !
  IF (.NOT.OREAD_BUDGETC .OR. OREAD_BUDGETC.AND.DGO%LRESET_BUDGETC) THEN
    CALL INIT_SURF_BUD(DC,0.)
    DC%XEVAP = 0.0
    DC%XSUBL = 0.0
  ELSE
     CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)    
     YREC='RNC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XRN,IRESP)
     YREC='HC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XH,IRESP)
     YREC='LEC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XLE,IRESP)
     YREC='LEIC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XLEI,IRESP) 
     YREC='GFLUXC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XGFLUX,IRESP)
     YREC='SWDC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XSWD,IRESP)
     YREC='SWUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XSWU,IRESP)
     YREC='LWDC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XLWD,IRESP)
     YREC='LWUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XLWU,IRESP)
     YREC='FMUC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XFMU,IRESP)
     YREC='FMVC_SEA'
     CALL READ_SURF(HPROGRAM,YREC,DC%XFMV,IRESP)
     IF (IVERSION<8)THEN
       DC%XEVAP     = 0.0
       DC%XSUBL     = 0.0              
     ELSE
       !
       YREC='EVAPC_SEA'
       CALL READ_SURF(HPROGRAM,YREC,DC%XEVAP,IRESP)
       YREC='SUBLC_SEA'
       CALL READ_SURF(HPROGRAM,YREC,DC%XSUBL,IRESP)
     ENDIF
  ENDIF   
ELSE
  CALL ALLOC_SURF_BUD(DC,0,0,0)
ENDIF
!
!* ocean diag
!
IF (DOC%LDIAG_OCEAN) THEN
  ALLOCATE(DOC%XTOCMOY  (KLU))
  ALLOCATE(DOC%XSOCMOY  (KLU))
  ALLOCATE(DOC%XUOCMOY  (KLU))
  ALLOCATE(DOC%XVOCMOY  (KLU))
  ALLOCATE(DOC%XDOCMOY  (KLU))
  !
  DOC%XTOCMOY(:)=XUNDEF
  DOC%XSOCMOY(:)=XUNDEF
  DOC%XUOCMOY(:)=XUNDEF
  DOC%XVOCMOY(:)=XUNDEF
  DOC%XDOCMOY(:)=XUNDEF
ELSE
  ALLOCATE(DOC%XTOCMOY  (0))
  ALLOCATE(DOC%XSOCMOY  (0))
  ALLOCATE(DOC%XUOCMOY  (0))
  ALLOCATE(DOC%XVOCMOY  (0))
  ALLOCATE(DOC%XDOCMOY  (0))
ENDIF
!
!
!* Earth system model coupling variables
!
IF(LCPL_SEA.OR.S%LHANDLE_SIC)THEN
!        
  ALLOCATE(S%XCPL_SEA_WIND(KLU))
  ALLOCATE(S%XCPL_SEA_FWSU(KLU))
  ALLOCATE(S%XCPL_SEA_FWSV(KLU))
  ALLOCATE(S%XCPL_SEA_SNET(KLU))
  ALLOCATE(S%XCPL_SEA_HEAT(KLU))
  ALLOCATE(S%XCPL_SEA_EVAP(KLU))
  ALLOCATE(S%XCPL_SEA_RAIN(KLU))
  ALLOCATE(S%XCPL_SEA_SNOW(KLU))
  ALLOCATE(S%XCPL_SEA_FWSM(KLU))
  S%XCPL_SEA_WIND(:) = 0.0
  S%XCPL_SEA_FWSU(:) = 0.0
  S%XCPL_SEA_FWSV(:) = 0.0
  S%XCPL_SEA_SNET(:) = 0.0
  S%XCPL_SEA_HEAT(:) = 0.0
  S%XCPL_SEA_EVAP(:) = 0.0
  S%XCPL_SEA_RAIN(:) = 0.0
  S%XCPL_SEA_SNOW(:) = 0.0
  S%XCPL_SEA_FWSM(:) = 0.0
!
ELSE
  ALLOCATE(S%XCPL_SEA_WIND(0))
  ALLOCATE(S%XCPL_SEA_FWSU(0))
  ALLOCATE(S%XCPL_SEA_FWSV(0))
  ALLOCATE(S%XCPL_SEA_SNET(0))
  ALLOCATE(S%XCPL_SEA_HEAT(0))
  ALLOCATE(S%XCPL_SEA_EVAP(0))
  ALLOCATE(S%XCPL_SEA_RAIN(0))
  ALLOCATE(S%XCPL_SEA_SNOW(0))
  ALLOCATE(S%XCPL_SEA_FWSM(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAFLUX_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAFLUX_INIT_n
