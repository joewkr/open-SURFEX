!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_SEAICE_INIT_n (DGO, DI, DIC, DGMSI, OREAD_BUDGETC, S, &
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
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_t
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
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: DI
TYPE(DIAG_t), INTENT(INOUT) :: DIC
TYPE(DIAG_MISC_SEAICE_t), INTENT(INOUT) :: DGMSI
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
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAICE_INIT_N',0,ZHOOK_HANDLE)
!
 CALL ALLOC_BUD(DGO,DI,KLU,KSW)
!
!* cumulative surface energy budget
!
#ifdef SFX_OL
IF (DGO%LSURF_BUDGETC .OR. (LRESTART .AND. .NOT.DGO%LRESET_BUDGETC)) THEN
#else
IF (DGO%LSURF_BUDGETC .OR. .NOT.DGO%LRESET_BUDGETC) THEN
#endif
  !
  CALL ALLOC_SURF_BUD(DIC,KLU,KLU,KSW)
  CALL INIT_SURF_BUD(DIC,0.)
  !
ELSE
  CALL ALLOC_SURF_BUD(DIC,0,0,0)
ENDIF
!
!* Seaice model diagnostics init 
!
IF (DGMSI%LDIAG_MISC_SEAICE) THEN
  ALLOCATE(DGMSI%XSIT(KLU))
  ALLOCATE(DGMSI%XSND(KLU)) 
  ALLOCATE(DGMSI%XMLT(KLU))  
  DGMSI%XSIT=XUNDEF
  DGMSI%XSND=XUNDEF
  DGMSI%XMLT=XUNDEF
ELSE
  ALLOCATE(DGMSI%XSIT  (0))
  ALLOCATE(DGMSI%XSND  (0))
  ALLOCATE(DGMSI%XMLT  (0))
ENDIF
!
IF(LCPL_SEAICE.OR.S%LHANDLE_SIC)THEN
  ALLOCATE(S%XCPL_SEAICE_SNET(KLU))
  ALLOCATE(S%XCPL_SEAICE_HEAT(KLU))
  ALLOCATE(S%XCPL_SEAICE_EVAP(KLU))
  S%XCPL_SEAICE_SNET(:) = 0.0
  S%XCPL_SEAICE_HEAT(:) = 0.0
  S%XCPL_SEAICE_EVAP(:) = 0.0
ELSE
  ALLOCATE(S%XCPL_SEAICE_SNET(0))
  ALLOCATE(S%XCPL_SEAICE_HEAT(0))
  ALLOCATE(S%XCPL_SEAICE_EVAP(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAICE_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAICE_INIT_n
