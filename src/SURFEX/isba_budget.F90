!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE ISBA_BUDGET (IO, PK, PEK, DEK, OWATER_BUDGET, PTSTEP,&
                       PWG_INI, PWGI_INI, PWR_INI, PSWE_INI, PRAIN, PSNOW, PEVAP  )
!     ###############################################################################
!
!!****  *ISBA_BUDGET * - water and energy budget for ISBA
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2012
!!      B. Decharme    01/16 : Bug with flood budget
!!------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XRHOLW
!     
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
!
LOGICAL, INTENT(IN) :: OWATER_BUDGET
!
REAL,                  INTENT(IN) :: PTSTEP     ! timestep of the integration    (s)
!
REAL, DIMENSION(:),    INTENT(IN) :: PWG_INI    ! total wg at t-1                (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PWGI_INI   ! total wgi at t-1               (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PWR_INI    ! total wr at t-1                (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PSWE_INI   ! total swe at t-1               (kg m-2)
!
REAL, DIMENSION(:),    INTENT(IN)  :: PRAIN     ! Rain rate                      (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PSNOW     ! Snow rate                      (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PEVAP     ! total evaporative flux         (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZINPUT
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZOUTPUT
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZTENDENCY
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZICEFLUX
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZSWE_T
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZWG_T
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZWGI_T
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZSNDRIFT
REAL, DIMENSION(SIZE(PEK%XWR)) :: ZEFLOOD
!
INTEGER :: INI, INL, INLS
INTEGER :: JI, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET',0,ZHOOK_HANDLE)
!
!*      1.0    Init
!       -----------
!
INI =SIZE(PEK%XWG,1)
INL =SIZE(PEK%XWG,2)
INLS=SIZE(PEK%TSNOW%WSNOW,2)
!
DEK%XDWG (:) = XUNDEF
DEK%XDWGI(:) = XUNDEF
DEK%XDWR (:) = XUNDEF
DEK%XDSWE(:) = XUNDEF
!
IF (PEK%TSNOW%SCHEME=='3-L'.OR.PEK%TSNOW%SCHEME=='CRO') THEN
  ZSNDRIFT(:) = DEK%XSNDRIFT(:)
ELSE
  ZSNDRIFT(:) = 0.0
ENDIF
!
!*      2.0    Comptut isba water budget in kg/m2/s
!       -------------------------------------------
!
IF(OWATER_BUDGET)THEN
!
! total swe at t in kg/m2
  ZSWE_T(:)=0.0
  DO JL=1,INLS
    DO JI=1,INI
      ZSWE_T(JI) = ZSWE_T(JI)+PEK%TSNOW%WSNOW(JI,JL)
    ENDDO
  ENDDO
!
! total wg and wgi at t in kg/m2
  ZWG_T (:)= 0.0
  ZWGI_T(:)= 0.0
  IF(IO%CISBA=='DIF')THEN
    DO JL=1,INL
      DO JI=1,INI
        IF(PEK%XWG(JI,JL)/=XUNDEF)THEN
          ZWG_T (JI) = ZWG_T (JI)+PEK%XWG(JI,JL) *PK%XDZG(JI,JL)*XRHOLW
          ZWGI_T(JI) = ZWGI_T(JI)+PEK%XWGI(JI,JL)*PK%XDZG(JI,JL)*XRHOLW
        ENDIF
      ENDDO
    ENDDO
  ELSE
    ZWG_T (:) = PEK%XWG (:,2)*PK%XDG(:,2)*XRHOLW
    ZWGI_T(:) = PEK%XWGI(:,2)*PK%XDG(:,2)*XRHOLW
    IF(IO%CISBA=='3-L')THEN
      ZWG_T(:)=ZWG_T(:)+PEK%XWG(:,3)*(PK%XDG(:,3)-PK%XDG(:,2))*XRHOLW
    ENDIF
  ENDIF
!
! Comptut reservoir time tendencies in kg/m2/s
  DEK%XDWG (:) = (ZWG_T   (:)-PWG_INI (:))/PTSTEP
  DEK%XDWGI(:) = (ZWGI_T  (:)-PWGI_INI(:))/PTSTEP
  DEK%XDWR (:) = (PEK%XWR(:)-PWR_INI (:))/PTSTEP
  DEK%XDSWE(:) = (ZSWE_T  (:)-PSWE_INI(:))/PTSTEP
!
! ice calving flux if used
  IF(IO%LGLACIER)THEN
    ZICEFLUX(:)=DEK%XICEFLUX(:)
  ELSE
    ZICEFLUX(:)=0.0
  ENDIF
!
! Floodplains evaporation (kg/m2/s)
  ZEFLOOD(:) = DEK%XLE_FLOOD(:)/PK%XLVTT(:)+DEK%XLEI_FLOOD(:)/PK%XLSTT(:)
!
! total input water in the system at t
  ZINPUT(:)=PRAIN(:)+PSNOW(:)+DEK%XIFLOOD(:)+DEK%XIRRIG_FLUX(:)
!
! total output water in the system at t
  ZOUTPUT(:) = PEVAP  (:)+DEK%XDRAIN  (:)+DEK%XRUNOFF (:) &
             + DEK%XPFLOOD(:)+ZICEFLUX(:)+ZSNDRIFT(:) - ZEFLOOD(:)
!
! total reservoir time tendencies at "t - (t-1)"
  ZTENDENCY(:) = DEK%XDWG(:)+DEK%XDWGI(:)+DEK%XDWR(:)+DEK%XDSWE(:)
!
! isba water budget (dw/dt=in-out) in kg/m2/s
  DEK%XWATBUD(:)=ZTENDENCY(:)-(ZINPUT(:)-ZOUTPUT(:))
!
ENDIF
!
!*      3.0    Comptut isba energy budget in W/m2
!       -----------------------------------------
!
! not yet implemented
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_BUDGET
