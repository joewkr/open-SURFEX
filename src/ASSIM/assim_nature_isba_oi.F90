!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_NATURE_ISBA_OI (IO, S, K, NP, NPE, HPROGRAM, KI, &
                                PRRCL,    PRRSL,  PRRCN,   PRRSN, &
                                PATMNEB,  PITM,   PEVAPTR, PEVAP, &
                                PSNC,     PTSC,   PUCLS, PVCLS,   &
                                PTS_O,    PT2M_O, PHU2M_O, PSWE,  &
                                HTEST, OD_MASKEXT, PLON_IN, PLAT_IN )

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Routine to perform OI within SURFEX 
!  a soil analysis for water content and temperature 
!  using the Meteo-France optimum interpolation technique of Giard and Bazile (2000)
!
!  Derived from CANARI subroutines externalized by Lora Taseva (Dec. 2007)
!
!  Author : Jean-Francois Mahfouf (01/2008)
!
!  Modifications : 
!   (05/2008)  : The I/O of this version follow the newly available LFI format in SURFEX  
!   (01/2009)  : Read directly atmospheric FA files using XRD library instead of using "edf"
!   (06/2009)  : Modifications to allow the assimilation of ASCAT superficial soil moisture
!   (09/2010)  : More parameters to goto_surfex
!   (03/2011)  : Initialization of ZEVAPTR (F.Bouyssel)
!   (07/2011)  : Read pgd+prep (B. Decharme)
!   (04/2012)  : Made as a subroutine (T. Aspelien)
!   (06/2013)  : Separating IO (T. Aspelien)
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_CSTS,            ONLY : XDAY, XPI, XRHOLW, XLVTT, NDAYSEC
USE MODD_SURF_PAR,        ONLY : XUNDEF 
!
USE MODD_ASSIM,           ONLY : LOBSWG, NITRAD, NPRINTLEV, NECHGU, XRD1, XRSCALDW,   &
                                 XRTHR_QC, XSIGWGB, XSIGWGO, XSIGWGO_MAX, XAT2M_ISBA, &
                                 XAHU2M_ISBA, XAZON10M_ISBA, XAMER10M_ISBA
!

USE YOMHOOK,              ONLY : LHOOK,   DR_HOOK
USE PARKIND1,             ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_OI_BC_SOIL_MOISTURE
USE MODI_OI_CACSTS
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN) :: KI
REAL, DIMENSION(KI), INTENT(IN) :: PRRCL
REAL, DIMENSION(KI), INTENT(IN) :: PRRSL
REAL, DIMENSION(KI), INTENT(IN) :: PRRCN
REAL, DIMENSION(KI), INTENT(IN) :: PRRSN
REAL, DIMENSION(KI), INTENT(IN) :: PATMNEB
REAL, DIMENSION(KI), INTENT(IN) :: PITM
REAL, DIMENSION(KI), INTENT(IN) :: PEVAPTR
REAL, DIMENSION(KI), INTENT(IN) :: PEVAP
REAL, DIMENSION(KI), INTENT(IN) :: PSNC
REAL, DIMENSION(KI), INTENT(IN) :: PTSC
REAL, DIMENSION(KI), INTENT(IN) :: PUCLS
REAL, DIMENSION(KI), INTENT(IN) :: PVCLS
REAL, DIMENSION(KI), INTENT(IN) :: PTS_O
REAL, DIMENSION(KI), INTENT(IN) :: PT2M_O
REAL, DIMENSION(KI), INTENT(IN) :: PHU2M_O
REAL, DIMENSION(KI), INTENT(OUT):: PSWE
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL,  DIMENSION (KI) ::  OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON_IN
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT_IN

!    Declarations of local variables
!
! Arrays for soil OI analysis
!
REAL, DIMENSION (KI) :: ZSAB, ZARG, ZWS, ZWP, ZTL, ZWS0, ZWP0, ZTL0,         &
                        ZTS, ZTP, ZSNS, ZTCLS, ZHCLS, ZD2, ZUCLS, ZVCLS,     &
                        ZRSMIN, ZLAI, ZVEG, ZSNS0, ZTS0, ZTP0,               &
                        ZIVEG,  ZSM_O, ZSIG_SMO, ZLSM_O, ZWS_O, ZLON, ZLAT,  &           
                        ZT2INC, ZH2INC, ZWGINC, ZWPINC1, ZWPINC2, ZWPINC3,   &
                        ZT2MBIAS, ZH2MBIAS, ZALBF, ZEMISF, ZZ0F, ZZ0H,       &
                        ZWSC, ZWPC, ZTPC, ZEVAP, ZEVAPTR, ZSSTC,             &
                        ZGELAT, ZGELAM, ZGEMU,  ZWSINC, ZWPINC, ZTLINC,      &
                        ZSNINC, ZTSINC, ZTPINC
!                 
REAL :: ZTHRES
INTEGER  :: IDAT
INTEGER  :: IYEAR                      ! current year (UTC)
INTEGER  :: IMONTH                     ! current month (UTC)
INTEGER  :: IDAY                       ! current day (UTC)
INTEGER  :: ISSSSS                     ! current time since start of the run (s)
INTEGER  :: JI, JJ, JP, JL, IMASK
INTEGER  :: INOBS   ! number of observations
REAL(KIND=JPRB)      :: ZHOOK_HANDLE
!
! ----------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_OI',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_ISBA_OI: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

IF ( NPRINTLEV > 0 ) THEN 
  WRITE(*,*) '--------------------------------------------------------------------------'
  WRITE(*,*) '|                                                                        |'
  WRITE(*,*) '|                             ENTER OI_ASSIM                             |'
  WRITE(*,*) '|                                                                        |'
  WRITE(*,*) '--------------------------------------------------------------------------'
ENDIF
!
!   Update some constants dependant from NACVEG
!
!  scaling of soil moisture increments when assimilation window is different
!  from 6 hours
XRSCALDW = REAL(NECHGU)/6.0
!  half assimilation window in sec
NITRAD = NECHGU*1800
!
!   Time initializations 
!
IYEAR  = S%TTIME%TDATE%YEAR
IMONTH = S%TTIME%TDATE%MONTH
IDAY   = S%TTIME%TDATE%DAY
ISSSSS = S%TTIME%TIME
IF ( ISSSSS>NDAYSEC ) ISSSSS = ISSSSS - NDAYSEC
IDAT = IYEAR*10000. + IMONTH*100. + IDAY
!
!
DO JI=1,KI     
  ZGELAM(JI) = PLON_IN(JI)  
  ZGELAT(JI) = PLAT_IN(JI)
ENDDO
!
DO JI=1,KI
  ZGEMU (JI) = SIN(ZGELAT(JI)*XPI/180.)
ENDDO
!
!
JP = 1
JL = 1
!
!
ZSAB  (:) = K%XSAND(:,JP)*100.
ZARG  (:) = K%XCLAY(:,JP)*100.
!
ZTCLS (:) = XAT2M_ISBA   (:,JP)
ZHCLS (:) = XAHU2M_ISBA  (:,JP)
!
ZD2   (:) = XUNDEF
ZTS0  (:) = XUNDEF
ZTP0  (:) = XUNDEF
ZWS0  (:) = XUNDEF
ZWP0  (:) = XUNDEF
ZTL0  (:) = XUNDEF
ZSNS0 (:) = XUNDEF
ZRSMIN(:) = XUNDEF
ZLAI  (:) = XUNDEF
ZVEG  (:) = XUNDEF
!
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)
  !
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !
    ZD2   (IMASK) = PK%XDG   (JI,2)
    !
    ZTS0  (IMASK) = PEK%XTG  (JI,1)
    ZTP0  (IMASK) = PEK%XTG  (JI,2)
    !
    ZWS0  (IMASK) = PEK%XWG  (JI,1)
    ZWP0  (IMASK) = PEK%XWG  (JI,2)
    ZTL0  (IMASK) = PEK%XWGI (JI,2)
    !
    ZSNS0 (IMASK) = PEK%TSNOW%WSNOW(JI,JL)
    !
    ZRSMIN(IMASK) = PEK%XRSMIN(JI)
    ZLAI  (IMASK) = PEK%XLAI  (JI)
    ZVEG  (IMASK) = PEK%XVEG  (JI)
    !
  ENDDO
ENDDO
!
ZSNS (:) = ZSNS0(:)
!
ZUCLS(:) = PUCLS(:)
ZVCLS(:) = PVCLS(:)
!
ZWPINC1 (:) = XUNDEF
ZWPINC2 (:) = XUNDEF
ZWPINC3 (:) = XUNDEF
ZT2MBIAS(:) = XUNDEF
ZH2MBIAS(:) = XUNDEF
!
! Sea-ice surface properties
ZALBF (:) = XUNDEF
ZEMISF(:) = XUNDEF
ZZ0F  (:) = XUNDEF
ZZ0H  (:) = XUNDEF
!
! Climatological arrays set to missing values
ZWSC(:) = XUNDEF
ZWPC(:) = XUNDEF
ZTPC(:) = XUNDEF
!
! PRINT 
!
IF ( NPRINTLEV > 1 ) THEN
  WRITE(*,*) 'value in PREP file => WG1       ',SUM(ZWS0)/KI
  WRITE(*,*) 'value in PREP file => WG2       ',SUM(ZWP0)/KI
  WRITE(*,*) 'value in PREP file => TG1       ',SUM(ZTS0)/KI
  WRITE(*,*) 'value in PREP file => TG2       ',SUM(ZTP0)/KI
  WRITE(*,*) 'value in PREP file => WGI2      ',SUM(ZTL0)/KI
  WRITE(*,*) 'value in PREP file => WSNOW_VEG1',SUM(ZSNS)/KI
  WRITE(*,*) 'value in PREP file => LAI       ',SUM(ZLAI)/KI
  WRITE(*,*) 'value in PREP file => VEG       ',SUM(ZVEG)/KI
  WRITE(*,*) 'value in PREP file => RSMIN     ',SUM(ZRSMIN)/KI
  WRITE(*,*) 'value in PREP file => DATA_DG2  ',SUM(ZD2)/KI
  WRITE(*,*) 'value in PREP file => SAND      ',SUM(ZSAB)/KI
  WRITE(*,*) 'value in PREP file => CLAY      ',SUM(ZARG)/KI
ENDIF
!
! SST not used in cacsts
ZSSTC(:)    = 0.
!
WHERE ( ZWS0(:)/=XUNDEF )
  ZWS(:) = ZWS0(:) * XRD1   * XRHOLW  ! conversion of m3/m3 -> mm
  ZWP(:) = ZWP0(:) * ZD2(:) * XRHOLW  ! conversion of m3/m3 -> mm
  ZTL(:) = ZTL0(:) * ZD2(:) * XRHOLW  ! conversion of m3/m3 -> mm
ENDWHERE
!
ZEVAP  (:) =  (PEVAP(:)/XLVTT*XDAY)/(NECHGU*3600.) ! conversion W/m2 -> mm/day
ZEVAPTR(:) =  PEVAPTR(:)*XDAY 
!
! Set PIVEG (SURFIND.VEG.DOMI) since it is not available
ZIVEG(:) = 0.0
!
!
!  Read ASCAT SM observations (in percent)
!
INOBS = 0
IF ( LOBSWG ) THEN
  OPEN(UNIT=111,FILE='ASCAT_SM.DAT')
  DO JI=1,KI
    READ(111,*) ZSM_O(JI),ZSIG_SMO(JI),ZLSM_O(JI)
    ! data rejection if not on land
    ! data rejection of error too large
    IF ( ZLSM_O(JI)<1.0 .OR. ZSIG_SMO(JI)>XSIGWGO_MAX ) ZSM_O(JI) = 999.0 
    IF ( ZSM_O(JI)/=999.0 ) INOBS = INOBS + 1
  ENDDO
  CLOSE(UNIT=111)
  IF ( NPRINTLEV > 0 ) WRITE(*,*) 'READ ASCAT SM OK'
ELSE
  ZSM_O   (:) = 999.0
  ZSIG_SMO(:) = 999.0
  ZLSM_O  (:) = 0.0
ENDIF
IF ( NPRINTLEV > 0 ) WRITE(*,*) ' NUMBER OF ASCAT OBSERVATIONS AFTER INITIAL CHECKS  :: ',INOBS
!
! Perform bias correction of SM observations
CALL OI_BC_SOIL_MOISTURE(KI,ZSM_O,ZSAB,ZWS_O)
!
!
! Screen-level innovations
ZT2INC=0.
WHERE ( PT2M_O(:) /= 999.0 ) 
  ZT2INC(:) = PT2M_O (:) - ZTCLS(:)
END WHERE
ZH2INC=0.
WHERE ( PHU2M_O(:) /= 999.0 ) 
  ZH2INC(:) = PHU2M_O(:) - ZHCLS(:)
END WHERE
!
! Avoid division by zero in next WHERE statement; 
! this may occur in the extension zone
WHERE (OD_MASKEXT(1:KI))
  ZD2   (:) = 1.0
  ZT2INC(:) = 0.0
  ZH2INC(:) = 0.0
END WHERE
!
! Threshold for background check
ZTHRES = XRTHR_QC*SQRT(XSIGWGO**2 + XSIGWGB**2)
! Superficial soil moisture innovations in (m3/m3)
INOBS = 0
DO JI=1,KI
  IF ( ZWS_O(JI)/=999.0 ) THEN
    ZWGINC(JI) = ZWS_O(JI) - ZWS(JI)
    IF ( ABS(ZWGINC(JI))>ZTHRES ) THEN 
      ZWGINC(JI) = 0.0 ! background check
    ELSE
      INOBS = INOBS + 1
    ENDIF
  ELSE
    ZWGINC(JI) = 0.0
  ENDIF
ENDDO
IF ( NPRINTLEV > 0 ) THEN
  WRITE(*,*) ' NUMBER OF ASCAT OBSERVATIONS AFTER BACKGROUND CHECK  :: ',INOBS
  WRITE(*,*) 'Mean T2m increments  ',SUM(ZT2INC)/KI
  WRITE(*,*) 'Mean HU2m increments ',SUM(ZH2INC)/KI
ENDIF
!
ZTS(:) = ZTS0(:)
ZTP(:) = ZTP0(:)
write(*,*) 'PERFORMING OI SOIL ANALYSIS'
CALL OI_CACSTS(KI, ZT2INC, ZH2INC, ZWGINC, ZWS_O,                   &
               IDAT, ISSSSS,                                        &
               ZTP, ZWP, ZTL, ZSNS, ZTS, ZWS,                       &
               ZTCLS, ZHCLS, ZUCLS, ZVCLS, ZSSTC,                   &
               ZWPINC1, ZWPINC2, ZWPINC3, ZT2MBIAS, ZH2MBIAS,       &
               PRRCL, PRRSL, PRRCN, PRRSN, PATMNEB, ZEVAP, ZEVAPTR, &
               PITM, ZVEG, ZALBF, ZEMISF, ZZ0F,                     &
               ZIVEG, ZARG, ZD2, ZSAB, ZLAI, ZRSMIN, ZZ0H,          &
               PTSC, ZTPC, ZWSC, ZWPC, PSNC, ZGELAT, ZGELAM, ZGEMU )  
!
!  Perform soil moiture analyses
PSWE(:) = ZSNS(:)
!
ZWSINC(:) = 0.0
ZWPINC(:) = 0.0
ZTLINC(:) = 0.0
ZSNINC(:) = 0.0
WHERE ( ZWS(:)/=XUNDEF )
  ZWSINC(:) = ZWS(:) - ZWS0(:) * XRD1 * XRHOLW
  ZWPINC(:) = ZWP(:) - ZWP0(:) * ZD2(:) * XRHOLW
  ZTLINC(:) = ZTL(:) - ZTL0(:) * ZD2(:) * XRHOLW
  ZSNINC(:) = ZSNS(:) - ZSNS0(:)
END WHERE

! Avoid division by zero in next WHERE statement; 
! this may occur in the extension zone
WHERE (OD_MASKEXT(1:KI)) ZD2(:) = 1.0
!
WHERE (ZWS0(:)/=XUNDEF)
  ZWS0(:)  = ZWS(:)/ (XRD1*XRHOLW)
  ZWP0(:)  = ZWP(:)/ (ZD2(:)*XRHOLW)
  ZTL0(:)  = ZTL(:)/ (ZD2(:)*XRHOLW)
  ZSNS0(:) = ZSNS(:)
ENDWHERE
!  Perform temperature analyses
!
ZTSINC(:) = 0.0
ZTPINC(:) = 0.0
WHERE (ZTS(:)/=XUNDEF)
  ZTSINC(:) = ZTS(:) - ZTS0(:)
  ZTPINC(:) = ZTP(:) - ZTP0(:)
  ZTS0(:)   = ZTS(:)
  ZTP0(:)   = ZTP(:)
END WHERE


! PRINT statistics of the soil analysis

WRITE(*,*) '---------------------------------------------------------------'
WRITE(*,*) 'Mean WS increments over NATURE ',SUM(ZWSINC)/KI
WRITE(*,*) 'Mean WP increments over NATURE ',SUM(ZWPINC)/KI
WRITE(*,*) 'Mean TS increments over NATURE ',SUM(ZTSINC)/KI
WRITE(*,*) 'Mean TP increments over NATURE ',SUM(ZTPINC)/KI
WRITE(*,*) 'Mean TL increments over NATURE ',SUM(ZTLINC)/KI
WRITE(*,*) '---------------------------------------------------------------'

! Update modified variables
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)
  !
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !
    PEK%XWG (JI,1) = ZWS0(IMASK)
    PEK%XWG (JI,2) = ZWP0(IMASK)
    PEK%XTG (JI,1) = ZTS0(IMASK)
    PEK%XTG (JI,2) = ZTP0(IMASK)
    PEK%XWGI(JI,2) = ZTL0(IMASK)
    !
  ENDDO
ENDDO
!
! -------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_OI',1,ZHOOK_HANDLE)
END SUBROUTINE ASSIM_NATURE_ISBA_OI
