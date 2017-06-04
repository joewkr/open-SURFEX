!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_NATURE_ISBA_ENKF(IO, S, K, NP, NPE, HPROGRAM, KI, PT2M, PHU2M, HTEST)

! -----------------------------------------------------------------------------
!
! Land Data Assimilation System based on an Extended Kalman Filter
!
! Revised version : JFM (15 September 2008)
!
! The control vector can be any element of (TG1,TG2,WG1,WG2) - Choice in namelist
!
! The observations can be any element of (T2M,HU2M,WG1) - Choice in namelist
!
! Possibility to evolve the B matrix in the cycling - otherwise SEKF
!
! First version including patches (15 October 2008)
! Trygve Aspelien, Separating IO  06/2013
! Alina Barbu: bug correction of B matrix, otherwise understimation of the gain matrix (11/2013) 
! Alina Barbu: equivalent analysis of B matrix to ensure its symetric and positiv definiteness properties (11/2013) 
  
! -----------------------------------------------------------------------------
!
USE MODD_SURFEX_MPI,    ONLY : NRANK, NPIO
USE MODD_ASSIM,         ONLY : NOBSTYPE, XERROBS, NVAR, NPRINTLEV, CVAR,   &
                               XF_PATCH, XF,COBS,CFILE_FORMAT_OBS,NENS,    &
                               NECHGU, NBOUTPUT, NOBS, XYO, LENKF, LDENKF, &
                               LPB_CORRELATIONS, LPERTURBATION_RUN,        &
                               LBIAS_CORRECTION, XINFL
! 
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
#ifdef SFX_ARO
USE YOMMP0,             ONLY : MYPROC 
#endif
!
USE YOMHOOK,            ONLY : LHOOK,DR_HOOK
USE PARKIND1,           ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_ADD_FORECAST_TO_DATE_SURF
!
USE MODI_OUTER_PRODUCT
USE MODE_EKF
USE MODE_RANDOM
!
! -----------------------------------------------------------
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM     ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL, DIMENSION(:), INTENT(IN) :: PT2M
REAL, DIMENSION(:), INTENT(IN) :: PHU2M
CHARACTER(LEN=2),   INTENT(IN) :: HTEST        ! must be equal to 'OK'
!
!    Declarations of local variables
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
 CHARACTER(LEN=30)  :: YBGFILE
 CHARACTER(LEN=17)  :: YLFNAME
 CHARACTER(LEN=9)   :: YFNAME
 CHARACTER(LEN=7)   :: YMYPROC
 CHARACTER(LEN=1)   :: YCHAR
!
! Local Matrix for Analysis calculation
!
!  Allocation
!  Perturbed simulations
!
! Initial values (to be analysed)
! Observations
!
! Temporary vectors used by the EKF approach
REAL,DIMENSION(KI) :: ZCOFSWI                     ! dynamic range (Wfc - Wwilt)
!
REAL,DIMENSION(NENS+1,NOBSTYPE*NBOUTPUT) :: ZYF            ! Vector of model observations (averaged)
!
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NENS) :: ZINNOV
!
REAL,DIMENSION(NVAR,NOBSTYPE*NBOUTPUT) :: ZBHT
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZR        ! covariance matrix of observation errors
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZK1, ZK2, ZHBHT
REAL,DIMENSION(NOBSTYPE*NBOUTPUT) :: ZP, ZX
!
REAL, DIMENSION(IO%NPATCH,NVAR,NENS) :: ZXINCR
REAL, DIMENSION(IO%NPATCH,NVAR,NENS) :: ZA, ZF
REAL, DIMENSION(IO%NPATCH,NOBSTYPE*NBOUTPUT,NENS) :: ZF_PATCH
REAL, DIMENSION(IO%NPATCH,NVAR) :: ZF_MEAN, ZA_MEAN
REAL, DIMENSION(NVAR) :: ZF_AGG, ZA_AGG
!
REAL,DIMENSION(:,:,:),ALLOCATABLE :: ZF_MEAN0, ZF_PATCH_MEAN
!
REAL :: ZTIME, ZALPHA, ZMIN                   ! current time since start of the run (s)

INTEGER :: IOBSCOUNT
INTEGER :: IYEAR                      ! current year (UTC)
INTEGER :: IMONTH                     ! current month (UTC)
INTEGER :: IDAY                       ! current day (UTC)
INTEGER :: IHOUR
INTEGER :: IRESP                      ! return code
INTEGER :: ISTEP                      ! 
INTEGER :: IMYPROC
INTEGER :: IOBS, IENS
INTEGER :: ISTAT, ICPT, IUNIT
!
INTEGER :: JI,JP,JK,JJ,L,K1,L1,INPATCH
!
LOGICAL :: GBEXISTS
!
REAL(KIND=JPRB)                            :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_ENKF',0,ZHOOK_HANDLE)

#ifdef USE_SODA

!
!############################# BEGINNING ###############################
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_ISBA_ENKF: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF ( NPRINTLEV>0 .AND. NRANK==NPIO) THEN
  WRITE(*,*)
  WRITE(*,*) '   --------------------------'
  WRITE(*,*) '   |   ENTERING  VARASSIM   |'
  WRITE(*,*) '   --------------------------'
  WRITE(*,*)
ENDIF
!
#ifdef SFX_ARO
IF ( MYPROC > 0 ) THEN 
  IMYPROC = MYPROC
ELSE
  IMYPROC = 1
ENDIF
#else
IMYPROC = NRANK+1
#endif
!
WRITE(YMYPROC(1:7),'(I7.7)') IMYPROC
!
IF ( NPRINTLEV > 0 .AND. NRANK==NPIO ) WRITE(*,*) 'number of patches =',IO%NPATCH
!
INPATCH = IO%NPATCH
!
!############################# INITIALISATIONS ###############################
!
!   Read CLAY fraction to  compute the SWI range (Wfc - Wwilt)
!   (XSIGMA is defined in terms of SWI), need to convert to equivalent v/v
!   using same clay fraction in both layers
!   Read SAND fraction to compute the saturation for conversion of ERS SWI
!
 CALL COFSWI(K%XCLAY(:,1),ZCOFSWI)
!DO I=1,KI
  !ZSMSAT (I) = 0.001 * (-1.08*100.*XSAND(I,1) + 494.305)
  !ZWILT  (I) = 0.001 * 37.1342 * ((100.*XCLAY(I,1))**0.5) 
!ENDDO
!
! ====================================================================
!
! Analysis
!
! ====================================================================
!
!   Time reinitialization 
IYEAR  = S%TTIME%TDATE%YEAR
IMONTH = S%TTIME%TDATE%MONTH
IDAY   = S%TTIME%TDATE%DAY
!
IHOUR = 0
ZTIME = FLOAT(NECHGU) * 3600.
!
!############################# READS OBSERVATIONS ###############################
!
! Map the variables in case we read them from CANARI inline/offline FA files
! At the moment only T2M and HU2M can be used. If other variables should be used 
! they must be added to the interface or be read from file.
IF ( TRIM(CFILE_FORMAT_OBS) == "FA" ) THEN
  !
  DO IOBS = 1,NOBSTYPE
    SELECT CASE (TRIM(COBS(IOBS)))   
      CASE("T2M") 
        XYO(:,IOBS) = PT2M(:)
      CASE("HU2M")   
        XYO(:,IOBS) = PHU2M(:)
      CASE("WG1","LAI")  
        CALL ABOR1_SFX("Mapping of "//COBS(IOBS)//" is not defined in ASSIM_NATURE_ISBA_EKF!")
    END SELECT                 
  ENDDO
  !  
ENDIF
!
!//////////////////////TO WRITE OBS/////////////////////////////////////
IF ( NPRINTLEV > 0 ) OPEN (UNIT=111,FILE='OBSout.'//YMYPROC,STATUS='unknown',IOSTAT=ISTAT)
DO JI = 1,KI
  ZMIN = XUNDEF
  DO JP = 1,INPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    !
    DO JJ = 1,PK%NSIZE_P
      IF (PK%NR_P(JJ) == JI) THEN
        IF (PEK%XWGI(JJ,1)<ZMIN) ZMIN = PEK%XWGI(JJ,1)
        EXIT
      ENDIF
    ENDDO
  ENDDO
 IF (ZMIN>0.) THEN
   XYO (JI,:) = XUNDEF
   IF ( NPRINTLEV > 1 ) WRITE(*,*) 'OBSERVATION FOR POINT ',JI,' REMOVED'
 ENDIF
 IF ( NPRINTLEV > 0 ) WRITE (111,*) XYO(JI,:)
ENDDO
IF ( NPRINTLEV > 0 ) CLOSE(111)
!//////////////////////TO WRITE OBS/////////////////////////////////////
!
! Recentering THE FORECAST ENSEMBLE MEMBERS
IF ( LBIAS_CORRECTION ) THEN
  !
  ALLOCATE(ZF_MEAN0(KI,INPATCH,NVAR))
  ALLOCATE(ZF_PATCH_MEAN(KI,INPATCH,NOBS))
  !
  DO JI = 1,KI
    DO JP=1,INPATCH
      DO L = 1,NVAR
        ZF_MEAN0(JI,JP,L) = SUM(XF(JI,JP,1:NENS,L))/REAL(NENS)
      ENDDO
      DO JK = 1,NOBS
        ZF_PATCH_MEAN(JI,JP,JK) = SUM(XF_PATCH(JI,JP,1:NENS,JK))/REAL(NENS)
      ENDDO
    ENDDO
  ENDDO
  !
  DO JI = 1,KI
    DO JP = 1,INPATCH
      DO IENS = 1,NENS
        !
        DO L = 1,NVAR
          IF ( XF(JI,JP,IENS,L) - ZF_MEAN0(JI,JP,L) + XF(JI,JP,NENS+1,L)>0.0 ) THEN
            XF(JI,JP,IENS,L) = XF(JI,JP,IENS,L) - ZF_MEAN0(JI,JP,L) + XF(JI,JP,NENS+1,L)
          ENDIF
        ENDDO
        !
        DO JK = 1,NOBS
          IF ( XF_PATCH(JI,JP,IENS,JK) - ZF_PATCH_MEAN(JI,JP,JK) + XF_PATCH(JI,JP,NENS+1,JK)>0.0 ) THEN
            XF_PATCH(JI,JP,IENS,JK) = XF_PATCH(JI,JP,IENS,JK) - ZF_PATCH_MEAN(JI,JP,JK) + XF_PATCH(JI,JP,NENS+1,JK)
          ENDIF
        ENDDO
        !
      ENDDO
    ENDDO
  ENDDO
  !
  DEALLOCATE(ZF_MEAN0,ZF_PATCH_MEAN)
  !
ENDIF
!
!############################# ANALYSIS ###############################
!
IF ( NPRINTLEV > 0 ) THEN
  IF (NRANK==NPIO) THEN
    WRITE(*,*) 'calculating jacobians',NOBS
    WRITE(*,*) ' and then PERFORMING ANALYSIS'
  ENDIF
  !
  !//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
  ! WRITE OUT OBS AND YERROR FOR DIAGNOSTIC PURPOSES
  OPEN (UNIT=111,FILE='OBSERRORout.'//YMYPROC,STATUS='unknown',IOSTAT=ISTAT)
  ! *** Write innovations in ASCII file ***
  OPEN (unit=112,file='INNOV.'//YMYPROC,status='unknown',IOSTAT=ISTAT)
  ! Write analysis results and increments in ASCJI file
  OPEN (unit=113,file='ANAL_INCR.'//YMYPROC,status='unknown',IOSTAT=ISTAT)
  ! **** Write out the observation operator + Gain matrix ****
  OPEN (unit=114,file='K1.'//YMYPROC,status='unknown',IOSTAT=ISTAT)
  OPEN (unit=115,file='F_AGG.'//YMYPROC,status='unknown',IOSTAT=ISTAT)
  OPEN (unit=116,file='A_AGG.'//YMYPROC,status='unknown',IOSTAT=ISTAT)
ENDIF
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
!
IOBSCOUNT = 0
!
CALL INIT_RANDOM_SEED()
!
DO JI=1,KI
  !
  !---------------- MEAN SIMULATED OBS AVERAGED OVER TILES-----------------------
  ZYF(:,:) = 0.
  DO JP = 1,INPATCH 
    IF (S%XPATCH(JI,JP) > 0.0) THEN
      WHERE ( XF_PATCH(JI,JP,:,:)/=XUNDEF ) 
        ZYF(:,:) = ZYF(:,:) + S%XPATCH(JI,JP)*XF_PATCH(JI,JP,:,:)
      ENDWHERE
    ENDIF
  ENDDO
  IF ( NPRINTLEV > 1 ) WRITE(*,*) 'read in sim obs yf', ZYF(:,1)
  !
  !
  ZR   (:,:) = 0. ! Observation error matrix
  ZINNOV (:,:) = 0.
  !
  ZXINCR(:,:,:) = 0.0
  ZK1(:,:) = 0.0
  ZK2(:,:) = 0.0
  ZP(:) = 0.0
  ZX(:) = 0.0
  ZBHT(:,:) = 0.0
  ZHBHT(:,:) = 0.0
  
  DO ISTEP=1,NBOUTPUT
    !
    DO JK = 1,NOBSTYPE
      !
      K1 = (ISTEP-1)*NOBSTYPE + JK
      !
!--------------------- SET OBSERVATION ERROR ------------------      
      ZR(K1,K1) = XERROBS(JK)*XERROBS(JK)
      IF ( COBS(JK) .EQ. "LAI" ) THEN
        ZR(K1,K1) = ZR(K1,K1) * XYO(JI,K1)*XYO(JI,K1)
      ELSEIF (COBS(JK) .EQ. "WG1") THEN
        ! convert R for wg1 from SWI  to abs value
        ZR(K1,K1) = ZR(K1,K1) * ZCOFSWI(JI)*ZCOFSWI(JI)
      ENDIF
      !
      ! Apply quality control
      IF ( ABS(XYO(JI,K1)-ZYF(NENS+1,K1)) > 0.5 .OR. ZR(K1,K1) < 0. ) XYO(JI,K1) = XUNDEF
      !
      IF( XYO(JI,K1).NE.XUNDEF .AND. XYO(JI,K1).NE.999.0 ) THEN         !if obs available
        !
        DO IENS = 1,NENS
          !
          ZINNOV(K1,IENS) = XYO(JI,K1) - ZYF(IENS,K1)
          IF (LENKF) THEN
            ZINNOV(K1,IENS) = ZINNOV(K1,IENS) + RANDOM_NORMAL() * (XERROBS(JK)*ZCOFSWI(JI))
          ENDIF
          !
        ENDDO
        !
        IOBSCOUNT = IOBSCOUNT + 1
        !
      ENDIF
      !
    ENDDO
    !
    IF ( NPRINTLEV > 0 ) THEN
      WRITE(111,*) ZR(:,:)
      DO JK = 1,NOBS
        WRITE(112,*) (SUM(ZINNOV(JK,:))/(NENS*1.0)), XYO(JI,JK)
      ENDDO
    ENDIF
    !
  ENDDO
  !
  IF (NPRINTLEV > 0) WRITE(*,*) 'PERFORMING ANALYSIS'
  !
  !---------------******  SOIL ANALYSIS *******--------------------------
  !
  DO IENS = 1,NENS
    ZF(:,:,IENS) = XF(JI,:,IENS,:)
    ZF_PATCH(:,:,IENS) = XF_PATCH(JI,:,IENS,:)
  ENDDO

  !
  ZF_MEAN(:,:) = SUM(ZF(:,:,:),DIM=3)/REAL(NENS)
  !
  IF (.NOT.LPERTURBATION_RUN) THEN
    !
    DO JP = 1,INPATCH
      !
      CALL OUTER_PRODUCT(NENS,NVAR,NOBS,ZF(JP,:,:),S%XPATCH(JI,JP)*ZF_PATCH(JP,:,:),&
                         ZBHT(:,:),ZHBHT(:,:),LPB_CORRELATIONS,CVAR,COBS)
      !
      ZK1(:,:) =  ZHBHT(:,:) + ZR(:,:)
      ZK2(:,:) = ZK2(:,:) + ZHBHT(:,:) 
      CALL CHOLDC(NOBS,ZK1(:,:),ZP(:))
      !
      DO IENS = 1,NENS           ! Cholesky decomposition (1)
        !
        CALL CHOLSL(NOBS,ZK1(:,:),ZP(:),ZINNOV(:,IENS),ZX(:))   ! Cholesky decomposition (2)       
        ZXINCR(JP,:,IENS) = MATMUL(ZBHT(:,:),ZX(:))
        !
        DO L = 1,NVAR
          ZA(JP,L,IENS) = ZF(JP,L,IENS)
          IF (CVAR(L)/="WG3" .AND. CVAR(L)/="TG3") THEN
            ZA(JP,L,IENS) = ZA(JP,L,IENS) + ZXINCR(JP,L,IENS)
          ENDIF
        ENDDO
        !
      ENDDO
      !
    ENDDO
    !
    ZA_MEAN(:,:) = SUM(ZA(:,:,:),DIM=3)/REAL(NENS)
    !
    IF (NPRINTLEV>1) WRITE(114,*) (ZK2(:,:) + ZR(:,:))
    !
    DO IENS = 1,NENS
      !
      DO L = 1,NVAR
        !
        DO JP = 1,INPATCH       
          !
          IF ( LDENKF .AND. CVAR(L)/="WG3" .AND. CVAR(L)/="TG3" ) THEN
            !
            DO JK = 1,NOBS
              ZALPHA = 1.0 / ( 1.0 + SQRT( ZR(JK,JK)/(ZK2(JK,JK) + ZR(JK,JK)) ) )
              ZA(JP,L,IENS) = ZA_MEAN(JP,L) + (1.0-ZALPHA) * ( ZF(JP,L,IENS)-ZF_MEAN(JP,L) ) &
                                          + ZALPHA * ( ZA(JP,L,IENS)-ZA_MEAN(JP,L))
            ENDDO
            !
          ENDIF                 
          !apply inflation factor to the ensemble spread
          ZA(JP,L,IENS) = ZA_MEAN(JP,L) + XINFL(L) * (ZA(JP,L,IENS) - ZA_MEAN(JP,L))
          !
        ENDDO
        !
      ENDDO
      !
    ENDDO
    !
    WHERE (ZA(:,:,:)<0.0)
      ZA(:,:,:) = ZF(:,:,:)
    END WHERE
    !
    !
  ELSE
    !
    ZA(:,:,:) = ZF(:,:,:)
    !
  ENDIF
  !
  ZA_MEAN(:,:) = SUM(ZA(:,:,:),DIM=3)/REAL(NENS)
  !
  ZF_AGG(:) = 0.
  ZA_AGG(:) = 0.
  DO L = 1,NVAR
    DO JP = 1,INPATCH
      IF (ZA_MEAN(JP,L)/=XUNDEF .AND. ZF_MEAN(JP,L)/=XUNDEF) THEN
        ZF_AGG(L) = ZF_AGG(L) + S%XPATCH(JI,JP) * ZF_MEAN(JP,L)
        ZA_AGG(L) = ZA_AGG(L) + S%XPATCH(JI,JP) * ZA_MEAN(JP,L)
      ENDIF
    ENDDO
  ENDDO
  !
  IF (NPRINTLEV>1) THEN
    WRITE(115,*) (ZF_AGG(L), L=1,NVAR)
    WRITE(116,*) (ZA_AGG(L), L=1,NVAR)
  ENDIF
  !
  !############################# GET VARIABLES FOR OUTPUT WRITING ###############################
  !
  DO IENS = 1,NENS
    XF(JI,:,IENS,:) = ZA(:,:,IENS)
  ENDDO
  !
  IF (LBIAS_CORRECTION) XF(JI,:,NENS+1,:) = ZA_MEAN(:,:)
  !
ENDDO
!
IF ( NPRINTLEV > 0 ) THEN
  CLOSE(111)
  CLOSE(112)
  CLOSE(113)
  CLOSE(114)
  CLOSE(115)
  CLOSE(116)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_ENKF',1,ZHOOK_HANDLE)

#endif

!
END SUBROUTINE ASSIM_NATURE_ISBA_ENKF
