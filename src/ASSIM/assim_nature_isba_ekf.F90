!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_NATURE_ISBA_EKF (IO, S, K, NP, NPE, HPROGRAM, KI, PT2M, PHU2M, HTEST)

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
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SURFEX_MPI,    ONLY : NRANK, NPIO
!
USE MODD_ASSIM,         ONLY : LBEV, LBFIXED, NOBSTYPE, XERROBS, XQCOBS, NVAR, NNCV, &
                               XSCALE_Q, NPRINTLEV, CVAR, XSIGMA, CBIO, XI,        &
                               XF_PATCH, XF, COBS, XSCALE_QLAI,CFILE_FORMAT_OBS,   &
                               XALPH,NECHGU, NBOUTPUT, XTPRT, XLAI_PASS, XBIO_PASS,&
                               NOBS, XYO
! 
USE MODD_SURF_PAR,      ONLY : XUNDEF
USE MODD_ISBA_PAR,      ONLY : XWGMIN
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
USE MODE_EKF
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
 CHARACTER(LEN=19)  :: YLFNAME
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
!REAL,DIMENSION(KI) :: ZSMSAT                      ! saturation  
!REAL,DIMENSION(KI) :: ZWILT
!
REAL,DIMENSION(KI,IO%NPATCH,NVAR) :: ZCOEF
REAL,DIMENSION(KI,IO%NPATCH,NVAR) :: ZEPS            ! The perturbation amplitude
!
REAL,DIMENSION(NVAR+1,NOBSTYPE) :: ZYF            ! Vector of model observations (averaged) 
!
REAL,DIMENSION(KI*IO%NPATCH*NVAR*IO%NPATCH*NVAR) :: ZBLONG
REAL,DIMENSION(KI,IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZB           ! background error covariance matrix
REAL,DIMENSION(IO%NPATCH*NVAR) :: ZINCR
!
REAL,DIMENSION(IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZLTM         ! linear tangent matrix for the f'ward model
REAL,DIMENSION(IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZQ           ! model error matrix
!
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,IO%NPATCH*NVAR) :: ZHOWR        ! Jacobian of observation operator
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,IO%NPATCH*NVAR) :: ZHO             ! Jacobian of observation operator
REAL,DIMENSION(IO%NPATCH*NVAR,NOBSTYPE*NBOUTPUT) :: ZHOT            ! Transpose of HO
REAL,DIMENSION(IO%NPATCH*NVAR,NOBSTYPE*NBOUTPUT) :: ZGAIN           ! Kalman gain (used explicitly for Ba) 
!
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZR        ! covariance matrix of observation errors
REAL,DIMENSION(NOBSTYPE*NBOUTPUT,NOBSTYPE*NBOUTPUT) :: ZK1
REAL,DIMENSION(NOBSTYPE*NBOUTPUT) :: ZX,ZB2,ZP
!
REAL,DIMENSION(IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZKRK
REAL,DIMENSION(IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZIDKH
REAL,DIMENSION(IO%NPATCH*NVAR,IO%NPATCH*NVAR) :: ZIDENT          ! identitiy matrix, used for Ba
!
REAL,DIMENSION(IO%NPATCH) :: ZVLAIMIN
!
REAL :: ZTIME, ZMIN                 ! current time since start of the run (s)
REAL :: ZB3, ZB4
!
INTEGER :: IOBSCOUNT
INTEGER :: IYEAR                      ! current year (UTC)
INTEGER :: IMONTH                     ! current month (UTC)
INTEGER :: IDAY                       ! current day (UTC)
INTEGER :: IHOUR
INTEGER :: IRESP                      ! return code
INTEGER :: ISTEP                      ! 
INTEGER :: IMYPROC
INTEGER :: IOBS
INTEGER :: ISCREENLEV
INTEGER :: ISTAT, ICPT, IUNIT
!
INTEGER :: JI,JJ,JP,JK,JJP,JL,K1,L1,IMASK,INPATCH
!
LOGICAL :: GBEXISTS
!
REAL(KIND=JPRB)                            :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_EKF',0,ZHOOK_HANDLE)

#ifdef USE_SODA
!
!############################# BEGINNING ###############################
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_ISBA_EKF: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF ( NPRINTLEV>0  .AND. NRANK==NPIO ) THEN
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
IF ( NPRINTLEV > 0  .AND. NRANK==NPIO ) WRITE(*,*) 'number of patches =',IO%NPATCH
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
 !
!DO JI=1,KI
  !ZSMSAT (I) = 0.001 * (-1.08*100.*XSAND(I,1) + 494.305)
  !ZWILT  (I) = 0.001 * 37.1342 * ((100.*XCLAY(I,1))**0.5) 
!ENDDO
!
! Set control variables
ZIDENT(:,:) = 0.                   ! identity matrix
DO JL = 1,NVAR
  !
  DO JP = 1,INPATCH
    ZIDENT(JP+INPATCH*(JL-1),JP+INPATCH*(JL-1)) = 1.0
  ENDDO
  !
  ! XTPRT=XTPRT*(FP-WP) for WG case
  IF ( TRIM(CVAR(JL))=='WG1' .OR. TRIM(CVAR(JL))=='WG2' .OR. &
       TRIM(CVAR(JL))=='WG3' .OR. TRIM(CVAR(JL))=='WG4' .OR. &
       TRIM(CVAR(JL))=='WG5' .OR. TRIM(CVAR(JL))=='WG6' .OR. &
       TRIM(CVAR(JL))=='WG7' .OR. TRIM(CVAR(JL))=='WG8') THEN
    !
    DO JP = 1,INPATCH
      WHERE ( XI(:,JP,JL)/=XUNDEF ) ! not sure that it is necessary
        ZEPS(:,JP,JL) = XTPRT(JL) * ZCOFSWI(:) ! XI(:,JP,JL)
      ELSEWHERE
        ZEPS(:,JP,JL) = 1.
      ENDWHERE
    ENDDO
    !
  ELSEIF (TRIM(CVAR(JL))=='LAI') THEN
    !
    WHERE ( XI(:,:,JL)/=XUNDEF )
      ZEPS(:,:,JL) = XTPRT(JL) * XI(:,:,JL)
    ELSEWHERE
      ZEPS(:,:,JL) = 1.
    ENDWHERE
    !
  ELSE
    !
    ZEPS(:,:,JL) = 1.
    !
  ENDIF
  !
  IF (NPRINTLEV>0) WRITE(*,*) 'ZEPS | ', TRIM(CVAR(JL)), ' : ' , XTPRT(JL)
  !
  IF ( TRIM(CVAR(JL))=='WG1' .OR. TRIM(CVAR(JL))=='WG2' .OR. &
       TRIM(CVAR(JL))=='WG3' .OR. TRIM(CVAR(JL))=='WG4' .OR. &
       TRIM(CVAR(JL))=='WG5' .OR. TRIM(CVAR(JL))=='WG6' .OR. &
       TRIM(CVAR(JL))=='WG7' .OR. TRIM(CVAR(JL))=='WG8') THEN
    !
    DO JI = 1,KI
      ZCOEF(JI,:,JL) = ZCOFSWI(JI)*ZCOFSWI(JI)
    ENDDO
    !
  ELSEIF ( TRIM(CVAR(JL))=='LAI' .AND. LBFIXED ) THEN
    !
    DO JI = 1,KI
      DO JP = 1,INPATCH
        IF ( XLAI_PASS(JI,JP)/=XUNDEF .AND. XLAI_PASS(JI,JP)>=2. ) THEN
          ZCOEF(JI,JP,JL) = XLAI_PASS(JI,JP)*XLAI_PASS(JI,JP)
        ELSE 
          ZCOEF(JI,JP,JL) = 0.4*0.4/(XSIGMA(JL)*XSIGMA(JL))
        ENDIF
      ENDDO
    ENDDO
    !
  ELSE
    !
    ZCOEF(:,:,JL) = 1.
    !
  ENDIF
  !
ENDDO
!
!############################# B CALCULATION ###############################
!
! ----------------------
! VARASSIM OPTION : LBEV
! ----------------------
!   Calculate the LTM, and evolve B. 
!
! Set the B input file depending of an existing B was found or not
YBGFILE = "BGROUNDin."//TRIM(YMYPROC)
INQUIRE (FILE=TRIM(YBGFILE),EXIST=GBEXISTS)
!
IF ( LBEV .AND. GBEXISTS ) THEN
  !
  ZB(:,:,:) = 0.
  CALL B_BIG_LOOP(IO,"READ",YBGFILE,ZB)
  IF ( NPRINTLEV > 0 ) WRITE(*,*) 'read previous B matrix  ==>',ZB(1,1,1),NVAR
  !
ELSEIF ( LBEV .OR. LBFIXED ) THEN
  !
  ! Initialization of B 
  ZB(:,:,:) = 0.
  DO JI = 1,KI
    DO JL = 1,NVAR
      DO JP = 1,INPATCH   
        !
        L1 = JP + INPATCH *(JL-1)
        ZB(JI,L1,L1) = XSIGMA(JL)*XSIGMA(JL) * ZCOEF(JI,JP,JL)
        !
      ENDDO
    ENDDO
    !
  ENDDO
  !
  IF ( LBEV ) THEN
    !
    ZBLONG(:) = 0.
    ICPT = 0
    !
    DO JI = 1,KI
      DO JP = 1,INPATCH
        DO JJP = 1,INPATCH
          DO JL = 1,NVAR
            DO JK = 1,NVAR
              !
              L1 = JP + INPATCH * (JL-1)
              !
              ICPT = ICPT + 1
              ZBLONG(ICPT) = ZB(JI,L1,L1)
              !
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    !
    ZB(:,:,:) = 0.
    CALL B_BIG_LOOP(IO,"BUIL","",ZB,ZBLONG)
    IF ( NPRINTLEV > 0 ) WRITE(*,*) 'Initialized B'
    !
  ENDIF
  !
ELSE
  !
  CALL ABOR1_SFX("LBEV or LBFIXED should be .TRUE.!")
  !
ENDIF
!
IF ( LBEV ) THEN
  !
!//////////////////////TO WRITE LTM/////////////////////////////////////
  IF (NPRINTLEV>0) THEN
    IUNIT = 120
    DO JL=1,NVAR
      DO JK=1,NVAR
        IUNIT = IUNIT + 1
        WRITE(YCHAR,'(I1)') JK
        YLFNAME='LTM_del'//TRIM(CVAR(JK))//'_del'//TRIM(CVAR(JL))//"."//TRIM(YMYPROC)
        OPEN(UNIT=IUNIT,FILE=YLFNAME,FORM='FORMATTED',STATUS='UNKNOWN',POSITION='APPEND')
      ENDDO
    ENDDO
  ENDIF
!/////////////////////TO WRITE LTM////////////////////////////////////////
  DO JI = 1,KI
    !
    ! calculate LTM
    ZLTM(:,:) = 0.0
    IUNIT = 120
    DO JL = 1,NVAR    ! control variable (x at previous time step)
      DO JK = 1,NVAR 
        IUNIT = IUNIT + 1
        DO JP = 1,INPATCH 
          !
          L1 = JP + INPATCH*(JL-1)
          K1 = JP + INPATCH*(JK-1)
          !
          IF ( S%XPATCH(JI,JP)>0.0 .AND. XF(JI,JP,JL+1,JK).NE.XUNDEF .AND. XF(JI,JP,1,JK).NE.XUNDEF ) THEN
            !
            ! Jacobian of fwd model
            ZLTM(L1,K1) = ( XF(JI,JP,JL+1,JK) - XF(JI,JP,1,JK) ) / ZEPS(JI,JP,JL)
            ! impose upper/lower limits 
            ZLTM(L1,K1) = MAX(-0.1, ZLTM(L1,K1))
            ZLTM(L1,K1) = MIN( 1.0, ZLTM(L1,K1))
            !
          ENDIF
          !
          IF (NPRINTLEV>0) WRITE (IUNIT,*) ZLTM(L1,K1)
          !
        ENDDO
      ENDDO
    ENDDO
    !
!//////////////////////TO WRITE LTM/////////////////////////////////////   
    IF (NPRINTLEV>0) THEN
      IUNIT = 120
      DO JL=1,NVAR
        DO JK=1,NVAR
          IUNIT = IUNIT + 1
          CLOSE(IUNIT)
        ENDDO
      ENDDO
      !//////////////////////TO WRITE LTM/////////////////////////////////////
      WRITE(*,*) 'LTM d(wg2)/d(wg2)', ZLTM(1,1)
    ENDIF
    !
    ! evolve B 
    ZB(JI,:,:) = MATMUL(ZLTM(:,:),MATMUL(ZB(JI,:,:),TRANSPOSE(ZLTM(:,:))))
    !
    !
    !   Adding model error to background error matrix 
    ZQ(:,:) = 0.0
    DO JL=1,NVAR
      DO JP=1,INPATCH
        !
        L1 = JP+INPATCH*(JL-1)
        !
        ZQ(L1,L1) = XSIGMA(JL)*XSIGMA(JL)
        !
        IF (TRIM(CVAR(JL)) == 'LAI') THEN
          ZQ(L1,L1) = XSCALE_QLAI*XSCALE_QLAI * ZQ(L1,L1)
        ELSE
          ZQ(L1,L1) = XSCALE_Q*XSCALE_Q * ZQ(L1,L1) * ZCOEF(JI,JP,JL)
        ENDIF
        !
      ENDDO
    ENDDO
    !
    ! B is the forecast matrix - need to add Q
    IF ( NPRINTLEV > 0 ) THEN
      WRITE(*,*) 'B before wg2 wg2 ==> ',ZB(JI,1,1)/ZCOFSWI(JI),ZB(JI,1,1)
      WRITE(*,*) 'Q value wg2 wg2 ==> ',ZQ(1,1)/ZCOFSWI(JI),ZQ(1,1)
    ENDIF
    !
    ZB(JI,:,:) = ZB(JI,:,:) + ZQ(:,:)
    !
    IF ( NPRINTLEV > 0 ) WRITE(*,*) 'B after wg2 wg2 ==>',ZB(JI,1,1)/ZCOFSWI(1),ZB(JI,1,1)
    !
  ENDDO
  !
  ! write out the LTM for the forward model
  ! Write out current B
  IF (NPRINTLEV>0) THEN
    YBGFILE="BGROUNDout_LBEV."//TRIM(YMYPROC)
    CALL B_BIG_LOOP(IO,"WRIT",YBGFILE,ZB)
    WRITE(*,*) 'store B matrix after TL evolution ==>',ZB(1,1,1)
    WRITE(*,*) 'writing out B'
  ENDIF
  !
ENDIF
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
      CASE("WG1","WG2","LAI")  
        CALL ABOR1_SFX("Mapping of "//COBS(IOBS)//" is not defined in ASSIM_NATURE_ISBA_EKF!")
    END SELECT                 
  ENDDO
  !  
ENDIF
!
! ISCREENLEV defines the ground level used to screen observations when soil is frozen
!   Future change: better look at COBS(IOBS) which should be WG1 for 3-L and WG2 for DIF
IF (IO%CISBA=='3-L') ISCREENLEV = 1
IF (IO%CISBA=='DIF') ISCREENLEV = 2
!
!//////////////////////TO WRITE OBS/////////////////////////////////////
IF ( NPRINTLEV > 0 ) OPEN (UNIT=111,FILE='OBSout.'//TRIM(YMYPROC),STATUS='unknown',IOSTAT=ISTAT)
DO JI = 1,KI
  ZMIN = XUNDEF
  DO JP = 1,INPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    !
    DO JJ = 1,PK%NSIZE_P
      IF (PK%NR_P(JJ) == JI) THEN
        IF (PEK%XWGI(JJ,ISCREENLEV)<ZMIN) ZMIN = PEK%XWGI(JJ,ISCREENLEV)
        EXIT
      ENDIF
    ENDDO
  ENDDO
  IF ( ZMIN>0. ) THEN
    XYO (JI,:) = XUNDEF
    !IF ( NPRINTLEV > 0 ) WRITE(*,*) 'OBSERVATION FOR POINT ',JI,' REMOVED'
  ENDIF
  IF ( NPRINTLEV > 0 ) WRITE (111,*) XYO(JI,:)
ENDDO
IF ( NPRINTLEV > 0 ) CLOSE(111)
!//////////////////////TO WRITE OBS/////////////////////////////////////
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
  OPEN (UNIT=111,FILE='OBSERRORout.'//TRIM(YMYPROC),STATUS='unknown',IOSTAT=ISTAT)
  ! *** Write innovations in ASCII file ***
  OPEN (unit=112,file='INNOV.'//TRIM(YMYPROC),status='unknown',IOSTAT=ISTAT)
  ! Write analysis results and increments in ASCII file
  OPEN (unit=113,file='ANAL_INCR.'//TRIM(YMYPROC),status='unknown',IOSTAT=ISTAT)
  ! **** Write out the observation operator + Gain matrix ****
  IUNIT = 150
  DO JL = 1,NVAR
    DO JK=1,NOBSTYPE
      IUNIT = IUNIT + 1
      WRITE(YCHAR,'(I1)') JK
      YFNAME='HO_'//TRIM(CVAR(JL))//'_v'//YCHAR
      OPEN(UNIT=IUNIT,FILE=YFNAME,FORM='FORMATTED',STATUS='UNKNOWN',IOSTAT=ISTAT)
    ENDDO
  ENDDO
ENDIF
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
!
IF (INPATCH==12) THEN
  ZVLAIMIN = (/0.3,0.3,0.3,0.3,1.0,1.0,0.3,0.3,0.3,0.3,0.3,0.3/)
ELSE
  ZVLAIMIN = (/0.3/)
ENDIF
!
DO JP = 1,INPATCH
  ALLOCATE(NP%AL(JP)%XINCR(KI,NVAR))
  NP%AL(JP)%XINCR(:,:) = 0.
  ALLOCATE(NP%AL(JP)%XHO(KI,NOBSTYPE*NBOUTPUT,NVAR))
  NP%AL(JP)%XHO(:,:,:) = 0.
ENDDO
!
ALLOCATE(S%XINNOV(KI,NOBSTYPE*NBOUTPUT))
S%XINNOV(:,:) = 0.
!
ALLOCATE(S%XRESID(KI,NOBSTYPE*NBOUTPUT))
S%XRESID(:,:) = 0.
!
IOBSCOUNT = 0
DO JI=1,KI
  !
!---------------- MEAN SIMULATED OBS AVERAGED OVER TILES-----------------------
  ZYF(:,:) = 0. 
  DO JP=1,INPATCH
    IF (S%XPATCH(JI,JP) > 0.0) THEN
      WHERE ( XF_PATCH(JI,JP,:,:)/=XUNDEF ) 
        ZYF(:,:) = ZYF(:,:) + S%XPATCH(JI,JP)*XF_PATCH(JI,JP,:,:)
      ENDWHERE
    ENDIF
  ENDDO
  !IF ( NPRINTLEV > 0 ) WRITE(*,*) 'read in sim obs yf', ZYF(:,1)
  !
  !
  ZR   (:,:) = 0. ! Observation error matrix
  !
  ZHO  (:,:) = XUNDEF  ! Linearized observation matrix
  ZHOWR(:,:) = XUNDEF
  ZB2  (:)   = XUNDEF  ! Innovation vector
  
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
      ELSEIF (COBS(JK) .EQ. "WG1" .OR. COBS(JK) .EQ. "WG2") THEN
        ! convert R for wg1 from SWI  to abs value
        ZR(K1,K1) = ZR(K1,K1) * ZCOFSWI(JI)*ZCOFSWI(JI)
      ENDIF
      !
      ! Apply quality control
      IF ( ( ABS( XYO(JI,K1)-ZYF(1,JK) ) > XQCOBS(JK) ) .OR. (ZR(K1,K1) .LT. 0 ) ) THEN 
        XYO(JI,K1) = 999.0
      ENDIF
      !      
!--------------------- CALCULATE JACOBIANS ------------------         
      DO JL=1,NVAR
        DO JP=1,INPATCH
          !
          PEK => NPE%AL(JP)
          PK => NP%AL(JP)
          !
          L1 = JP + INPATCH*(JL-1)
          !
          IF ( S%XPATCH(JI,JP)>0.0 .AND. XF_PATCH(JI,JP,JL+1,JK).NE.XUNDEF .AND. &
               XF_PATCH(JI,JP,1,JK).NE.XUNDEF ) THEN
            DO JJ = 1, PK%NSIZE_P
              IF (PK%NR_P(JJ)==JI) EXIT
            ENDDO
            IF (PEK%XWGI(JJ,ISCREENLEV).EQ.0.) THEN 
              ZHOWR(K1,L1) = S%XPATCH(JI,JP)*(XF_PATCH(JI,JP,JL+1,JK) - XF_PATCH(JI,JP,1,JK))/ZEPS(JI,JP,JL)
            ENDIF
          ENDIF
          !
          IF( (XYO(JI,K1).NE.XUNDEF) .AND. (XYO(JI,K1).NE.999.0) ) THEN         !if obs available
            ! Jacobian of obs operator
            ZHO(K1,L1) = S%XPATCH(JI,JP)*(XF_PATCH(JI,JP,JL+1,JK) - XF_PATCH(JI,JP,1,JK))/ZEPS(JI,JP,JL)
            !IF (NPRINTLEV>0) WRITE(*,*) JI,S%XPATCH(JI,JP)*XF_PATCH(JI,JP,JL+1,JK), S%XPATCH(JI,JP)*XF_PATCH(JI,JP,1,JK),ZEPS(JI,JP,JL)
            !IF (NPRINTLEV>0) WRITE(*,*) 'NVAR | OBS | ZHO ', TRIM(CVAR(JL)), ' | ', COBS(JK), ' | ' , ZHO(K1,L1), JI
            !IF (NPRINTLEV>0) WRITE(*,*) 'ZCOEF | ZCOFSWI ', ZCOEF(JI,JP,JL), ' | ', ZCOFSWI(JI)
            ! impose limits  
            !ZHO(K1,L1) = MAX(-0.1, ZHO(K1,L1))
            !ZHO(K1,L1) = MIN( 1.0, ZHO(K1,L1))
            ! innovation vector
            ZB2(K1) = XYO(JI,K1) - ZYF(1,JK)
            IF (S%XPATCH(JI,JP)>0.0 .AND. XF_PATCH(JI,JP,JL+1,JK).NE.XUNDEF .AND. XF_PATCH(JI,JP,1,JK).NE.XUNDEF) THEN
              S%XINNOV(JI,K1) = ZB2(K1)
            ENDIF
            IOBSCOUNT = IOBSCOUNT + 1
          ELSE  !if no obs available
            ! set obs operator and innovation to zero if no obs available
            ZHO(K1,L1) = 0.0
            ZB2(K1) = 0.0 
          ENDIF
          !
        ENDDO
      ENDDO
      !
    ENDDO
    !
  ENDDO
  !
  !S%XINNOV(JI,:) = ZB2(:)
  IF ( NPRINTLEV > 0 ) THEN
    WRITE(111,*) ZR(:,:)
    WRITE(112,*) ZB2(:)
  ENDIF
  
!---------------******  SOIL ANALYSIS *******--------------------------
  ZHOT(:,:) = 0.
  ZK1(:,:) = 0.
  ZP(:) = 0.
  ZX(:) = 0.
  !
  ZHOT(:,:) = TRANSPOSE(ZHO(:,:))
  ZK1 (:,:) = MATMUL(ZHO(:,:),MATMUL(ZB(JI,:,:),ZHOT(:,:))) + ZR(:,:)
  CALL CHOLDC(NOBSTYPE,ZK1(:,:),ZP(:))                         ! Cholesky decomposition (1)
  CALL CHOLSL(NOBSTYPE,ZK1(:,:),ZP(:),ZB2(:),ZX(:))            ! Cholesky decomposition (2)
  ZINCR(:) = MATMUL(ZB(JI,:,:),MATMUL(ZHOT(:,:),ZX(:)))
  DO JL=1,NVAR
    DO JP=1,INPATCH
      !
      IF ( S%XPATCH(JI,JP)>0.0 .AND. XF(JI,JP,1,JL).NE.XUNDEF ) THEN
        !
        L1 = JP+INPATCH*(JL-1)
        !
        ! Update the modified values
        IF ( TRIM(CVAR(JL))=="LAI" ) THEN
          ZINCR(L1) = MAX( ZINCR(L1), ZVLAIMIN(JP)-XF(JI,JP,1,JL) )
          ZINCR(L1) = MIN (MAX( ZINCR(L1), -1.), 1.)
          XBIO_PASS(JI,JP) = XBIO_PASS(JI,JP) + ZINCR(L1)*XALPH(JP)
        ELSEIF ( TRIM(CVAR(JL))=='WG1' .OR. TRIM(CVAR(JL))=='WG2' .OR. &
                 TRIM(CVAR(JL))=='WG3' .OR. TRIM(CVAR(JL))=='WG4' .OR. &
                 TRIM(CVAR(JL))=='WG5' .OR. TRIM(CVAR(JL))=='WG6' .OR. &
                 TRIM(CVAR(JL))=='WG7' .OR. TRIM(CVAR(JL))=='WG8') THEN
          IF ( TRIM(CVAR(JL))=='WG1' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,1)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG2' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,2)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG3' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,3)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG4' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,4)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG5' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,5)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG6' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,6)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG7' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,7)-XF(JI,JP,1,JL) )
          ELSEIF ( TRIM(CVAR(JL))=='WG8' ) THEN
            ZINCR(L1) = MIN( MAX( ZINCR(L1), XWGMIN-XF(JI,JP,1,JL) ), K%XWSAT(JI,8)-XF(JI,JP,1,JL) )
          ENDIF
          ZINCR(L1) = MIN( MAX( ZINCR(L1), -0.1), 0.1)
        ELSEIF ( XF(JI,JP,1,JL)+ZINCR(L1)<0. ) THEN
          ZINCR(L1) = 0.
        ENDIF
        !
        XF(JI,JP,1,JL) = XF(JI,JP,1,JL) + ZINCR(L1)
        !
        ! For no only warn if we have negative values.
        IF ( NPRINTLEV > 0 ) THEN
          IF ( XF(JI,JP,1,JL) < 0. ) WRITE(*,*) "WARNING X<0. for ",JI,JP," for variable ",TRIM(CVAR(JL))
        ENDIF
        !
      ENDIF
    ENDDO
  ENDDO
  !
  ! TRY TO COMPUTE ANALYSIS RESIDUAL
  DO ISTEP = 1,NBOUTPUT
    !
    DO JK = 1,NOBSTYPE
      !
      K1  = (ISTEP-1)*NOBSTYPE + JK
      ZB3 = XUNDEF
      !
      DO JL = 1,NVAR
        IF ( ( TRIM(CVAR(JL))=="WG1" .AND. TRIM(COBS(JK)) == "WG1" ) .OR. &
             ( TRIM(CVAR(JL))=="WG2" .AND. TRIM(COBS(JK)) == "WG2" ) .OR. &
             ( TRIM(CVAR(JL))=="LAI" .AND. TRIM(COBS(JK)) == "LAI" ) ) THEN
          IF( (XYO(JI,K1).NE.XUNDEF) .AND. (XYO(JI,K1).NE.999.0) ) THEN
            ZB4  = 0.
            DO JP = 1,INPATCH ! all patches
              IF ( S%XPATCH(JI,JP)>0.0 .AND.  XF(JI,JP,1,JL) .NE.XUNDEF) THEN
                ZB4 = ZB4 + XF(JI,JP,1,JL) * S%XPATCH(JI,JP)
              ENDIF
            ENDDO
            ZB3 = XYO(JI,K1)-  ZB4
          ENDIF
        ENDIF
      ENDDO
      !
      S%XRESID(JI,K1) = ZB3
      !
    ENDDO
    !
  ENDDO
  !
  DO JL=1,NVAR
    DO JP = 1,INPATCH
      PK => NP%AL(JP)
      !
      DO JJ = 1,PK%NSIZE_P
        IF ( PK%NR_P(JJ)==JI ) THEN
          PK%XHO(JJ,:,JL) = ZHOWR(:,JP+INPATCH*(JL-1))
          PK%XINCR(JJ,JL) = ZINCR(JP+INPATCH*(JL-1))
        ENDIF
      ENDDO
      !
    ENDDO
  ENDDO
  !
  IF ( NPRINTLEV > 0 ) THEN
    DO JP=1,INPATCH
      WRITE(113,*) (XF(JI,JP,1,JL),JL=1,NVAR), (ZINCR(JP+INPATCH*(JL-1)),JL=1,NVAR)
    ENDDO
  ENDIF
  !
!--------------------ANALYSIS OF B (FOR USE IN NEXT CYCLE)-------------------
  ! Ba = (I-KH)Bf(I-KH)t+KRKt
  ! K = BfHt{K1}**-1
  ! K1 needs PATCH dim added
  
  ZGAIN(:,:) = 0.
  ZIDKH(:,:) = 0.
  ZKRK(:,:) = 0.
  !
  ! K1 = (R+H.B.HT) (calculate inverse -> output goes to K1)
  CALL INVERSE_MATRIX(NOBS,ZK1(:,:),ZP(:))
  ZGAIN(:,:) = MATMUL(ZB(JI,:,:),MATMUL(ZHOT(:,:),ZK1(:,:)))
  ZIDKH(:,:) = ZIDENT(:,:) - MATMUL(ZGAIN(:,:),ZHO(:,:))
  ZKRK (:,:) = MATMUL(ZGAIN(:,:),MATMUL(ZR(:,:),TRANSPOSE(ZGAIN(:,:))))
  IF (.NOT.LBFIXED)  ZB(JI,:,:) = MATMUL(ZIDKH(:,:),MATMUL(ZB(JI,:,:),TRANSPOSE(ZIDKH(:,:)))) + ZKRK(:,:)
  
  IF ( NPRINTLEV > 0 ) THEN
    IUNIT = 150
    DO JL = 1,NVAR
      DO JK = 1,NOBSTYPE
        IUNIT = IUNIT + 1
        DO JP=1,INPATCH
          WRITE(IUNIT,*) ZHOWR(JK,JP+INPATCH*(JL-1)),ZGAIN(JP+INPATCH*(JL-1),JK)
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  !  
ENDDO
!
!
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
IF ( NPRINTLEV > 0 ) THEN
  CLOSE(111)
  CLOSE(112)
  CLOSE(113)
  IUNIT = 150
  DO JL = 1,NVAR
    DO JK = 1,NOBSTYPE
      IUNIT = IUNIT + 1
      CLOSE(IUNIT)
    ENDDO
  ENDDO
ENDIF
!//////////////////////TO WRITE ANALYSIS ARRAYS/////////////////////////////////////
!
IF (LBEV .OR. NPRINTLEV>1) THEN
  ! Write out analysed B (for use in next cycle)
  YBGFILE = "BGROUNDout_ASSIM."//TRIM(YMYPROC)
  CALL B_BIG_LOOP(IO,"WRIT",YBGFILE,ZB)
ENDIF
!
IF ( NPRINTLEV > 0 ) THEN
  IOBSCOUNT = IOBSCOUNT / INPATCH / NVAR
  IF (NRANK==NPIO) THEN
    WRITE(*,*)
    WRITE(*,*) '   ---------------------------------------'
    WRITE(*,*) '   |   EXITING VARASSIM AFTER ANALYSIS   |'
    WRITE(*,*) '   ---------------------------------------'
    WRITE(*,*)
  ENDIF
  WRITE(*,*) 'Number of assimilated observations =',IOBSCOUNT
  WRITE(*,*)
ENDIF
!
!############################# GET VARIABLES FOR OUTPUT WRITING ###############################
DO JL=1,NVAR
  DO JP = 1,INPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    !
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      !
      ! Update the modified values
      SELECT CASE (TRIM(CVAR(JL)))
        CASE("TG1")
          PEK%XTG(JI,1) = XF(IMASK,JP,1,JL)
        CASE("TG2")
          PEK%XTG(JI,2) = XF(IMASK,JP,1,JL)
        CASE("WG1")
          PEK%XWG(JI,1) = XF(IMASK,JP,1,JL)
        CASE("WG2")
          PEK%XWG(JI,2) = XF(IMASK,JP,1,JL)
        CASE("WG3")
          PEK%XWG(JI,3) = XF(IMASK,JP,1,JL)
        CASE("WG4")
          PEK%XWG(JI,4) = XF(IMASK,JP,1,JL)
        CASE("WG5")
          PEK%XWG(JI,5) = XF(IMASK,JP,1,JL)
        CASE("WG6")
          PEK%XWG(JI,6) = XF(IMASK,JP,1,JL)
        CASE("WG7")
          PEK%XWG(JI,7) = XF(IMASK,JP,1,JL)
        CASE("WG8")
          PEK%XWG(JI,8) = XF(IMASK,JP,1,JL)
        CASE("LAI") 
          PEK%XLAI(JI) = XF(IMASK,JP,1,JL)
          SELECT CASE (TRIM(CBIO))
            CASE("BIOMA1","BIOMASS1")
              PEK%XBIOMASS(JI,1) = XBIO_PASS(IMASK,JP)
            CASE("BIOMA2","BIOMASS2")
              PEK%XBIOMASS(JI,2) = XBIO_PASS(IMASK,JP)
            CASE("RESPI1","RESP_BIOM1")
              PEK%XRESP_BIOMASS(JI,1) = XBIO_PASS(IMASK,JP)
            CASE("RESPI2","RESP_BIOM2")
              PEK%XRESP_BIOMASS(JI,2) = XBIO_PASS(IMASK,JP)
            CASE("LAI")
              PEK%XLAI(JI) = XBIO_PASS(IMASK,JP)
            CASE DEFAULT
              CALL ABOR1_SFX("Mapping of "//CBIO//" is not defined in EKF!")
          END SELECT
        CASE DEFAULT
          CALL ABOR1_SFX("Mapping of "//TRIM(CVAR(JL))//" is not defined in EKF!")
      END SELECT
    ENDDO
  ENDDO
ENDDO
!
#endif
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_EKF',1,ZHOOK_HANDLE)
!
END SUBROUTINE ASSIM_NATURE_ISBA_EKF
