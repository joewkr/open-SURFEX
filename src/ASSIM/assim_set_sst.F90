!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_SET_SST (DTCO, S, U, KI, PITM, PSST, PSIC, HTEST)

!     ###############################################################################
!
!!****  *ASSIM_SET_SST * - Reads SST from file
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     T. Aspelien
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!--------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC
!
USE MODD_ASSIM,         ONLY : LECSST, LREAD_SST_FROM_FILE, CFILE_FORMAT_SST, NPRINTLEV
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
#ifdef SFX_FA
USE MODD_IO_SURF_FA,    ONLY : CFILEIN_FA, CDNOMC
#endif
!
USE MODI_ABOR1_SFX
USE MODI_INIT_IO_SURF_n
USE MODI_READ_SURF
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_END_IO_SURF_n
USE MODI_IO_BUFF_CLEAN
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK,            ONLY : LHOOK,DR_HOOK
USE PARKIND1,           ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER,            INTENT(IN)  :: KI
REAL,DIMENSION(KI), INTENT(IN)  :: PITM
REAL,DIMENSION(KI), INTENT(OUT) :: PSST
REAL,DIMENSION(KI), INTENT(OUT) :: PSIC  ! Not used at the moment
CHARACTER(LEN=2),   INTENT(IN)  :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL,ALLOCATABLE, DIMENSION(:,:) :: ZWORK,ZWORK2
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSEA
CHARACTER(LEN=200)   :: YMFILE     ! Name of the SST file
CHARACTER(LEN=6)     :: YPROGRAM2 = 'FA    '
REAL, DIMENSION(SIZE(PSST)) :: ZSST
REAL                 :: ZFMAX, ZFMIN, ZFMEAN
INTEGER              :: IRESP,ISTAT
INTEGER              :: JI,JJ,ICPT
REAL(KIND=JPRB)      :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_SET_SST',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_SET_SST: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF (U%CSEA=="NONE" .OR. U%NDIM_SEA == 0) THEN
  IF (LHOOK) CALL DR_HOOK('ASSIM_SET_SST_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
PSIC(:) = 0.
!
IF ( LREAD_SST_FROM_FILE ) THEN
  !
  IF ( TRIM(CFILE_FORMAT_SST) == "ASCII" ) THEN
    !
    ALLOCATE(ZSEA  (U%NDIM_FULL))
    ALLOCATE(ZWORK (U%NDIM_FULL,2))
    ALLOCATE(ZWORK2(U%NSIZE_FULL,2))
    !
    IF (NPROC>1) CALL GATHER_AND_WRITE_MPI(U%XSEA,ZSEA)
    !
    IF (NRANK==NPIO) THEN
      YMFILE = 'SST_SIC'
      IF (NPRINTLEV > 0 .AND. NRANK==NPIO ) &
              WRITE(*,*) "READING SST/SIC from file "//TRIM(YMFILE)//".DAT for ",&
                                U%NDIM_SEA," sea points",U%NDIM_FULL
      ISTAT = 0
      OPEN(UNIT=55,FILE=TRIM(YMFILE)//".DAT",FORM='FORMATTED',STATUS='OLD',IOSTAT=ISTAT)
      IF ( ISTAT /= 0 ) CALL ABOR1_SFX("Can not open "//TRIM(YMFILE))

      ZWORK(:,:) = XUNDEF
      ! Read SST/SIC values
      DO JI = 1,U%NDIM_FULL
        IF ( ZSEA(JI) > 0. ) THEN
          READ (55,*,IOSTAT=ISTAT)  (ZWORK(JI,JJ),JJ=1,2)
          IF ( ISTAT /= 0 ) CALL ABOR1_SFX("Error reading file "//TRIM(YMFILE))
        ENDIF
      ENDDO
      CLOSE(55)
    ENDIF

    ! Distribute ZWORK to all processors
    IF (NPROC>1) THEN
      CALL READ_AND_SEND_MPI(ZWORK(:,1),ZWORK2(:,1))
      CALL READ_AND_SEND_MPI(ZWORK(:,2),ZWORK2(:,2))
    ELSE
      ZWORK2=ZWORK
    ENDIF

    ! Set SST/SIC variables
    DO JI = 1,U%NSIZE_FULL
      PSST(JI)=ZWORK2(JI,1)
      PSIC(JI)=ZWORK2(JI,2)
    ENDDO

    DEALLOCATE(ZWORK)
    DEALLOCATE(ZWORK2)
    DEALLOCATE(ZSEA)

  ELSEIF ( TRIM(CFILE_FORMAT_SST) == "FA" ) THEN
    !
    !  Read SST from boundaries when SST analysis NOT is performed in CANARI
    !
    !  Define FA file name for SST analysis interpolated from boundary file 
    !
#ifdef SFX_FA
    CFILEIN_FA = 'SST_SIC'        ! input SST and SIC analysis  
    CDNOMC     = 'CADRE SST'      ! new frame name 
    IF (NRANK==NPIO .AND. NPRINTLEV>0) WRITE(*,*) 'READING SST FROM ',TRIM(CFILEIN_FA)
#endif
    !
    !  Open FA file
    !
    CALL INIT_IO_SURF_n(DTCO, U, YPROGRAM2,'EXTZON','SURF  ','READ ')
    !
    !  Read SST_SIC 
    !
    IF ( LECSST ) THEN
      ! SST field interpolated from ECMWF SST ANALYSIS to model domain
      CALL READ_SURF(YPROGRAM2,'SURFSEA.TEMPERA',PSST,IRESP)
    ELSE
      ! Surface temperature from boundary in SST_SIC
      CALL READ_SURF(YPROGRAM2,'SURFTEMPERATURE',PSST,IRESP)
    ENDIF
    !
    !  Close SST_SIC file
    !
    CALL END_IO_SURF_n(YPROGRAM2)
    CALL IO_BUFF_CLEAN
    IF (NRANK==NPIO) WRITE(*,*) 'READ SST_SIC OK'

    ZFMIN = MINVAL(PSST)
    ZFMAX = MAXVAL(PSST)
    IF ( KI > 0 ) THEN
      ZFMEAN = SUM(PSST)/FLOAT(KI)
    ELSE
      ZFMEAN=XUNDEF
    ENDIF

    IF ( LECSST ) THEN

      IF (NRANK==NPIO .AND. NPRINTLEV>0) THEN
        WRITE(*,*) '  ECMWF_SST_SIC'
        WRITE(*,'("  SURFSEA.TEMPERA - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
      ENDIF

      ! Replace -9999. with UNDEF
      WHERE ( PSST(:)< 0. )
        PSST(:) = XUNDEF
      ENDWHERE

    ELSE

      IF (NRANK==NPIO .AND. NPRINTLEV>0) THEN
        WRITE(*,*) '  Boundary file'
        WRITE(*,'("  SURFTEMPERATURE - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
      ENDIF
      ! To avoid surface temperatures influenced by land, NATURE points are replaced with UNDEF
      WHERE ( PITM(:)>0.5 )
        PSST(:) = XUNDEF
      ENDWHERE

    ENDIF

    ZFMIN = MINVAL(PSST)
    ZFMAX = MAXVAL(PSST)
    IF ( KI > 0 ) THEN
      ZFMEAN = SUM(PSST)/FLOAT(KI)
    ELSE
      ZFMEAN=XUNDEF
    ENDIF

    IF (NRANK==NPIO .AND. NPRINTLEV>0) THEN
      WRITE(*,*) '  Replaced land by UNDEF '
      WRITE(*,'("  SST            - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
    ENDIF

  ELSE
     CALL ABOR1_SFX("CFILE_FORMAT_SST="//TRIM(CFILE_FORMAT_SST)//" not implemented!")
  ENDIF

ELSE
  !
  IF ( U%NSIZE_SEA>0 .AND. U%CSEA/="NONE") THEN
    CALL UNPACK_SAME_RANK(U%NR_SEA,S%XSST,ZSST)
    PSST(:) = ZSST(:)
  ELSE
    PSST(:) = XUNDEF
  ENDIF
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ASSIM_SET_SST',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_SET_SST
