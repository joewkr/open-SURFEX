!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!------------------------
PROGRAM PREP
!------------------------
!!
!!    PURPOSE
!!    -------
!!   This program prepares the initial file for offline run
!!
!!    METHOD
!!    ------
!!   
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. LeMoigne                  Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     22/04/04
!!
!----------------------------------------------------------------------------
!
USE MODE_POS_SURF
!
USE MODD_SURFEX_MPI, ONLY : NCOMM, NPROC, NRANK, NPIO, WLOG_MPI, PREP_LOG_MPI,   &
                            END_LOG_MPI, NNUM, NINDEX, NSIZE_TASK
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODN_IO_OFFLINE
!
USE MODD_IO_SURF_ASC
USE MODD_IO_SURF_FA
USE MODD_IO_SURF_LFI
USE MODD_IO_SURF_NC
USE MODD_SURF_PAR
USE MODD_SURF_CONF, ONLY : CSOFTWARE
!
USE MODD_WRITE_SURF_ATM, ONLY : LFIRST_WRITE,  NCPT_WRITE
!
USE MODD_SFX_OASIS, ONLY : LOASIS
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_ALL_NAMELISTS
USE MODI_GET_LUOUT
!
USE MODI_INIT_PGD_SURF_ATM
USE MODI_IO_BUFF_CLEAN
USE MODI_PREP_SURF_ATM
USE MODI_WRITE_DIAG_SURF_ATM_n
USE MODI_WRITE_HEADER_MNH
USE MODI_WRITE_SURF_ATM_n
!
USE MODI_GET_LONLAT_n
USE MODI_FLAG_UPDATE
USE MODI_ABOR1_SFX
!
USE MODI_SFX_OASIS_INIT
USE MODI_SFX_OASIS_READ_NAM
USE MODI_SFX_OASIS_PREP_OL
USE MODI_SFX_OASIS_END
!
USE MODI_INIT_OUTPUT_NC_n
USE MODI_INIT_INDEX_MPI
!
!------------------------------------------------------------------------------
!
#ifdef SFX_MPL
USE MPL_DATA_MODULE, ONLY : LMPLUSERCOMM, MPLUSERCOMM
#endif
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_OFF_SURFEX_n
USE MODE_MODELN_SURFEX_HANDLER
!
IMPLICIT NONE
!
#ifndef AIX64
!$ INCLUDE 'omp_lib.h'
#endif
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
!*    0.     Declaration of local variables
!            ------------------------------
!
INTEGER            :: ILUOUT
INTEGER            :: ILUNAM
INTEGER            :: IYEAR, IMONTH, IDAY
REAL               :: ZTIME
LOGICAL            :: GFOUND

REAL, DIMENSION(0) :: ZZS
CHARACTER(LEN=28)  :: YATMFILE  ='                            '  ! name of the Atmospheric file
CHARACTER(LEN=6)   :: YATMFILETYPE ='      '                     ! type of the Atmospheric file
CHARACTER(LEN=28)  :: YPGDFILE  ='                            '  ! name of the pgd file
CHARACTER(LEN=6)   :: YPGDFILETYPE ='      '                     ! type of the pgd file
CHARACTER(LEN=28)  :: YLUOUT    ='LISTING_PREP                '  ! name of listing
 CHARACTER(LEN=100) :: YNAME
!
INTEGER, DIMENSION(11)  :: IDATEF
!
TYPE (PREP_CTL) :: YLCTL
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
INTEGER :: ILEVEL, INFOMPI
INTEGER :: JNW, INW
INTEGER :: IRET, INB, JPROC
DOUBLE PRECISION :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!
!*    1.      Set default names and parallelized I/O
!             --------------------------------------
!
!Must be call before DRHOOK !
CALL SFX_OASIS_INIT(CNAMELIST,NCOMM,'PRE')
#ifdef SFX_MPL
IF(LOASIS)THEN
  LMPLUSERCOMM = .TRUE.
  MPLUSERCOMM = NCOMM
ENDIF
#endif
!
#ifdef SFX_MPI
IF(.NOT.LOASIS)THEN
  CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE,ILEVEL,INFOMPI)
  IF (INFOMPI /= MPI_SUCCESS) THEN 
    CALL ABOR1_SFX('OFFLINE: ERROR WHEN INITIALIZING MPI')
  ENDIF
  NCOMM = MPI_COMM_WORLD
ENDIF
 CALL MPI_COMM_SIZE(NCOMM,NPROC,INFOMPI)
 CALL MPI_COMM_RANK(NCOMM,NRANK,INFOMPI)
#endif
!
IF (LHOOK) CALL DR_HOOK('PREP',0,ZHOOK_HANDLE)
!
!    Allocations of Surfex Types
 CALL SURFEX_ALLOC_LIST(1)
!
CSOFTWARE='PREP'
!
!     1.1     initializations
!             ---------------
!
IYEAR    = NUNDEF
IMONTH   = NUNDEF
IDAY     = NUNDEF
ZTIME    = XUNDEF
!
LPREP    = .TRUE.
!
!     1.2     output listing
!             --------------
!
IF (NRANK>=10) THEN
  WRITE(YNAME,FMT='(A15,I2)') TRIM(YLUOUT),NRANK
ELSE
  WRITE(YNAME,FMT='(A15,I1)') TRIM(YLUOUT),NRANK
ENDIF
!
CLUOUT_LFI =  ADJUSTL(ADJUSTR(YNAME)//'.txt')
CLUOUT_NC  =  ADJUSTL(ADJUSTR(YNAME)//'.txt')
CALL GET_LUOUT('ASCII ',ILUOUT)
OPEN (UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YNAME)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
!     1.3     output file name read in namelist
!             ---------------------------------
!
 CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
!
 CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
!
 CFILEPGD     = ADJUSTL(ADJUSTR(CPGDFILE)//'.txt')
 CFILEPGD_FA  = ADJUSTL(ADJUSTR(CPGDFILE)//'.fa')
 CFILEPGD_LFI = CPGDFILE
 CFILEPGD_NC  = ADJUSTL(ADJUSTR(CPGDFILE)//'.nc')
 !
 CFILEIN     = ADJUSTL(ADJUSTR(CPGDFILE)//'.txt')      ! output of PGD program
 CFILEIN_FA  = ADJUSTL(ADJUSTR(CPGDFILE)//'.fa')
 CFILEIN_LFI = CPGDFILE
 CFILEIN_NC  = ADJUSTL(ADJUSTR(CPGDFILE)//'.nc')
 !
 CFILEIN_SAVE     = CFILEIN
 CFILEIN_LFI_SAVE = CFILEIN_LFI
 CFILEIN_FA_SAVE  = CFILEIN_FA
 CFILEIN_NC_SAVE  = CFILEIN_NC
 !
 CFILEOUT    = ADJUSTL(ADJUSTR(CPREPFILE)//'.txt')
 CFILEOUT_FA = ADJUSTL(ADJUSTR(CPREPFILE)//'.fa')
 CFILEOUT_LFI= CPREPFILE
 CFILEOUT_NC = ADJUSTL(ADJUSTR(CPREPFILE)//'.nc')
 !
CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
! Reading all namelist (also assimilation)
 YSC => YSURF_LIST(1)
 CALL READ_ALL_NAMELISTS(YSC, CSURF_FILETYPE,'PRE',.FALSE.)
!
!*      1.4.   Reads SFX - OASIS coupling namelists
!              ------------------------------------
!
CALL SFX_OASIS_READ_NAM(CSURF_FILETYPE,XTSTEP_SURF,'PRE')
!
!*      1.5.   Goto model of Surfex Types
!              ---------------------------
!
  ICURRENT_MODEL = 1
!
!*    2.      Preparation of surface physiographic fields
!             -------------------------------------------
!
!$OMP PARALLEL
!$ NBLOCKTOT = OMP_GET_NUM_THREADS()
!$OMP END PARALLEL
!
 CALL PREP_LOG_MPI
!
 CALL WLOG_MPI(' ')
!
 CALL WLOG_MPI('NBLOCKTOT ',KLOG=NBLOCKTOT)
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
CALL INIT_INDEX_MPI(YSC%DTCO,YSC%U,YSC%UG,YSC%GCP,CSURF_FILETYPE,'PRE',YALG_MPI,XIO_FRAC)
!
 CALL IO_BUFF_CLEAN
 CALL INIT_PGD_SURF_ATM(YSC, CSURF_FILETYPE,'PRE',YATMFILE,YATMFILETYPE, &
                        IYEAR, IMONTH, IDAY, ZTIME) 
!
 CALL IO_BUFF_CLEAN
 CALL PREP_SURF_ATM(YSC,CSURF_FILETYPE,YATMFILE,YATMFILETYPE,YPGDFILE,YPGDFILETYPE,YLCTL)
!
!*    3.      Preparation of SFX-OASIS grid, mask, areas files
!             ------------------------------------------------
!
IF(LOASIS)THEN
  CALL SFX_OASIS_PREP_OL(YSC%IM%O, YSC%IM%S, YSC%UG, YSC%U, CSURF_FILETYPE,YALG_MPI)
ENDIF
!
!*    4.      Store of surface physiographic fields
!             -------------------------------------
!
CALL FLAG_UPDATE(YSC%IM%ID%O, YSC%DUO, .FALSE.,.TRUE.,.FALSE.,.FALSE.)
!
!* opens the file
IF (NRANK==NPIO) THEN
  IF (CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA          
    LFANOCOMPACT = .TRUE.
    NUNIT_FA = 19
    IDATEF(1)=YSC%U%TTIME%TDATE%YEAR
    IDATEF(2)=YSC%U%TTIME%TDATE%MONTH
    IDATEF(3)=YSC%U%TTIME%TDATE%DAY
    IDATEF(4)=NINT(YSC%U%TTIME%TIME/3600.) 
    IDATEF(5)=NINT(YSC%U%TTIME%TIME/60.) - IDATEF(4) * 60 
    IDATEF(6)=1 
    IDATEF(7:11)=0  
    CALL FAITOU(IRET,NUNIT_FA,.TRUE.,CFILEOUT_FA,'NEW',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
    CALL FANDAR(IRET,NUNIT_FA,IDATEF)
#endif    
  ENDIF
END IF
!
ALLOCATE(YSC%DUO%CSELECT(0))
!
LDEF = .TRUE.
!
IF (CSURF_FILETYPE=="NC    ") THEN
  CALL INIT_OUTPUT_NC_n (YSC%TM%BDD, YSC%CHE, YSC%CHN, YSC%CHU,   &
                         YSC%SM%DTS, YSC%TM%DTT, YSC%DTZ, YSC%IM, &
                         YSC%UG, YSC%U, YSC%DUO%CSELECT)
ENDIF
!
INW = 1
IF (CSURF_FILETYPE=="NC    ") INW = 2
!
LFIRST_WRITE = .TRUE.
NCPT_WRITE = 0
!
DO JNW = 1,INW
  !
  IF (LWRITE_COORD) CALL GET_LONLAT_n(YSC%DTCO, YSC%U, YSC%UG, YSC%DUO%CSELECT, CSURF_FILETYPE)
  !
  !* writes into the file
  CALL IO_BUFF_CLEAN
  !
  ! FLAG_UPDATE now in WRITE_PGD_SURF_ATM_n
  CALL WRITE_SURF_ATM_n(YSC, CSURF_FILETYPE,'PRE',LLAND_USE) !no pgd field
  CALL WRITE_DIAG_SURF_ATM_n(YSC, CSURF_FILETYPE,'ALL')
  !
  LDEF = .FALSE.
  LFIRST_WRITE = .FALSE.
  NCPT_WRITE = 0
  CALL IO_BUFF_CLEAN  
  !
ENDDO
!
!* closes the file
IF (NRANK==NPIO) THEN
  IF (CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA          
    CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
#endif    
  END IF
  !
  !* add informations in the file
  IF (CSURF_FILETYPE=='LFI   ' .AND. LMNH_COMPATIBLE) CALL WRITE_HEADER_MNH
  !
  !
  !*    4.     Close parallelized I/O
  !            ----------------------
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '    -----------------------'
  WRITE(ILUOUT,*) '    | PREP ENDS CORRECTLY |'
  WRITE(ILUOUT,*) '    -----------------------'
  !
  WRITE(*,*) ' '
  WRITE(*,*) '    -----------------------'
  WRITE(*,*) '    | PREP ENDS CORRECTLY |'
  WRITE(*,*) '    -----------------------'
  !
  CLOSE(ILUOUT)
  !
ENDIF
!
 CALL SURFEX_DEALLO_LIST
!
IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
IF (ALLOCATED(NNUM)) DEALLOCATE(NNUM)
IF (ALLOCATED(NSIZE_TASK)) DEALLOCATE(NSIZE_TASK)
!
 CALL END_LOG_MPI
!
IF (LHOOK) CALL DR_HOOK('PREP',1,ZHOOK_HANDLE)
!
! * OASIS must be finalized after the last DR_HOOK call
!
IF(LOASIS)THEN
  CALL SFX_OASIS_END
ELSE
#ifdef SFX_MPI
  CALL MPI_FINALIZE(INFOMPI)
#endif
ENDIF
!
!-------------------------------------------------------------------------------
!
END PROGRAM PREP
