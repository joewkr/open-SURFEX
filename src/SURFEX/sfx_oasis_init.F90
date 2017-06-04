!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_OASIS_INIT(HNAMELIST,KLOCAL_COMM,HINIT)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Initialize coupled mode communication and XIOS I/O scheme
!!
!!
!!     METHOD
!!     ------
!!
!!     Depending on namelist flags for Oasis and XIOS, either call :
!!        - XIOS_INITIALIZE alone (when LXIOS and not LOASIS) , or
!!        - OASIS_INIT_COMP (when LOASIS) and then, depending on LXIOS, 
!!           * either XIOS_INITALIZE  
!!           * or OASIS_GET_LOCAL_COMM 
!!
!!     Note : OASIS-MCT interface must be initialized before any DR_HOOK call
!!
!!     EXTERNAL
!!     --------
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     S. Valcke et al., 2013: OASIS-MCT User Guide 
!!     CERFACS, Toulouse, France, 50 pp.
!!     https://verc.enes.org/oasis/oasis-dedicated-user-support-1/documentation/oasis3-mct-user-guide
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> ; cd <dir>/doc ; ....
!!
!!
!!     AUTHOR
!!     ------
!!
!!     B. Decharme, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    10/2013
!!     S.Sénési    08/2015 - handle XIOS
!!     B.Decharme  09/2016 - no CALL ABORT if no namelist in Arpege
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_OASIS, ONLY : LOASIS, CMODEL_NAME, XRUNTIME
USE MODI_ABOR1_SFX
!
USE MODD_XIOS , ONLY : LXIOS ! Should we call XIOS_INITIALIZE instead of OASIS_GET_LOCAL_COMM
!
#ifdef WXIOS
USE XIOS, ONLY : XIOS_INITIALIZE
#endif
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
IMPLICIT NONE
!
#ifdef CPLOASIS
INCLUDE 'mpif.h'
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN )           :: HNAMELIST
INTEGER,           INTENT(OUT)           :: KLOCAL_COMM ! value of local communicator
 CHARACTER(LEN=3),  INTENT(IN ), OPTIONAL :: HINIT       ! choice of fields to initialize
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=9)   :: YWORD, YTIMERUN
 CHARACTER(LEN=1000):: YLINE, YFOUND
INTEGER            :: IERR, IWORK, IRANK
INTEGER            :: ICOMP_ID
INTEGER            :: ITIMERUN
LOGICAL            :: GFOUND
 CHARACTER(LEN=3)   :: YINIT
!
!
!*       0.3   Declarations of namelist variables
!              ----------------------------------
!
NAMELIST/NAM_OASIS/LOASIS,CMODEL_NAME
!
!-------------------------------------------------------------------------------
!
! ATTENTION : Do not introduce DR_HOOK in this routine
!
!*       0.     Initialization:
!               ---------------
!
LOASIS      = .FALSE.
 CMODEL_NAME = 'surfex'
XRUNTIME    = 0.0
!
YINIT = 'ALL'
IF(PRESENT(HINIT))YINIT=HINIT
!
!-------------------------------------------------------------------------------
!
!*       1.     Read namelist:
!               --------------
!
IF(LEN_TRIM(HNAMELIST)/=0)THEN
!
  OPEN(UNIT=11,FILE=HNAMELIST,ACTION='READ',FORM="FORMATTED",POSITION="REWIND",STATUS='OLD',IOSTAT=IERR)   
!
  IF (IERR /= 0) THEN
    WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    WRITE(*,'(A)' )' WARNING WARNING WARNING WARNING WARNING     '
    WRITE(*,'(A)' )' ---------------------------------------     '
    WRITE(*,'(2A)')'SFX_OASIS_INIT: SFX NAMELIST FILE NOT FOUND: ',TRIM(HNAMELIST)
    WRITE(*,'(A)' )'-------------------------------------------  '     
    WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
#ifndef SFX_ARO 
    CALL ABORT
    STOP
#endif
  ELSE
    READ (UNIT=11,NML=NAM_OASIS,IOSTAT=IERR)
    CLOSE(UNIT=11)
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Setup OASIS (possibly via XIOS) and XIOS
!               ----------------------------------------
!
IF (LXIOS) THEN
!
!
#ifdef WXIOS
   ! NOTE : XIOS_INITIALIZE will call OASIS_INIT_COMP and 
   ! OASIS_GET_LOCALCOMM if its own config file calls for Oasis
!$OMP SINGLE
  CALL XIOS_INITIALIZE(CMODEL_NAME, return_comm=KLOCAL_COMM)
!$OMP END SINGLE
!
#else
!
   WRITE(*,*) 'SFX_OASIS_INIT : BINARY WAS NOT COMPILED WITH XIOS SUPPORT '
   CALL ABOR1_SFX('SFX_OASIS_INIT : BINARY WAS NOT COMPILED WITH XIOS SUPPORT')
!
#endif
!
!
ELSE  ! (i.e. .NOT. LXIOS)
!
#ifdef CPLOASIS

  IF (LOASIS ) THEN
    IRANK=0
    CALL OASIS_INIT_COMP(ICOMP_ID,CMODEL_NAME,IERR)  
    IF (IERR/=OASIS_OK) THEN
      WRITE(*,'(A)'   )'SFX : Error initializing OASIS'
      WRITE(*,'(A,I4)')'SFX : Return code from oasis_init_comp : ',IERR
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'SFX_OASIS_INIT: Error initializing OASIS')
      CALL ABORT
      STOP
    ENDIF
    CALL OASIS_GET_LOCALCOMM(KLOCAL_COMM,IERR) 
    IF (IERR/=OASIS_OK) THEN
      IF(IRANK==0)THEN
        WRITE(*,'(A)'   )'SFX : Error getting local communicator from OASIS'
        WRITE(*,'(A,I4)')'SFX : Return code from oasis_get_local_comm : ',IERR
      ENDIF
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'SFX_OASIS_INIT: Error getting local communicator')
      CALL ABORT
      STOP
    ENDIF
!
  ELSE
    KLOCAL_COMM=0
    RETURN
  ENDIF

#else

  KLOCAL_COMM=0
  RETURN

#endif
!
ENDIF
!
#ifdef SFX_MPI
CALL MPI_COMM_RANK(KLOCAL_COMM,IRANK,IWORK)
#endif
IF(IRANK==0)THEN
   WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   IF (LOASIS) WRITE(*,'(A)')'OASIS used for model : '//TRIM(CMODEL_NAME)
   IF (LXIOS)  WRITE(*,'(A)')'XIOS  used for model : '//TRIM(CMODEL_NAME)
   WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
ENDIF
!
IF(YINIT=='PRE')THEN
   RETURN
ENDIF

#ifdef CPLOASIS
IF (LOASIS) THEN
!
!-------------------------------------------------------------------------------
!
!*       5.     Read total simulated time in namcouple
!               --------------------------------------
!
 OPEN (UNIT=11,FILE ='namcouple',STATUS='OLD',FORM ='FORMATTED',POSITION="REWIND",IOSTAT=IERR)
 IF (IERR /= 0) THEN
    IF(IRANK==0)THEN
       WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       WRITE(*,'(A)'   )'SFX : OASIS namcouple not found'
       WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    ENDIF
    CALL ABORT
    STOP
 ENDIF
!
 YTIMERUN=' $RUNTIME'
 ITIMERUN=-1
!
 DO WHILE (ITIMERUN==-1)
    READ (UNIT = 11,FMT = '(A9)',IOSTAT=IERR) YWORD
    IF(IERR/=0)THEN
       IF(IRANK==0)THEN
          WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(*,'(A)'   )'SFX : Problem $RUNTIME empty in namcouple'
          WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
       ENDIF
       CALL ABORT
       STOP           
    ENDIF
    IF (YWORD==YTIMERUN)THEN
       READ (UNIT = 11,FMT = '(A1000)',IOSTAT=IERR) YLINE
       IF(IERR/=0)THEN
          IF(IRANK==0)THEN
             WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
             WRITE(*,'(A)'   )'SFX : Problem looking for $RUNTIME in namcouple'
             WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
          ENDIF
          CALL ABORT
          STOP           
       ENDIF
       CALL FOUND_TIMERUN (YLINE, YFOUND, 1000, GFOUND)
       IF (GFOUND) THEN
          READ (YFOUND,FMT = '(I100)',IOSTAT=IERR) ITIMERUN
          IF(IERR/=0)THEN
             IF(IRANK==0)THEN
                WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                WRITE(*,'(A)'   )'SFX : Problem reading $RUNTIME in namcouple'
                WRITE(*,'(2A)'  )'$RUNTIME = ', TRIM(YFOUND)
                WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
             ENDIF
             CALL ABORT
             STOP
          ENDIF
       ENDIF
    ENDIF
 ENDDO
 CLOSE(11)
!
 XRUNTIME = REAL(ITIMERUN)
!
ENDIF
#endif
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE FOUND_TIMERUN(HIN, HOUT, KLEN, OFOUND)
!
IMPLICIT NONE
!
INTEGER ,          INTENT (IN   ) :: KLEN
 CHARACTER (LEN=*), INTENT (INOUT) :: HIN 
 CHARACTER (LEN=*), INTENT (INOUT) :: HOUT
LOGICAL,           INTENT (OUT  ) :: OFOUND
!
!* ---------------------------- Local declarations -------------------
!
 CHARACTER(LEN=1), PARAMETER :: YBLANK = ' '
 CHARACTER(LEN=1), PARAMETER :: YNADA  = '#'

 CHARACTER(LEN=KLEN) :: YLINE
 CHARACTER(LEN=KLEN) :: YWORK
!
INTEGER             :: ILEN
INTEGER             :: IERR
!
!
!*    1. Skip line if it is a comment
!        ----------------------------
!
DO WHILE (HIN(1:1)==YNADA)
   READ (UNIT = 11, FMT = '(A9)',IOSTAT=IERR) YLINE 
   IF(IERR/=0)THEN
       IF(IRANK==0)THEN
         WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         WRITE(*,'(A)'   )'SFX : Problem looking for $RUNTINE line in namcouple'
         WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
       ENDIF
       CALL ABORT
       STOP           
   ENDIF
   HIN(1:KLEN) = YLINE(1:KLEN)
ENDDO 
!
!* Fill HOUT with blanks
!
HOUT = YBLANK
!
!* Fill temporary string and remove leading blanks
!
YWORK = ADJUSTL(HIN)
!
IF(LEN_TRIM(YWORK)<=0)THEN
   OFOUND = .FALSE.
   RETURN
ENDIF
!
!* Find the length of this set of characters
!
ILEN = INDEX(YWORK,YBLANK) - 1
!
!* Copy to HOUT
!
HOUT(1:ILEN) = YWORK(1:ILEN)
!
OFOUND = .TRUE.
!
END SUBROUTINE FOUND_TIMERUN
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_INIT
