!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.

MODULE MODE_READ_SURF_COV
!
INTERFACE WRITE_READ_COV
  MODULE PROCEDURE READ_SURF_COV
END INTERFACE
!
CONTAINS

!     #############################################################
      SUBROUTINE READ_SURF_COV (HPROGRAM,HREC,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
USE MODD_SURFEX_MPI, ONLY : NPROC, NPIO, NRANK, IDX_R, NREQ, NCOMM, NSIZE, NINDEX
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI, ONLY : NMASK_lfi=>NMASK, NFULL_lfi=>NFULL
#endif
#ifdef SFX_NC
USE MODD_IO_SURF_NC, ONLY : NMASK_nc=>NMASK, NFULL_nc=>NFULL
#endif
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC, ONLY : NMASK_asc=>NMASK, NFULL_asc=>NFULL
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA, ONLY : NMASK_fa=>NMASK, NFULL_fa=>NFULL
#endif
#ifdef SFX_MNH
USE MODI_READ_SURFX2COV_MNH
#endif
!
USE MODI_READ_SURF
USE MODI_PACK_SAME_RANK
USE MODI_READ_AND_SEND_MPI
USE MODI_ABOR1_SFX
#ifdef SFX_ARO
USE MODI_READ_SURFX1_ARO
#endif
!
USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1 ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM    ! calling program
 CHARACTER(LEN=*), INTENT(IN) :: HREC        ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD ! array containing the data field
LOGICAL,DIMENSION(:), INTENT(IN) :: OFLAG   ! mask for array filling
INTEGER, INTENT(OUT) :: KRESP               ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: HCOMMENT ! name of the article to be read
 CHARACTER(LEN=1), OPTIONAL, INTENT(IN)  :: HDIR     ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE,NPROC-1) :: ISTATUS
#endif
!INTEGER, DIMENSION(NPROC) :: ITREQ
REAL, DIMENSION(:),ALLOCATABLE :: ZWORKR
REAL, DIMENSION(:),ALLOCATABLE :: ZFIELD
INTEGER, DIMENSION(:), POINTER :: IMASKF
INTEGER, DIMENSION(COUNT(OFLAG)) :: IMASK
 CHARACTER(LEN=100) :: YCOMMENT
 CHARACTER(LEN=12)  :: YREC
 CHARACTER(LEN=16)  :: YREC2
 CHARACTER(LEN=1)   :: YDIR
 CHARACTER(LEN=4)  :: YLVL
INTEGER :: IFLAG
INTEGER            :: IPIO_SAVE, IPAS, JP, IDEB, IFIN, JJ
INTEGER            :: JCOVER, JPROC, IPROC
INTEGER            :: IL1, IL2, IDX_SAVE, IDX, IVAL
INTEGER :: INFOMPI, IREQ, JPROC2, IFULL
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_1',0,ZHOOK_HANDLE)
!
IDX_SAVE = IDX_R
YREC = HREC
YCOMMENT="empty"
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IL1 = SIZE(PFIELD,1)
IL2 = COUNT(OFLAG)
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_1',1,ZHOOK_HANDLE)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
   YREC2 = YREC
   CALL READ_SURFX2COV_MNH(YREC2,IL1,IL2,PFIELD,OFLAG,KRESP,YCOMMENT,YDIR)
#endif
ELSE
  !
  IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_2',0,ZHOOK_HANDLE)
  !
  !mask associating the index of the cover in xcover to its real number
  JCOVER = 0
  DO JJ = 1,SIZE(OFLAG)
    IF (OFLAG(JJ)) THEN
      JCOVER=JCOVER+1
      IMASK(JCOVER) = JJ
    ENDIF
  ENDDO
  !
  !the mask to call read_and_send_mpi depends on the I/O type
  IF (HPROGRAM=='LFI   ') THEN
#ifdef SFX_LFI
    IFULL = NFULL_lfi
    ALLOCATE(ZFIELD(NFULL_lfi))
    IMASKF=>NMASK_lfi
#endif
  ELSEIF (HPROGRAM=='ASCII ') THEN
#ifdef SFX_ASC
    IFULL = NFULL_asc
    ALLOCATE(ZFIELD(NFULL_asc))
    IMASKF=>NMASK_asc
#endif
  ELSEIF (HPROGRAM=='FA     ') THEN
#ifdef SFX_FA
    IFULL = NFULL_fa
    ALLOCATE(ZFIELD(NFULL_fa))
    IMASKF=>NMASK_fa
#endif
  ELSEIF (HPROGRAM=='NC     ') THEN
#ifdef SFX_NC
    IFULL = NFULL_nc
    ALLOCATE(ZFIELD(NFULL_nc))
    IMASKF=>NMASK_nc
#endif
  ENDIF
  !
  !if we want to get covers for the current task or for the whole domain
  IF (YDIR=='H') THEN
     !second dimension because the reading of covers is parallelized with MPI
    ALLOCATE(ZWORKR(NSIZE))
  ELSEIF (NRANK==NPIO) THEN
    ALLOCATE(ZWORKR(IFULL))
  ELSE
    ALLOCATE(ZWORKR(0))
  ENDIF
  ZWORKR(:) = 0.
  !
  IF (NPROC>1 .AND. YDIR=='H') THEN
    IFLAG = 0
    !for the parallelization of reading, NINDEX must be known by all tasks
    IF (NRANK/=NPIO) THEN
      IF (ALLOCATED(NINDEX)) THEN
        IF (SIZE(NINDEX)==IFULL) IFLAG=1
        DEALLOCATE(NINDEX)
      ENDIF
      ALLOCATE(NINDEX(IFULL))
    ENDIF
#ifdef SFX_MPI
    CALL MPI_BCAST(NINDEX,SIZE(NINDEX)*KIND(NINDEX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
  ENDIF
  !
  IPIO_SAVE = NPIO
  !number of covers read by each task
  IPAS = CEILING(IL2*1./NPROC)
  !
  PFIELD(:,:) = 0.
  !
  !first cover number read by the current task
  IDEB = IPAS*NRANK
  !
  IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_2',1,ZHOOK_HANDLE)
  !
  DO JP = 1,IPAS
    !
    IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_3',0,ZHOOK_HANDLE)
    !
    !index of the cover read by this task at this loop index
    JCOVER = IDEB + JP
    !
    IF (JCOVER<=IL2) THEN
      !
      !real number of the cover
      JJ = IMASK(JCOVER)
      !
      IF (TRIM(HREC)=='COVER') THEN
        WRITE(YREC,'(A5,I3.3)') TRIM(HREC),JJ
      ELSE
        WRITE(YLVL,'(I4)') JJ
        YREC = TRIM(HREC)//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      ENDIF
      YCOMMENT='X_Y_'//YREC
      !
      !
      IF (HPROGRAM=='AROME ') THEN
#ifdef ARO
        CALL READ_SURFX1_ARO(YREC,IL1,PFIELD(:,JCOVER),KRESP,YCOMMENT,YDIR)
#endif
      ELSE
        !
        !reads one cover by task
        !
        !number of the I/O task for this read
        NPIO = NRANK
        !
        !reading of the whol cover (HDIR='A')
        CALL READ_SURF(HPROGRAM,YREC,ZFIELD,KRESP,YCOMMENT,'A')
        !
        !NPIO rebecomes the I/O task
        NPIO = IPIO_SAVE
        !
        IDX = IDX_SAVE + JP
        IF (YDIR=='H') THEN
          !
          !send covers to other tasks
          CALL READ_AND_SEND_MPI(ZFIELD,PFIELD(:,JCOVER),IMASKF,NRANK,IDX)
          !
        ELSEIF (YDIR=='A' .OR. YDIR=='E') THEN
          !
          !NPIO needs to know all covers read
          IF (NRANK/=NPIO) THEN
            IDX = IDX + 1
#ifdef SFX_MPI
            CALL MPI_SEND(ZFIELD,SIZE(ZFIELD)*KIND(ZFIELD)/4,MPI_REAL,NPIO,IDX,NCOMM,INFOMPI)
#endif
          ELSE
            CALL PACK_SAME_RANK(IMASKF,ZFIELD,PFIELD(:,JCOVER))
          ENDIF
          !
        ELSE
          CALL ABOR1_SFX("READ_SURFX2COV:HDIR MUST BE H OR A OR E")
        ENDIF
        !
      ENDIF
      !
    ENDIF
    !
    IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_3',1,ZHOOK_HANDLE)
    !
    IF (NRANK==NPIO .OR. YDIR=='H') THEN
      !
      !receives pieces of cover fields
      !ITREQ(:) = 0
      !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_4',0,ZHOOK_HANDLE_OMP)
#ifdef SFX_MPI
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JPROC,IDX,ISTATUS,INFOMPI) FIRSTPRIVATE(ZWORKR)
#endif
      DO JPROC=0,NPROC-1
        !
        !the cover exists and was read
        IF (IPAS*JPROC + JP<=IL2) THEN
          !
          !IF (JPROC<NRANK) THEN
          !  ITREQ(JPROC+1) = JPROC+1
          !ELSE
          !  ITREQ(JPROC+1) = JPROC
          !ENDIF
          !
          IF (JPROC/=NRANK) THEN
            IDX = IDX_SAVE + JP + 1
            !each task receives the part of the cover read that concerns it
            !only NPIO in cas of HDIR/=H
#ifdef SFX_MPI
            CALL MPI_RECV(ZWORKR(:),SIZE(ZWORKR)*KIND(ZWORKR)/4,&
                          MPI_REAL,JPROC,IDX,NCOMM,ISTATUS,INFOMPI)
#endif
            IVAL = IPAS*JPROC + JP
            CALL PACK_SAME_RANK(IMASKF,ZWORKR(:),PFIELD(:,IVAL))
            !
          ENDIF
          !
        ENDIF
        !
      ENDDO
#ifdef SFX_MPI
!$OMP END DO
#endif
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_4',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL

!
      IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_5',0,ZHOOK_HANDLE)
      !
      !waits that all cover pieces are sent
#ifdef SFX_MPI
      IF (YDIR=='H' .AND. IPAS*NRANK+JP<=IL2 .AND. NPROC>1) THEN
        CALL MPI_WAITALL(NPROC-1,NREQ(1:NPROC-1),ISTATUS,INFOMPI)
      ENDIF
#endif
      !
      IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_5',1,ZHOOK_HANDLE)
      !
!      IF (YDIR=='H' .OR. NRANK==NPIO) THEN
!        !packs data
!        IREQ = MAXVAL(ITREQ)
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
!IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_6',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JPROC,IVAL)
!        DO JPROC=0,IREQ-1
!          IVAL = IPAS*JPROC + JP
!          IF (JPROC>=NRANK ) IVAL = IVAL + IPAS
!          CALL PACK_SAME_RANK(IMASKF,ZWORKR(:,JPROC+1),PFIELD(:,IVAL))
!        ENDDO
!!$OMP END DO
!IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_6',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
!      ENDIF
      !
    ENDIF
    !
  ENDDO
  !
  IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_7',0,ZHOOK_HANDLE)
  !
  IF (NRANK/=NPIO .AND. YDIR=='H' .AND. IFLAG==0) THEN
    DEALLOCATE(NINDEX)
    ALLOCATE(NINDEX(0))
  ENDIF
  !
  IDX_R = IDX_R + IPAS + 1
  DEALLOCATE(ZWORKR)
  IF (HPROGRAM /= 'AROME ') THEN
    DEALLOCATE(ZFIELD)
  ENDIF
  IMASKF=>NULL()
  !
  IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_7',1,ZHOOK_HANDLE)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_8',0,ZHOOK_HANDLE)
!
!RJ: what is a point of comment here? last field comment? Should be 'COVER_PACKED' status?
IF (PRESENT(HCOMMENT)) HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_COV_8',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURF_COV

END MODULE MODE_READ_SURF_COV
