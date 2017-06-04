!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_SURF_ASC
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURF_ASC is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!      J.Escobar      10/06/2013: replace DOUBLE PRECISION by REAL to handle problem for promotion of real on IBM SP
!----------------------------------------------------------------------------
!
INTERFACE READ_SURF0_ASC
        MODULE PROCEDURE READ_SURFX0_ASC
        MODULE PROCEDURE READ_SURFN0_ASC
        MODULE PROCEDURE READ_SURFL0_ASC
        MODULE PROCEDURE READ_SURFC0_ASC
END INTERFACE
INTERFACE READ_SURFN_ASC
        MODULE PROCEDURE READ_SURFX1_ASC
        MODULE PROCEDURE READ_SURFN1_ASC
        MODULE PROCEDURE READ_SURFL1_ASC
        MODULE PROCEDURE READ_SURFX2_ASC
END INTERFACE
INTERFACE READ_SURFT_ASC
        MODULE PROCEDURE READ_SURFT0_ASC
        MODULE PROCEDURE READ_SURFT1_ASC
        MODULE PROCEDURE READ_SURFT2_ASC
END INTERFACE
!
CONTAINS
!
!     #############################################################
      SUBROUTINE READ_SURFX0_ASC (&
                                  HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!
USE MODD_IO_SURF_ASC, ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*), INTENT(IN)  :: HREC     ! name of the article to be read
REAL,              INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,           INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50):: YCOMMENT
 CHARACTER(LEN=6) :: YMASK
LOGICAL          :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) PFIELD
!
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX0_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFX1_ASC (&
                                  HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface 
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, NREQ
!
USE MODD_IO_SURF_ASC,  ONLY : NUNIT, NLUOUT, NMASK, NFULL, CMASK
!
USE MODE_POS_SURF
!
USE MODI_PACK_SAME_RANK
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT)  :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),    INTENT(IN)  :: HDIR     ! type of field :
                                             ! 'H' : field with
                                             !       horizontal spatial dim.
                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=6)  :: YMASK
INTEGER           :: IL1, INFOMPI
LOGICAL :: GFOUND
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK
REAL   :: XTIME0
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE,NPROC-1) :: ISTATUS
#endif
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX1_ASC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(PFIELD)
!
KRESP=0
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
  ALLOCATE(ZWORK(IL1))
ENDIF
!
IF (NRANK==NPIO) THEN
  !
  IF (HDIR=='A') THEN
    ALLOCATE(ZWORK(IL1))
  ELSEIF (HDIR/='-') THEN
    ALLOCATE(ZWORK(NFULL))
  END IF
  !
  IF (HDIR=='A' .OR. HDIR=='-' .OR. HDIR=='E') THEN
    CALL POSNAM(NUNIT,CMASK//' '//HREC,GFOUND,NLUOUT)
    IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT)
  ELSE
    YMASK=CMASK
    CALL IO_BUFF(&
                HREC,'R',GFOUND)
    IF (GFOUND) YMASK='FULL  '
    CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
  ENDIF
  !
  READ(NUNIT,FMT=*,IOSTAT=KRESP)
  READ(NUNIT,FMT='(A50)',IOSTAT=KRESP) HCOMMENT
  READ(NUNIT,FMT=*,IOSTAT=KRESP) ZWORK
  !
ELSEIF (HDIR/='-') THEN
  ALLOCATE(ZWORK(0))
ENDIF
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (KRESP/=0) CALL ERROR_READ_SURF_ASC(HREC,KRESP)
!
IF (HDIR=='E') THEN
  IF ( NRANK==NPIO ) THEN
    CALL PACK_SAME_RANK(NMASK,ZWORK(:),PFIELD(:))
  ENDIF
ELSEIF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:) = ZWORK(1:IL1)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(ZWORK,IL1*KIND(ZWORK)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  PFIELD(:) = ZWORK(1:IL1)
ELSE
  CALL READ_AND_SEND_MPI(ZWORK,PFIELD,NMASK)
  !IF (NRANK==NPIO) THEN
  !  CALL MPI_WAITALL(NPROC-1,NREQ,ISTATUS,INFOMPI)
  !ENDIF  
ENDIF
!
DEALLOCATE(ZWORK)
!  
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX1_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFX2_ASC (&
                                  HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ, NREQ
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, NMASK, NFULL, CMASK
!
USE MODE_POS_SURF
!
USE MODI_PACK_SAME_RANK
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
LOGICAL :: GFOUND
 CHARACTER(LEN=6)  :: YMASK
INTEGER           :: IL1, IL2, INFOMPI
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK2
REAL   :: XTIME0
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE,NPROC-1) :: ISTATUS
#endif
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX2_ASC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
!
KRESP=0
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
  ALLOCATE(ZWORK2(IL1,IL2))
ENDIF
!
IF (NRANK==NPIO) THEN
  !
  IF (HDIR=='A') THEN
    ALLOCATE(ZWORK2(IL1,IL2))
  ELSEIF (HDIR/='-') THEN
    ALLOCATE(ZWORK2(NFULL,IL2))
  END IF                 
  !
  IF (HDIR=='A' .OR. HDIR=='-'.OR.HDIR=='E') THEN
    CALL POSNAM(NUNIT,CMASK//' '//HREC,GFOUND,NLUOUT)
    IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT)
  ELSE
    YMASK=CMASK
    CALL IO_BUFF(&
                HREC,'R',GFOUND)
    IF (GFOUND) YMASK='FULL  '
    CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
  ENDIF
  !
  READ(NUNIT,FMT=*,IOSTAT=KRESP)
  READ(NUNIT,FMT='(A50)',IOSTAT=KRESP) HCOMMENT
  READ(NUNIT,FMT=*,IOSTAT=KRESP) ZWORK2
  !
ELSEIF (HDIR/='-') THEN
  ALLOCATE(ZWORK2(0,0))
ENDIF
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (KRESP/=0) CALL ERROR_READ_SURF_ASC(HREC,KRESP)
!
IF (HDIR=='E') THEN
  IF ( NRANK==NPIO ) THEN
    CALL PACK_SAME_RANK(NMASK,ZWORK2(:,:),PFIELD(:,:))
  ENDIF
ELSEIF (HDIR=='A') THEN
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:,:) = ZWORK2(1:IL1,:)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(ZWORK2,IL1*IL2*KIND(ZWORK2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  IF (NRANK==NPIO) PFIELD(:,:) = ZWORK2(1:IL1,:)
ELSE
  CALL READ_AND_SEND_MPI(ZWORK2,PFIELD,NMASK)
  !IF (NRANK==NPIO) THEN
  !  CALL MPI_WAITALL(NPROC-1,NREQ,ISTATUS,INFOMPI)
  !ENDIF  
ENDIF
!
DEALLOCATE(ZWORK2)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFX2_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX2_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFN0_ASC (&
                                  HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, NMASK, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
LOGICAL           :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFN0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) KFIELD
!
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFN0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFN0_ASC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFN0_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFN1_ASC (&
                                  HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, NMASK, NFULL, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:),  INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=6)  :: YMASK
INTEGER           :: IL1, INFOMPI
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IWORK
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
REAL   :: XTIME0
LOGICAL :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFN1_ASC',0,ZHOOK_HANDLE)
!  
IL1 = SIZE(KFIELD)
!
KRESP = 0
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
  ALLOCATE(IWORK(IL1))
ENDIF
!
IF (NRANK==NPIO) THEN
  !
  !
  IF (HDIR=='A') THEN
    ALLOCATE(IWORK(IL1))
  ELSEIF (HDIR/='-') THEN
    ALLOCATE(IWORK(NFULL))
  END IF            
  !
  IF (HDIR=='A' .OR. HDIR=='-') THEN
    CALL POSNAM(NUNIT,CMASK//' '//HREC,GFOUND,NLUOUT)
    IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT)
  ELSE
    YMASK=CMASK
    CALL IO_BUFF(&
                HREC,'R',GFOUND)
    IF (GFOUND) YMASK='FULL  '
    CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
  ENDIF
  !
  READ(NUNIT,FMT=*,IOSTAT=KRESP)
  READ(NUNIT,FMT='(A50)',IOSTAT=KRESP) HCOMMENT
  READ(NUNIT,FMT=*,IOSTAT=KRESP) IWORK
  !
  !
ELSEIF (HDIR/='-') THEN
  ALLOCATE(IWORK(0))
ENDIF
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (KRESP/=0) CALL ERROR_READ_SURF_ASC(HREC,KRESP)
!
IF (HDIR=='A') THEN
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    KFIELD(:) = IWORK(1:IL1)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(IWORK,IL1*KIND(IWORK)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
  KFIELD(:) = IWORK(1:IL1)
ELSE
  CALL READ_AND_SEND_MPI(IWORK,KFIELD,NMASK)
ENDIF
!
DEALLOCATE(IWORK)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFN1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN1_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFC0_ASC (&
                                  HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read a character
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC      ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(OUT) :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT  ! comment
!
!*      0.2   Declarations of local variables
!
INTEGER :: IRESP
 CHARACTER(LEN=50):: YCOMMENT
 CHARACTER(LEN=6) :: YMASK
LOGICAL          :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFC0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,IOSTAT=IRESP)
READ(NUNIT,FMT='(A50)',IOSTAT=IRESP) YCOMMENT
READ(NUNIT,FMT='(A40)',IOSTAT=IRESP) HFIELD
!
IF (IRESP/=0) CALL ERROR_READ_SURF_ASC(HREC,KRESP)

HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFC0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFC0_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFL0_ASC (&
                                  HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READL0* - routine to read a logical
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,            INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
LOGICAL           :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFL0_ASC',0,ZHOOK_HANDLE)
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) OFIELD
!
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFL0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFL0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL0_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFL1_ASC (&
                                  HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:),  INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=6) :: YMASK
INTEGER          :: INFOMPI, IL1
REAL :: XTIME0
LOGICAL :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFL1_ASC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(OFIELD)
!
KRESP = 0
!
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
  ! 
  YMASK=CMASK
  CALL IO_BUFF(&
                HREC,'R',GFOUND)
  IF (GFOUND) YMASK='FULL  '
  !
  CALL POSNAM(NUNIT,YMASK//' '//HREC,GFOUND,NLUOUT)
  IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
  !
  READ(NUNIT,FMT=*,IOSTAT=KRESP)
  READ(NUNIT,FMT='(A50)',IOSTAT=KRESP) HCOMMENT
  READ(NUNIT,FMT=*,IOSTAT=KRESP) OFIELD
  !
  !
ENDIF
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (KRESP/=0) CALL ERROR_READ_SURF_ASC(HREC,KRESP)
!
#ifdef SFX_MPI
IF (NPROC>1 .AND. HDIR/='A') THEN
  XTIME0 = MPI_WTIME()
  CALL MPI_BCAST(OFIELD,IL1,MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
  XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
ENDIF
#endif
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFL1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL1_ASC
!
!
!     #############################################################
      SUBROUTINE READ_SURFT0_ASC (&
                                  HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KYEAR    ! year
INTEGER,            INTENT(OUT) :: KMONTH   ! month
INTEGER,            INTENT(OUT) :: KDAY     ! day
REAL,               INTENT(OUT) :: PTIME    ! year
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
LOGICAL           :: GFOUND
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT0_ASC',0,ZHOOK_HANDLE)
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//TRIM(HREC)//'%TDATE',GFOUND,NLUOUT)
!IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) ITDATE(:)
!
KYEAR  = ITDATE(1)
KMONTH = ITDATE(2)
KDAY   = ITDATE(3)
!
 CALL POSNAM(NUNIT,YMASK//' '//TRIM(HREC)//'%TIME',GFOUND,NLUOUT)
IF (.NOT. GFOUND) CALL POSNAM(NUNIT,'FULL  '//' '//HREC,GFOUND,NLUOUT) ! used for auxilliary files
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) PTIME
!
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT0_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFT1_ASC (&
                                  HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT2* - routine to read a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:), INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:), INTENT(OUT) :: KDAY     ! day
REAL,    DIMENSION(:), INTENT(OUT) :: PTIME    ! year
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
LOGICAL           :: GFOUND
INTEGER, DIMENSION(3,SIZE(KYEAR)) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT1_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//TRIM(HREC)//'%TDATE',GFOUND,NLUOUT)
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) ITDATE(:,:)
!
KYEAR  (:) = ITDATE(1,:)
KMONTH (:) = ITDATE(2,:)
KDAY   (:) = ITDATE(3,:)
!
 CALL POSNAM(NUNIT,CMASK//' '//TRIM(HREC)//'%TIME',GFOUND,NLUOUT)
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) PTIME
!
HCOMMENT = YCOMMENT
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT1_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT1_ASC
!
!     #############################################################
      SUBROUTINE READ_SURFT2_ASC (&
                                  HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT2* - routine to read a date
!
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NLUOUT, CMASK
!
USE MODE_POS_SURF
!
USE MODI_IO_BUFF
USE MODI_ERROR_READ_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KDAY     ! day
REAL,    DIMENSION(:,:), INTENT(OUT) :: PTIME    ! year
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=50) :: YCOMMENT
 CHARACTER(LEN=6)  :: YMASK
LOGICAL           :: GFOUND
INTEGER, DIMENSION(3,SIZE(KYEAR,1),SIZE(KYEAR,2)) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT2_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
YMASK=CMASK
 CALL IO_BUFF(&
                HREC,'R',GFOUND)
IF (GFOUND) YMASK='FULL  '
!
 CALL POSNAM(NUNIT,YMASK//' '//TRIM(HREC)//'%TDATE',GFOUND,NLUOUT)
!
READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) ITDATE(:,:,:)
!
KYEAR  (:,:) = ITDATE(1,:,:)
KMONTH (:,:) = ITDATE(2,:,:)
KDAY   (:,:) = ITDATE(3,:,:)
!
 CALL POSNAM(NUNIT,YMASK//' '//TRIM(HREC)//'%TIME',GFOUND,NLUOUT)

READ(NUNIT,FMT=*,END=100)
READ(NUNIT,FMT='(A50)') YCOMMENT
READ(NUNIT,FMT=*,ERR=100) PTIME
!
HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT2_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_READ_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_ASC:READ_SURFT2_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT2_ASC
!
END MODULE MODE_READ_SURF_ASC
