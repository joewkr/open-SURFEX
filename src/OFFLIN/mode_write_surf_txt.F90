!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
MODULE MODE_WRITE_SURF_TXT
!
USE MODI_INIT_WRITE_TXT
INTERFACE WRITE_SURF0_TXT
        MODULE PROCEDURE WRITE_SURFX0_TXT
        MODULE PROCEDURE WRITE_SURFN0_TXT
        MODULE PROCEDURE WRITE_SURFC0_TXT
        MODULE PROCEDURE WRITE_SURFL0_TXT
END INTERFACE
INTERFACE WRITE_SURFX_TXT
        MODULE PROCEDURE WRITE_SURFX1_TXT
        MODULE PROCEDURE WRITE_SURFX2_TXT
END INTERFACE
INTERFACE WRITE_SURFN_TXT
        MODULE PROCEDURE WRITE_SURFN1_TXT
        MODULE PROCEDURE WRITE_SURFL1_TXT
END INTERFACE
INTERFACE WRITE_SURFT_TXT
        MODULE PROCEDURE WRITE_SURFT0_TXT
        MODULE PROCEDURE WRITE_SURFT2_TXT
END INTERFACE
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_TXT(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a real scalar
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
REAL,               INTENT(IN) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX0_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) PFIELD

IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX0_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX0_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_TXT(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write an integer
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN0_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) KFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN0_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN0_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_TXT(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a character
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC      ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(IN)  :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT  ! comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFC0_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT='(A40)',ERR=100) HFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFC0_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFC0_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_TXT(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a logical
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL,            INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL0_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) OFIELD

IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL0_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL0_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_TXT (HSELECT, HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 1D array for the externalised surface 
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_TXT,        ONLY : NMASK, NFULL, CMASK
USE MODD_WRITE_TXT,          ONLY : CVAR, NVAR, NIND
!
USE MODI_ERROR_WRITE_SURF_TXT
USE MODI_GATHER_AND_WRITE_MPI
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
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=12),   INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:),  INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
INTEGER                :: ISIZE
LOGICAL                :: LWFL=.FALSE.
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD))) :: ZWORK   ! work array read in the file
DOUBLE PRECISION       :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX1_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!  
IF (HDIR=='-') THEN
  ISIZE = SIZE(PFIELD)
  ZWORK(1:ISIZE) = PFIELD
ELSE
  ISIZE = SIZE(ZWORK)
  CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  ! 
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  CALL INIT_WRITE_TXT(HSELECT, HREC,LWFL)
  !
  IF (LWFL) WRITE(NIND,FMT='(50D14.6)',IOSTAT=KRESP) ZWORK(1:ISIZE)
  !
  IF (KRESP/=0) CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX1_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX1_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_TXT (HSELECT, HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_TXT,        ONLY : NMASK, NFULL
USE MODD_WRITE_TXT,          ONLY : CVAR, NVAR, NIND
!
USE MODI_ERROR_WRITE_SURF_TXT
USE MODI_GATHER_AND_WRITE_MPI
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
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
INTEGER :: ISIZE
LOGICAL :: LWFL=.FALSE.
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2)) :: ZWORK   ! work array read in the file
DOUBLE PRECISION  :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX2_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
IF (HDIR=='-') THEN
  ISIZE = SIZE(PFIELD,1)
  ZWORK(1:ISIZE,:) = PFIELD
ELSE
  ISIZE = SIZE(ZWORK,1)
  CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  CALL INIT_WRITE_TXT(HSELECT, HREC,LWFL)
  !
  IF (LWFL) WRITE(NIND,FMT='(50D14.6)',IOSTAT=KRESP) ZWORK(1:ISIZE,:)
  !
  IF (KRESP/=0) CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)   
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFX2_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX2_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_TXT(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write an integer array
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:),  INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
!INTEGER, DIMENSION(NFULL) :: IWORK  ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN1_TXT',0,ZHOOK_HANDLE)
!
KRESP = 0
!
!plm IF (HREC(1:8)=="EMISTIME") THEN
  !plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
  !plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
  !plm WRITE(NUNIT,FMT='(100I8)',ERR=100) KFIELD

!plm ELSE
  !plm CALL UNPACK_SAME_RANK(NMASK,KFIELD,IWORK(:))

  !plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
  !plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
  !plm WRITE(NUNIT,FMT='(100I8)',ERR=100) IWORK
  !
!plm ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN1_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFN1_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN1_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_TXT(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write a logical array
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:), INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL1_TXT',0,ZHOOK_HANDLE)
KRESP=0
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) OFIELD

IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL1_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFL1_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL1_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_TXT(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!             ------------
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT0_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm ITDATE(1) = KYEAR
!plm ITDATE(2) = KMONTH
!plm ITDATE(3) = KDAY

!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TDATE'
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) ITDATE(:)

!-------------------------------------------------------------------------------
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TIME'
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) PTIME

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT0_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT0_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0_TXT
!
!     #############################################################
      SUBROUTINE WRITE_SURFT2_TXT(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
USE MODI_ERROR_WRITE_SURF_TXT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),       INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KYEAR    ! year
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KMONTH   ! month
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KDAY     ! day
REAL,    DIMENSION(:,:), INTENT(IN)  :: PTIME    ! time
INTEGER,                 INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),      INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(3,SIZE(KYEAR,1),SIZE(KYEAR,2)) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT2_TXT',0,ZHOOK_HANDLE)
!
KRESP=0
!
!plm ITDATE(1,:,:) = KYEAR  (:,:)
!plm ITDATE(2,:,:) = KMONTH (:,:)
!plm ITDATE(3,:,:) = KDAY   (:,:)

!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TDATE'
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) ITDATE(:,:,:)

!-------------------------------------------------------------------------------
!
!plm WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TIME'
!plm WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
!plm WRITE(NUNIT,FMT=*,ERR=100) PTIME

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT2_TXT',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_TXT(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_TXT:WRITE_SURFT2_TXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT2_TXT
!
END MODULE MODE_WRITE_SURF_TXT
