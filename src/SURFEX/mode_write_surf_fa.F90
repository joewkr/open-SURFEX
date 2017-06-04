!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_WRITE_SURF_FA
!
INTERFACE WRITE_SURF0_FA
        MODULE PROCEDURE WRITE_SURFX0_FA
        MODULE PROCEDURE WRITE_SURFN0_FA
        MODULE PROCEDURE WRITE_SURFL0_FA
        MODULE PROCEDURE WRITE_SURFC0_FA
END INTERFACE
INTERFACE WRITE_SURFN_FA
        MODULE PROCEDURE WRITE_SURFX1_FA
        MODULE PROCEDURE WRITE_SURFN1_FA
        MODULE PROCEDURE WRITE_SURFL1_FA
        MODULE PROCEDURE WRITE_SURFX2_FA
        MODULE PROCEDURE WRITE_SURFX3_FA
END INTERFACE
INTERFACE WRITE_SURFT_FA
        MODULE PROCEDURE WRITE_SURFT0_FA
        MODULE PROCEDURE WRITE_SURFT2_FA
END INTERFACE
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_FA (&
                                  HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a real scalar
!
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
REAL,               INTENT(IN) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18):: YNAME                  ! Field Name
INTEGER          :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:ERROR_WRITE_SURF_FA:WRITE_SURFX0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX0_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF(LFANOCOMPACT)THEN
  CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ! -- Pour ecrire sans compactage
  CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
 CALL  FAECR_R(KRESP,NUNIT_FA,YNAME,PFIELD)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
IF(LFANOCOMPACT)THEN
  ! On remet la valeur par defaut 
  CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_FA (&
                                  HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write an integer
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NMASK, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18):: YNAME                  ! Field Name
INTEGER          :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN0_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF(LFANOCOMPACT)THEN
  CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ! -- Pour ecrire sans compactage
  CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
 CALL  FAECR_I(KRESP,NUNIT_FA,YNAME,KFIELD)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
IF(LFANOCOMPACT)THEN
  ! On remet la valeur par defaut 
  CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_FA (&
                                  HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a logical
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL,            INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18):: YNAME ! Field Name
INTEGER          :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL0_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF(LFANOCOMPACT)THEN
  CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ! -- Pour ecrire sans compactage
  CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
 CALL  FAECR_L(KRESP,NUNIT_FA,YNAME,OFIELD)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
IF(LFANOCOMPACT)THEN
  ! On remet la valeur par defaut 
  CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_FA (&
                                  HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a character
!
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC      ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(IN)  :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT  ! comment string
!
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER,DIMENSION(40)  :: YFIELD
 CHARACTER(LEN=18)        :: YNAME ! Field Name
INTEGER                  :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL(KIND=JPRB)          :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFC0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND.AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFC0_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF(LFANOCOMPACT)THEN
  CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ! -- Pour ecrire sans compactage
  CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
READ(HFIELD,'(40A1)') YFIELD
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
 CALL  FAECR_C(KRESP,NUNIT_FA,YNAME,40,YFIELD)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
IF(LFANOCOMPACT)THEN
  ! On remet la valeur par defaut 
  CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFC0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_FA (&
                                  HREC,KL,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 1D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NMASK, NFULL, CPREFIX1D, &
                            LFANOCOMPACT 
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
!
 CHARACTER(LEN=12),   INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN) :: KL       ! number of points
REAL, DIMENSION(KL), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
INTEGER                :: I,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL                   :: ZMEAN, ZCOUNT
REAL       :: XTIME0
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD))) :: ZWORK   ! work array read in the file
REAL(KIND=JPRB)        :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX1_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX1_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF(HDIR=='H')THEN
  CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
ELSE !no horizontal dim. case (not masked)
  ZWORK(1:KL)=PFIELD(1:KL)
  ZWORK(KL+1:NFULL)=SUM(PFIELD(1:KL))/REAL(KL)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  IF(LFANOCOMPACT)THEN
    CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    ! -- Pour ecrire sans compactage
    CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    CALL FAIENC(KRESP,NUNIT_FA,CPREFIX1D,0,HREC,ZWORK,.FALSE.)
    IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
    ! On remet la valeur par defaut 
    CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ELSE
    ZMEAN =0.0
    ZCOUNT=0.0
    DO I=1,NFULL
      IF(ZWORK(I)/=XUNDEF)THEN
        ZMEAN =ZMEAN+ZWORK(I)
        ZCOUNT=ZCOUNT+1.0
      ENDIF
    ENDDO
    IF (ZCOUNT.GT.0.0) ZMEAN=ZMEAN/ZCOUNT
    WHERE(ZWORK(:)==XUNDEF)ZWORK(:)=ZMEAN
    CALL FAIENC(KRESP,NUNIT_FA,CPREFIX1D,0,HREC,ZWORK,.FALSE.)
    IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX1_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX1_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_FA (HREC,KL1,KL2,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NMASK, NFULL, &
                            CPREFIX2D, LFANOCOMPACT
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,                  INTENT(IN) :: KL1      ! number of points
INTEGER,                  INTENT(IN) :: KL2      ! 2nd dimension
REAL, DIMENSION(KL1,KL2), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
LOGICAL :: GFOUND
CHARACTER(LEN=4)  :: YPREFIX
CHARACTER(LEN=3)  :: YPATCH
INTEGER           :: I, JL ! loop counter
INTEGER           :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL  :: XTIME0
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2)) :: ZWORK   ! work array read in the file
REAL, DIMENSION(SIZE(PFIELD,2))       :: ZMEAN, ZCOUNT
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX2_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX2_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !    
  IF(LFANOCOMPACT)THEN
    CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    ! -- Pour ecrire sans compactage
    CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    DO JL=1,SIZE(ZWORK,2)
      WRITE(YPATCH,'(I3.3)')JL
      YPREFIX=CPREFIX2D//YPATCH//'_'
      CALL FAIENC(KRESP,NUNIT_FA,YPREFIX,0,HREC,ZWORK(:,JL),.FALSE.)
      IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
    END DO
    ! On remet la valeur par defaut 
    CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ELSE
    ZMEAN (:)=0.0
    ZCOUNT(:)=0.0
    DO I=1,NFULL
      DO JL=1,SIZE(ZWORK,2)
        IF(ZWORK(I,JL)/=XUNDEF) THEN
          ZMEAN (JL)=ZMEAN(JL)+ZWORK(I,JL)
          ZCOUNT(JL)=ZCOUNT(JL)+1.0
        ENDIF
      ENDDO
    ENDDO
    WHERE(ZCOUNT(:)>0.0)ZMEAN(:)=ZMEAN(:)/ZCOUNT(:)        
    DO JL=1,SIZE(ZWORK,2)
      WHERE(ZWORK(:,JL)==XUNDEF)ZWORK(:,JL)=ZMEAN(JL)
      WRITE(YPATCH,'(I3.3)')JL
      YPREFIX=CPREFIX2D//YPATCH//'_'
      CALL FAIENC(KRESP,NUNIT_FA,YPREFIX,0,HREC,ZWORK(:,JL),.FALSE.)
      IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
    END DO
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX2_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX2_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFX3_FA (HREC,KL1,KL2,KL3,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NMASK, NFULL, &
                            CPREFIX2D, LFANOCOMPACT
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,                  INTENT(IN) :: KL1      ! number of points
INTEGER,                  INTENT(IN) :: KL2      ! 2nd dimension
INTEGER,                  INTENT(IN) :: KL3      ! 2nd dimension
REAL, DIMENSION(KL1,KL2,KL3), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
LOGICAL :: GFOUND
CHARACTER(LEN=4)  :: YPREFIX
CHARACTER(LEN=3)  :: YPATCH
INTEGER           :: I, JL, JP ! loop counter
INTEGER           :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL  :: XTIME0
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2),SIZE(PFIELD,3)) :: ZWORK   ! work array read in the file
REAL, DIMENSION(SIZE(PFIELD,2),SIZE(PFIELD,3))       :: ZMEAN, ZCOUNT
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX2_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX2_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !    
  IF(LFANOCOMPACT)THEN
    CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    ! -- Pour ecrire sans compactage
    CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    DO JP=1,SIZE(ZWORK,3)
      DO JL=1,SIZE(ZWORK,2)
        WRITE(YPATCH,'(I3.3)')JL
        YPREFIX=CPREFIX2D//YPATCH//'_'
        CALL FAIENC(KRESP,NUNIT_FA,YPREFIX,0,HREC,ZWORK(:,JL,JP),.FALSE.)
        IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
      ENDDO
    END DO
    ! On remet la valeur par defaut 
    CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ELSE
    ZMEAN (:,:)=0.0
    ZCOUNT(:,:)=0.0
    DO I=1,NFULL
      DO JP=1,SIZE(ZWORK,3)
        DO JL=1,SIZE(ZWORK,2)
          IF(ZWORK(I,JL,JP)/=XUNDEF) THEN
            ZMEAN (JL,JP)=ZMEAN(JL,JP)+ZWORK(I,JL,JP)
            ZCOUNT(JL,JP)=ZCOUNT(JL,JP)+1.0
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    WHERE(ZCOUNT(:,:)>0.0)ZMEAN(:,:)=ZMEAN(:,:)/ZCOUNT(:,:)
    DO JP=1,SIZE(ZWORK,3)        
      DO JL=1,SIZE(ZWORK,2)
        WHERE(ZWORK(:,JL,JP)==XUNDEF)ZWORK(:,JL,JP)=ZMEAN(JL,JP)
        WRITE(YPATCH,'(I3.3)')JL
        YPREFIX=CPREFIX2D//YPATCH//'_'
        CALL FAIENC(KRESP,NUNIT_FA,YPREFIX,0,HREC,ZWORK(:,JL,JP),.FALSE.)
        IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
      ENDDO
    END DO
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFX3_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX3_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_FA (&
                                  HREC,KL,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write an integer array
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NMASK, NFULL, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
!
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN) :: KL       ! number of points
INTEGER, DIMENSION(KL), INTENT(IN) :: KFIELD   ! array containing the data field
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
LOGICAL :: GFOUND
 CHARACTER(LEN=18)         :: YNAME! Field Nam
INTEGER                   :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
INTEGER, DIMENSION(MAX(NFULL,SIZE(KFIELD))) :: IWORK  ! work array read in the file
REAL   :: XTIME0
REAL(KIND=JPRB)           :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN1_FA',0,ZHOOK_HANDLE)
!
KRESP = 0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN1_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (HDIR/='H' .OR. HREC=="-") THEN
  IWORK(1:KL) = KFIELD
ELSE
  CALL GATHER_AND_WRITE_MPI(KFIELD,IWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !    
  IF(LFANOCOMPACT)THEN
    CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    ! -- Pour ecrire sans compactage
    CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ENDIF
  !
  YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
  !
  CALL  FAECR_I_D(KRESP,NUNIT_FA,YNAME,KL,IWORK(1:KL))
  IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
  !
  IF(LFANOCOMPACT)THEN
    ! On remet la valeur par defaut 
    CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFN1_FA',1,ZHOOK_HANDLE)
RETURN
!
END SUBROUTINE WRITE_SURFN1_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_FA (&
                                  HREC,KL,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write a logical array
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN) :: KL       ! number of points
LOGICAL, DIMENSION(KL), INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18):: YNAME ! Field Name
INTEGER          :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
REAL :: XTIME0
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL1_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL1_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  IF(LFANOCOMPACT)THEN
    CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
    ! -- Pour ecrire sans compactage
    CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ENDIF
  !
  YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
  CALL  FAECR_L_D(KRESP,NUNIT_FA,YNAME,KL,OFIELD)
  IF (KRESP/=0) CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
  !
  IF(LFANOCOMPACT)THEN
    ! On remet la valeur par defaut 
    CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFL1_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL1_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_FA (&
                                  HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
USE MODD_IO_SURF_FA, ONLY : CPREFIX1D, NUNIT_FA, LFANOCOMPACT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18)     :: YNAME ! Field Name
INTEGER               :: IRET
INTEGER               :: IHOUR, IMIN, ISEC
INTEGER               :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT0_FA',0,ZHOOK_HANDLE)
!
KRESP=0
!
IF (HREC=='DTCUR') THEN
!        
  IHOUR = FLOOR(PTIME)/3600
  IMIN  = FLOOR(PTIME)/60 - IHOUR * 60
  ISEC  = NINT(PTIME) - IHOUR * 3600 - IMIN * 60
  CALL FANDAR(IRET,NUNIT_FA,(/ KYEAR, KMONTH, KDAY, IHOUR, IMIN, ISEC, 0, 0, 0, 0, 0 /))
!
ELSE
!
  CALL IO_BUFF(&
                HREC,'W',GFOUND)
  IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT0_FA',1,ZHOOK_HANDLE)
  IF (GFOUND) RETURN
!
END IF
!
ITDATE(1) = KYEAR
ITDATE(2) = KMONTH
ITDATE(3) = KDAY
!
IF(LFANOCOMPACT)THEN
  CALL FAVEUR(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
  ! -- Pour ecrire sans compactage
  CALL FAGOTE(KRESP,NUNIT_FA,-1,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)//'%TDATE'
 CALL  FAECR_I_D(KRESP,NUNIT_FA,YNAME,3,ITDATE)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
YNAME=TRIM(CPREFIX1D)//TRIM(HREC)//'%TIME'
 CALL  FAECR_R(KRESP,NUNIT_FA,YNAME,PTIME)
IF (KRESP/=0) THEN
  CALL ERROR_WRITE_SURF_FA(HREC,KRESP)
ENDIF
!
IF(LFANOCOMPACT)THEN
  ! On remet la valeur par defaut 
  CALL FAGOTE(KRESP,NUNIT_FA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT0_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0_FA
!
!     #############################################################
      SUBROUTINE WRITE_SURFT2_FA (&
                                  HREC,KL1,KL2,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, CPREFIX1D, NLUOUT
!
USE MODE_FASURFEX
!
USE MODI_IO_BUFF
USE MODI_ABOR1_SFX
USE MODI_ERROR_WRITE_SURF_FA
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                      INTENT(IN) :: KL1      ! number of points
INTEGER,                      INTENT(IN) :: KL2      ! 2nd dimension
INTEGER, DIMENSION(KL1,KL2), INTENT(IN)  :: KYEAR    ! year
INTEGER, DIMENSION(KL1,KL2), INTENT(IN)  :: KMONTH   ! month
INTEGER, DIMENSION(KL1,KL2), INTENT(IN)  :: KDAY     ! day
REAL,    DIMENSION(KL1,KL2), INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=18):: YNAME ! Field Name
INTEGER, DIMENSION(3,SIZE(KYEAR,1),SIZE(KYEAR,2)) :: ITDATE
REAL :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT2_FA',0,ZHOOK_HANDLE)
!
KRESP = 0
!
CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT2_FA',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  ITDATE(1,:,:) = KYEAR  (:,:)
  ITDATE(2,:,:) = KMONTH (:,:)
  ITDATE(3,:,:) = KDAY   (:,:)
  !
  YNAME=TRIM(CPREFIX1D)//TRIM(HREC)
  WRITE(NLUOUT,*) ' WRITE_SURFT2_FA : time in 2 dimensions not yet implemented : YNAME=',YNAME,'ITDATE=',ITDATE
  CALL ABOR1_SFX('MODE_WRITE_SURF_FA:WRITE_SURFT2_FA: time in 2 dimensions not yet implemented')
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_FA:WRITE_SURFT2_FA',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT2_FA
!
END MODULE MODE_WRITE_SURF_FA
