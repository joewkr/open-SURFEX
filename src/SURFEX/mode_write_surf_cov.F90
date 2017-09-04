!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.

MODULE MODE_WRITE_SURF_COV
!
INTERFACE WRITE_SURF_COV
  MODULE PROCEDURE WRITE_SURF_COV
END INTERFACE
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURF_COV (HSELECT, &
                                 HPROGRAM,HREC,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODI_WRITE_SURF
#ifdef SFX_MNH
USE MODI_WRITE_SURFX2COV_MNH
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=*),     INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(:), INTENT(IN)  :: OFLAG  ! mask for array filling
INTEGER,              INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN)  :: HCOMMENT ! Comment string
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12)  :: YREC
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IL1
INTEGER            :: IL2
 CHARACTER(LEN=1)   :: YDIR
INTEGER            :: JCOVER, ICOVER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_COV',0,ZHOOK_HANDLE)
!
YREC = HREC
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
IL1  = SIZE(PFIELD,1)
IL2  = SIZE(PFIELD,2)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
    CALL WRITE_SURFX2COV_MNH(YREC,IL1,IL2,PFIELD,OFLAG,KRESP,HCOMMENT,YDIR)
#endif
ELSE
  !
!RJ: could be generalized for all
  IF (NRANK==NPIO) THEN
    IF (HPROGRAM=='LFI   ') THEN
      YREC = 'COVER_PACKED'
      YCOMMENT='-'
!!    YCOMMENT=HCOMMENT
      CALL WRITE_SURF(HSELECT,HPROGRAM,YREC,.FALSE.,KRESP,YCOMMENT)
    ENDIF
  ENDIF
  !
  ICOVER=0
  DO JCOVER=1,SIZE(OFLAG)
    !
    WRITE(YREC,'(A5,I3.3)') 'COVER',JCOVER
    YCOMMENT='X_Y_'//YREC
    IF (.NOT. OFLAG(JCOVER)) CYCLE
    ICOVER = ICOVER+1
    !
    CALL WRITE_SURF(HSELECT, HPROGRAM,YREC,PFIELD(:,ICOVER),KRESP,YCOMMENT,YDIR)
    !
  END DO
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_COV',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURF_COV

END MODULE MODE_WRITE_SURF_COV
