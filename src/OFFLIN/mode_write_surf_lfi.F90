!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
MODULE MODE_WRITE_SURF_LFI
!
#ifdef SFX_LFI
!
USE MODI_GET_LUOUT
INTERFACE WRITE_SURF0_LFI
        MODULE PROCEDURE WRITE_SURFX0_LFI
        MODULE PROCEDURE WRITE_SURFN0_LFI
        MODULE PROCEDURE WRITE_SURFL0_LFI
        MODULE PROCEDURE WRITE_SURFC0_LFI
END INTERFACE
INTERFACE WRITE_SURFN_LFI
        MODULE PROCEDURE WRITE_SURFX1_LFI
        MODULE PROCEDURE WRITE_SURFN1_LFI
        MODULE PROCEDURE WRITE_SURFL1_LFI
        MODULE PROCEDURE WRITE_SURFX2_LFI
        MODULE PROCEDURE WRITE_SURFX3_LFI
END INTERFACE
INTERFACE WRITE_SURFT_LFI
        MODULE PROCEDURE WRITE_SURFT0_LFI
        MODULE PROCEDURE WRITE_SURFT1_LFI 
        MODULE PROCEDURE WRITE_SURFT2_LFI        
END INTERFACE
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_LFI (&
                                   HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a real scalar
!
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX0_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX0_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL FMWRITX0(CFILEOUT_LFI,HREC,CLUOUT_LFI,1,PFIELD,4,100,HCOMMENT,KRESP)
!
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX0_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_LFI (&
                                   HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write an integer
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI, &
                                    LMNH_COMPATIBLE, NIU, NIB, NIE, NJU, NJB, NJE
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN0_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
IF (LMNH_COMPATIBLE .AND. HREC=='IMAX') THEN
  NIU = KFIELD+2
  NIB = 2
  NIE = KFIELD+1
END IF
IF (LMNH_COMPATIBLE .AND. HREC=='JMAX') THEN
  NJU = KFIELD+2
  NJB = 2
  NJE = KFIELD+1
END IF
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN0_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL FMWRITN0(CFILEOUT_LFI,HREC,CLUOUT_LFI,1,KFIELD,4,100,HCOMMENT,KRESP)
!
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN0_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_LFI (&
                                   HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a logical
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL0_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL0_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL FMWRITL0(CFILEOUT_LFI,HREC,CLUOUT_LFI,1,OFIELD,4,100,HCOMMENT,KRESP)
!
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL0_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_LFI (&
                                   HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a character
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI, LMNH_COMPATIBLE, LCARTESIAN
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFC0_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFC0_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
 CALL FMWRITC0(CFILEOUT_LFI,HREC,CLUOUT_LFI,1,HFIELD,4,100,HCOMMENT,KRESP)
!
IF (HREC=="GRID_TYPE") LMNH_COMPATIBLE = (HFIELD=="CARTESIAN " .OR. HFIELD=="CONF PROJ ")
IF (HREC=="GRID_TYPE" .AND. LMNH_COMPATIBLE) LCARTESIAN=(HFIELD=="CARTESIAN ")
!
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFC0_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_LFI (&
                                   HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 1D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI, ONLY : CFILEOUT_LFI, CLUOUT_LFI, NMASK, NFULL, &
                             LMNH_COMPATIBLE, NIU, NIB, NIE, NJU, NJB, NJE
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_GET_SURF_UNDEF
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
REAL, DIMENSION(:),  INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=20)        :: YREC
INTEGER                  :: JI, JJ
DOUBLE PRECISION         :: XTIME0
REAL                     :: ZUNDEF  ! default value
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD)))   :: ZWORK   ! work array read in the file
REAL, DIMENSION(NIU,NJU) :: ZWORK2D ! work array read in a MNH file
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (HDIR=='H') CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
!  
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  IF (HDIR=='H') THEN
    !
    CALL GET_SURF_UNDEF(ZUNDEF)
    !
    IF (.NOT. LMNH_COMPATIBLE) THEN
      CALL FMWRITX1(CFILEOUT_LFI,HREC,CLUOUT_LFI,NFULL,ZWORK,4,100,HCOMMENT,KRESP)
      CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
    ELSE
      !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)  
!$OMP DO PRIVATE(JJ,JI)
      DO JJ=1,NJE-NJB+1
        DO JI=1,NIE-NIB+1
          ZWORK2D(NIB+JI-1,NJB+JJ-1) = ZWORK(JI+(NIE-NIB+1)*(JJ-1))
        END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL      
      !
      ZWORK2D(1:NIB-1,:) = ZUNDEF
      ZWORK2D(:,NJE+1:NJU) = ZUNDEF
      ZWORK2D(:,1:NJB-1) = ZUNDEF
      ZWORK2D(NIE+1:NIU,:) = ZUNDEF      
      !
      IF     (HREC=='DX              ' .OR. HREC=='XX              ') THEN
        YREC = 'XHAT'
        CALL WRITE_IN_LFI_X1_FOR_MNH(HREC,YREC,ZWORK2D(NIB:NIE,NJB),KRESP,HCOMMENT,NIU,NIB,NIE)
      ELSEIF (HREC=='DY              ' .OR. HREC=='YY              ') THEN
        YREC = 'YHAT'
        CALL WRITE_IN_LFI_X1_FOR_MNH(HREC,YREC,ZWORK2D(NIB,NJB:NJE),KRESP,HCOMMENT,NJU,NJB,NJE)
      ELSEIF (NJB==NJE) THEN
         YREC = HREC
        CALL WRITE_IN_LFI_X1_FOR_MNH(HREC,YREC,ZWORK2D(:,NJB),KRESP,HCOMMENT,NIU,NIB,NIE)
      ELSEIF (NIB==NIE) THEN
        YREC = HREC
        CALL WRITE_IN_LFI_X1_FOR_MNH(HREC,YREC,ZWORK2D(NIB,:),KRESP,HCOMMENT,NJU,NJB,NJE)
      ELSE
        CALL FMWRITX2(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK2D),ZWORK2D,4,100,HCOMMENT,KRESP)
        CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
      ENDIF
      !
    END IF
    !
  ELSE
    CALL FMWRITX1(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(PFIELD),PFIELD,4,100,HCOMMENT,KRESP)
    CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  END IF
  ! 
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI',1,ZHOOK_HANDLE)
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_IN_LFI_X1_FOR_MNH(HREC,HREC2,PFIELD,KRESP,HCOMMENT,KU,KB,KE)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
 CHARACTER(LEN=20),        INTENT(IN) :: HREC2    ! name of the article to be read
REAL, DIMENSION(:),       INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
INTEGER,                  INTENT(IN) :: KU
INTEGER,                  INTENT(IN) :: KB
INTEGER,                  INTENT(IN) :: KE
!
!*      0.2   Declarations of local variables
! 
REAL, DIMENSION(KU)      :: ZWORK ! 1D work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI:WRITE_IN_LFI_X1_FOR_MNH',0,ZHOOK_HANDLE)
!
ZWORK(:) = 0.
!
SELECT CASE(HREC)
  !
  CASE('DX              ','DY              ')
    IF (KB/=KE) THEN
      IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI:WRITE_IN_LFI_X1_FOR_MNH',1,ZHOOK_HANDLE)
      RETURN
    ENDIF
    ZWORK(1) = - PFIELD(1)*0.5  ! 1D case
    ZWORK(2) =   PFIELD(1)*0.5
    ZWORK(3) =   PFIELD(1)*1.5
  !
  CASE('XX              ','YY              ')
    IF (KB==KE) THEN
      IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI:WRITE_IN_LFI_X1_FOR_MNH',1,ZHOOK_HANDLE)
      RETURN
    ENDIF          
    ZWORK(KB+1:KE)   = 0.5 * PFIELD(1:KE-2) + 0.5 * PFIELD(2:KE-1)
    ZWORK(KB)        = 1.5 * PFIELD(1)      - 0.5 * PFIELD(2)
    ZWORK(KB-1)      = 2. * ZWORK(KB) - ZWORK(KB+1)
    ZWORK(KE+1)      = 2. * ZWORK(KE) - ZWORK(KE-1)
  CASE DEFAULT
    ZWORK(:) = PFIELD(:)
  !  
END SELECT
!
 CALL FMWRITX1(CFILEOUT_LFI,HREC2,CLUOUT_LFI,KU,ZWORK,4,100,HCOMMENT,KRESP)
 CALL ERROR_WRITE_SURF_LFI(HREC2,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX1_LFI:WRITE_IN_LFI_X1_FOR_MNH',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_IN_LFI_X1_FOR_MNH
!
END SUBROUTINE WRITE_SURFX1_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_LFI (HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI, NMASK, NFULL, &
                                    LMNH_COMPATIBLE, NIU, NIB, NIE, NJU, NJB, NJE
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_GET_SURF_UNDEF
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
REAL, DIMENSION(:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
DOUBLE PRECISION :: XTIME0
REAL             :: ZUNDEF  ! default value
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2)) :: ZWORK   ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX2_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX2_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (HDIR=='H') CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  IF (HDIR=='H') THEN
    !
    CALL GET_SURF_UNDEF(ZUNDEF)
    !
    IF (.NOT. LMNH_COMPATIBLE) THEN
      CALL FMWRITX2(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK),ZWORK,4,100,HCOMMENT,KRESP)
      CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
    ELSE
      CALL WRITE_IN_LFI_X2_FOR_MNH(HREC,ZWORK,KRESP,HCOMMENT)
    END IF
    !
  ELSE
    CALL FMWRITX2(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(PFIELD),PFIELD,4,100,HCOMMENT,KRESP)
    CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  END IF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
!if (HREC=='ALBVIS_ISBA') stop
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX2_LFI',1,ZHOOK_HANDLE)
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_IN_LFI_X2_FOR_MNH(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
! 
INTEGER :: JI, JJ
REAL    :: ZUNDEF
REAL, DIMENSION(NIU,NJU,SIZE(PFIELD,2)) :: ZWORK3D ! work array read in a MNH file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX2_LFI:WRITE_IN_LFI_X2_FOR_MNH',0,ZHOOK_HANDLE)
!
 CALL GET_SURF_UNDEF(ZUNDEF)
!
ZWORK3D=ZUNDEF
DO JJ=1,NJE-NJB+1
  DO JI=1,NIE-NIB+1
    ZWORK3D(NIB+JI-1,NJB+JJ-1,:) = PFIELD(JI+(NIE-NIB+1)*(JJ-1),:)
  END DO
END DO
!
IF (NJE==NJB) THEN
  CALL FMWRITX2(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK3D,3)*NIU,ZWORK3D(:,NJE,:),4,100,HCOMMENT,KRESP)
ELSEIF (NIE==NIB) THEN
  CALL FMWRITX2(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK3D,3)*NJU,ZWORK3D(NIE,:,:),4,100,HCOMMENT,KRESP)
ELSE
  CALL FMWRITX3(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK3D),ZWORK3D,4,100,HCOMMENT,KRESP)
ENDIF
!  
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX2_LFI:WRITE_IN_LFI_X2_FOR_MNH',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_IN_LFI_X2_FOR_MNH
!
END SUBROUTINE WRITE_SURFX2_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFX3_LFI (HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI, NMASK, NFULL, &
                                    LMNH_COMPATIBLE, NIU, NIB, NIE, NJU, NJB, NJE
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_GET_SURF_UNDEF
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
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
DOUBLE PRECISION :: XTIME0
REAL             :: ZUNDEF  ! default value
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2),SIZE(PFIELD,3)) :: ZWORK   ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX3_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX3_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (HDIR=='H') CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  IF (HDIR=='H') THEN
    !
    CALL GET_SURF_UNDEF(ZUNDEF)
    !
    IF (.NOT. LMNH_COMPATIBLE) THEN
      CALL FMWRITX3(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK),ZWORK,4,100,HCOMMENT,KRESP)
      CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
    ELSE
      CALL WRITE_IN_LFI_X3_FOR_MNH(HREC,ZWORK,KRESP,HCOMMENT)
    END IF
    !
  ELSE
    CALL FMWRITX3(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(PFIELD),PFIELD,4,100,HCOMMENT,KRESP)
    CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  END IF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
!if (HREC=='ALBVIS_ISBA') stop
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX3_LFI',1,ZHOOK_HANDLE)
!
CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_IN_LFI_X3_FOR_MNH(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
! 
INTEGER :: JI, JJ
REAL    :: ZUNDEF
REAL, DIMENSION(NIU,NJU,SIZE(PFIELD,2),SIZE(PFIELD,3)) :: ZWORK4D ! work array read in a MNH file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX3_LFI:WRITE_IN_LFI_X3_FOR_MNH',0,ZHOOK_HANDLE)
!
 CALL GET_SURF_UNDEF(ZUNDEF)
!
ZWORK4D=ZUNDEF
DO JJ=1,NJE-NJB+1
  DO JI=1,NIE-NIB+1
    ZWORK4D(NIB+JI-1,NJB+JJ-1,:,:) = PFIELD(JI+(NIE-NIB+1)*(JJ-1),:,:)
  END DO
END DO
!
IF (NJE==NJB) THEN
  CALL FMWRITX3(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK4D,3)*NIU,ZWORK4D(:,NJE,:,:),4,100,HCOMMENT,KRESP)
ELSEIF (NIE==NIB) THEN
  CALL FMWRITX3(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK4D,3)*NJU,ZWORK4D(NIE,:,:,:),4,100,HCOMMENT,KRESP)
ELSE
  !CALL FMWRITX4(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(ZWORK3D),ZWORK4D,4,100,HCOMMENT,KRESP)
  CALL ABOR1_SFX("WRITE_SURFX3_LFI: NOT POSSIBLE TO WRITE 4D FIELDS IN LFI")
ENDIF
!  
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFX3_LFI:WRITE_IN_LFI_X3_FOR_MNH',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_IN_LFI_X3_FOR_MNH
!
END SUBROUTINE WRITE_SURFX3_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_LFI (&
                                   HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write an integer array
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI, ONLY : CFILEOUT_LFI, CLUOUT_LFI, NMASK, NFULL
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
INTEGER, DIMENSION(:),  INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
INTEGER, DIMENSION(MAX(NFULL,SIZE(KFIELD))) :: IWORK  ! work array read in the file
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN1_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN1_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (HDIR=='H') CALL GATHER_AND_WRITE_MPI(KFIELD,IWORK,NMASK)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  IF (HDIR=='H') THEN
    CALL FMWRITN1(CFILEOUT_LFI,HREC,CLUOUT_LFI,NFULL,IWORK,4,100,HCOMMENT,KRESP)
  ELSE
    CALL FMWRITN1(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(KFIELD),KFIELD,4,100,HCOMMENT,KRESP)
  END IF
  !  
  CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !   
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFN1_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN1_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_LFI (&
                                   HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write a logical array
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_GET_LUOUT
USE MODI_FMWRIT
USE MODI_ABOR1_SFX
USE MODI_ERROR_WRITE_SURF_LFI
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
LOGICAL, DIMENSION(:),  INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
INTEGER         :: ILUOUT ! listing logical unit
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL1_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!  
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL1_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  IF (HDIR=='H') THEN
    CALL GET_LUOUT('LFI   ',ILUOUT)
    WRITE(ILUOUT,*) 'Error: 1D logical vector for writing on an horizontal grid:'
    WRITE(ILUOUT,*) 'this option is not coded in WRITE_SURFL1_LFI'
    CALL ABOR1_SFX('MODE_WRITE_SURF_LFI: 1D LOGICAL VECTOR FOR WRITING NOT CODED IN WRITE_SURFL1_LFI')
  ELSE
    !
    CALL FMWRITL1(CFILEOUT_LFI,HREC,CLUOUT_LFI,SIZE(OFIELD),OFIELD,4,100,HCOMMENT,KRESP)
    !
    CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  END IF
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFL1_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL1_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_LFI (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_GET_SURF_UNDEF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
 CHARACTER(LEN=12)     :: YREC     ! Name of the article to be written
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT0_LFI',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',GFOUND)
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT0_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
ITDATE(1) = KYEAR
ITDATE(2) = KMONTH
ITDATE(3) = KDAY
!
YREC=TRIM(HREC)//'%TDATE'
 CALL FMWRITN1(CFILEOUT_LFI,YREC,CLUOUT_LFI,3,ITDATE,4,100,HCOMMENT,KRESP)
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
YREC=TRIM(HREC)//'%TIME'
 CALL FMWRITX0(CFILEOUT_LFI,YREC,CLUOUT_LFI,1,PTIME,4,100,HCOMMENT,KRESP)
 CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT0_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFT1_LFI (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
 CHARACTER(LEN=12),    INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(IN) :: KYEAR    ! year
INTEGER, DIMENSION(:), INTENT(IN) :: KMONTH   ! month
INTEGER, DIMENSION(:), INTENT(IN) :: KDAY     ! day
REAL,    DIMENSION(:), INTENT(IN) :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=12) :: YREC     ! Name of the article to be written
INTEGER, DIMENSION(3,SIZE(KYEAR)) :: ITDATE
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT1_LFI',0,ZHOOK_HANDLE)
!
KRESP = 0
!
CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT1_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  KRESP=0
  !
  ITDATE(1,:) = KYEAR (:)
  ITDATE(2,:) = KMONTH(:)
  ITDATE(3,:) = KDAY  (:)
  !
  YREC=TRIM(HREC)//'%TDATE'
  CALL FMWRITN2(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(ITDATE),ITDATE,4,100,HCOMMENT,KRESP)
  !
  YREC=TRIM(HREC)//'%TIME'
  CALL FMWRITX1(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(PTIME),PTIME,4,100,HCOMMENT,KRESP)
  !
  CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT1_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT1_LFI
!
!     #############################################################
      SUBROUTINE WRITE_SURFT2_LFI (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI
!
USE MODI_IO_BUFF
USE MODI_FMWRIT
USE MODI_ERROR_WRITE_SURF_LFI
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
 CHARACTER(LEN=12),    INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(IN) :: KYEAR    ! year
INTEGER, DIMENSION(:,:), INTENT(IN) :: KMONTH   ! month
INTEGER, DIMENSION(:,:), INTENT(IN) :: KDAY     ! day
REAL,    DIMENSION(:,:), INTENT(IN) :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
LOGICAL :: GFOUND
 CHARACTER(LEN=12) :: YREC     ! Name of the article to be written
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT2_LFI',0,ZHOOK_HANDLE)
!
KRESP = 0
!
CALL IO_BUFF(&
                HREC,'W',GFOUND)
!
IF (GFOUND .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT2_LFI',1,ZHOOK_HANDLE)
IF (GFOUND) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !  
  KRESP=0
  !
  YREC=TRIM(HREC)//'%YEAR'
  CALL FMWRITN2(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(KYEAR),KYEAR,4,100,HCOMMENT,KRESP)
  !
  YREC=TRIM(HREC)//'%MONTH'
  CALL FMWRITN2(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(KMONTH),KMONTH,4,100,HCOMMENT,KRESP)
  !
  YREC=TRIM(HREC)//'%DAY'
  CALL FMWRITN2(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(KDAY),KDAY,4,100,HCOMMENT,KRESP)  
  !
  YREC=TRIM(HREC)//'%TIME'
  CALL FMWRITX2(CFILEOUT_LFI,YREC,CLUOUT_LFI,SIZE(PTIME),PTIME,4,100,HCOMMENT,KRESP)
  !
  CALL ERROR_WRITE_SURF_LFI(HREC,KRESP)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_LFI:WRITE_SURFT2_LFI',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT2_LFI
!
#endif
!
END MODULE MODE_WRITE_SURF_LFI
!

