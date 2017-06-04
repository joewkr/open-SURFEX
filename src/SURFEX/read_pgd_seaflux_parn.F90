!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_SEAFLUX_PAR_n (DTCO, U, GCP, DTS, KDIM, HPROGRAM,KSIZE,HDIR)
!     ################################################
!
!!****  *READ_PGD_SEAFLUX_PAR_n* - reads SEAFLUX sst
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_PREP,             ONLY : LINTERP
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_HOR_INTERPOL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
INTEGER, INTENT(IN) :: KSIZE
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR       ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDATA_SST
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=1)  :: YDIR
INTEGER           :: ILUOUT
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER           :: JTIME          ! loop index
INTEGER           :: IVERSION, IBUGFIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_PAR_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<4 .OR. IVERSION==4 .AND. IBUGFIX<=4 .OR. &
    IVERSION>5 .OR. IVERSION==5 .AND. IBUGFIX>=1) THEN
  YRECFM='ND_SEA_TIME'
ELSE
  YRECFM='NDATA_SEA_TIME'
ENDIF
 CALL READ_SURF(HPROGRAM,YRECFM,DTS%NTIME,IRESP,HCOMMENT=YCOMMENT)
!
ALLOCATE(ZDATA_SST (KSIZE,DTS%NTIME))
DO JTIME=1,DTS%NTIME
  !
  IF (IVERSION>5 .OR. IVERSION==5 .AND. IBUGFIX>=1) THEN
    WRITE(YRECFM,FMT='(A7,I3.3)') 'D_SST_T',JTIME
  ELSEIF (IVERSION<4 .OR. IVERSION==4 .AND. IBUGFIX<=4) THEN
    WRITE(YRECFM,FMT='(A9,I3.3)') 'DATA_SST_',JTIME
  ELSE
    WRITE(YRECFM,FMT='(A10,I3.3)') 'DATA_SST_T',JTIME
  ENDIF
  !
  CALL READ_SURF(HPROGRAM,YRECFM,ZDATA_SST(:,JTIME),IRESP,&
                 HCOMMENT=YCOMMENT,HDIR=YDIR)
  !
END DO
!
ALLOCATE(DTS%XDATA_SST(KDIM,DTS%NTIME))
IF (KDIM/=KSIZE) THEN
  LINTERP(:) = .TRUE.      
  DO JTIME=1,DTS%NTIME
    CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZDATA_SST(:,JTIME:JTIME),DTS%XDATA_SST(:,JTIME:JTIME))
  ENDDO
  DEALLOCATE(ZDATA_SST)
ELSE
  DTS%XDATA_SST(:,:) = ZDATA_SST(:,:)
ENDIF
!
ALLOCATE(DTS%TDATA_SST       (DTS%NTIME))
!
IF (IVERSION<4 .OR. IVERSION==4 .AND. IBUGFIX<=4) THEN
  DO JTIME=1,DTS%NTIME
    WRITE(YRECFM,FMT='(A7,I3.3)') 'DTA_SST',JTIME
    YCOMMENT='(-)'
    CALL READ_SURF(HPROGRAM,YRECFM,DTS%TDATA_SST,IRESP,HCOMMENT=YCOMMENT)
  END DO
ELSE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='TD_SST'
  ELSE
    YRECFM='TDATA_SST'
  ENDIF
  YCOMMENT='(-)'
  CALL READ_SURF(HPROGRAM,YRECFM,DTS%TDATA_SST,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_PAR_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_PGD_SEAFLUX_PAR_n
