!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######
SUBROUTINE RESTART_COUPL_TOPD (UG, KR_NATURE, HPROGRAM,KI)
!###################################################################
!
!!****  *RESTART_COUPL_TOPDn*  
!!
!!    PURPOSE
!!    -------
!!  Read all files needed in case of restart 
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Vincendon
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/06/11 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF,NUNDEF
USE MODD_ISBA_PAR,        ONLY : XWGMIN
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN,       ONLY : NNCAT, CCAT, NNPT, NLINE, NNMC, NPMAX,&
                               NNB_TOPD_STEP

USE MODD_COUPLING_TOPD, ONLY : XAS_NATURE, &
                                 NNB_STP_STOCK,NNB_STP_RESTART,XWTOPT,&
                                 XRUN_TOROUT,XDR_TOROUT
!
USE MODI_READ_TOPD_FILE
USE MODI_READ_FILE_ISBAMAP
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_UNPACK_SAME_RANK
USE MODI_PACK_SAME_RANK
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER, DIMENSION(:), INTENT(IN) :: KR_NATURE
!
 CHARACTER(LEN=6), INTENT(IN)         :: HPROGRAM ! program calling surf. schemes
INTEGER,          INTENT(IN)         :: KI       ! Surfex grid dimension
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JJ
INTEGER                                     :: ILUOUT   ! unit of output listing file
INTEGER                                     :: JSTP,JCAT,JPIX! loop control indexes
REAL, DIMENSION(:),ALLOCATABLE              :: ZAS      ! Saturated area fraction for each Isba meshes
REAL, DIMENSION(:),ALLOCATABLE              :: ZWTOPT   ! Initial water content in case of restart
 CHARACTER(LEN=50), DIMENSION(:),ALLOCATABLE :: YFILETOP ! File names
LOGICAL                                     :: LSTOCK, LWG, LASAT
REAL                                        :: ZCORR_STOCK ! used to avoid to lose stock
REAL                                        :: ZCNT_UNDEF,ZSUM1,ZSUM2, ZDENOM ! used to correct budget
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('RESTART_COUPL_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
! * 1. Read stock files
!       
WRITE(*,*) 'Read STOCK file ',NNB_STP_STOCK
NNB_STP_STOCK = MIN(NNB_STP_STOCK, NNB_TOPD_STEP + NNB_STP_RESTART)
!
INQUIRE(FILE='stocks_init.txt', EXIST=LSTOCK)
INQUIRE(FILE='surfcont_init.map', EXIST=LASAT)
!
IF (.NOT.LSTOCK) THEN
  WRITE(ILUOUT,*) 'You asked to run in restart mode but stock file is missing'
  CALL ABOR1_SFX("RESTART_COUPL_TOPD_n: stock file is missing")
ELSEIF (.NOT.LASAT) THEN
  WRITE(ILUOUT,*) 'You asked to run in restart mode but contributive area file is missing'
  CALL ABOR1_SFX("RESTART_COUPL_TOPD_n: contributive area file is missing")
ELSE
  !
  CALL OPEN_FILE('ASCII ',NUNIT,'stocks_init.txt','FORMATTED',HACTION='READ ')
  DO JSTP=1,NNB_STP_STOCK
    READ(NUNIT,*)  XRUN_TOROUT(1:NNCAT,JSTP),XDR_TOROUT(1:NNCAT,JSTP)
  ENDDO
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !  
  ! * 2. Read pixels water content
  !
  DO JCAT=1,NNCAT
    !
    IF (XRUN_TOROUT(JCAT,NNB_STP_STOCK)/=0.) THEN
      !
      ZCORR_STOCK = XRUN_TOROUT(JCAT,NNB_STP_STOCK) - XRUN_TOROUT(JCAT,NNB_STP_STOCK-1)
      DO JSTP = NNB_STP_STOCK+1,NNB_TOPD_STEP
        XRUN_TOROUT(JCAT,JSTP) = MAX(0.,XRUN_TOROUT(JCAT,JSTP-1)+ZCORR_STOCK)
      ENDDO
      !
    ENDIF
    !
    IF (XDR_TOROUT(JCAT,NNB_STP_STOCK)/=0.) THEN
      !
      ZCORR_STOCK = XDR_TOROUT(JCAT,NNB_STP_STOCK) - XDR_TOROUT(JCAT,NNB_STP_STOCK-1)
      DO JSTP = NNB_STP_STOCK+1,NNB_TOPD_STEP
        XDR_TOROUT(JCAT,JSTP) = MAX(0.,XDR_TOROUT(JCAT,JSTP-1)+ZCORR_STOCK)
      ENDDO
      !
    ENDIF
    !
  ENDDO
  !
  WRITE(*,*) 'Write pixels water content files'
  !
  ALLOCATE(ZWTOPT(NPMAX))
  ALLOCATE(YFILETOP(NNCAT))
  !
  DO JCAT=1,NNCAT
    !
    YFILETOP(JCAT)=TRIM(CCAT(JCAT))//'_xwtop_init.map'
    INQUIRE(FILE=YFILETOP(JCAT), EXIST=LWG)
    IF (.NOT.LWG) THEN
      !
      WRITE(ILUOUT,*) 'You asked to run in restart mode but pixels water content file is missing'
      WRITE(ILUOUT,*) 'for catchment : ',CCAT(JCAT)
      CALL ABOR1_SFX("RESTART_COUPL_TOPD_n: pixels water content file is missing")
      !
    ELSE
      !
      ZSUM1=SUM(XWTOPT(JCAT,:),MASK=XWTOPT(JCAT,:)/=XUNDEF)
      !
      CALL READ_TOPD_FILE('ASCII ',YFILETOP(JCAT),'FORMATTED',NNPT(JCAT),ZWTOPT)
      !
      DO JPIX=1,SIZE(NLINE(JCAT,:))
        IF ( NLINE(JCAT,JPIX)/=0 .AND. NLINE(JCAT,JPIX)/=XUNDEF ) THEN
          IF (ZWTOPT(JPIX) /= XUNDEF) XWTOPT(JCAT,NLINE(JCAT,JPIX)) = ZWTOPT(JPIX)
        ENDIF
      ENDDO
      !
      ZSUM2=SUM(XWTOPT(JCAT,:),MASK=XWTOPT(JCAT,:)<XUNDEF)
      !
      IF ( ABS(ZSUM2-ZSUM1)>100. ) THEN
        !
        ZCNT_UNDEF = 0.
        DO JJ=1,SIZE(NLINE,2)
          IF ( NLINE(JCAT,JJ)/=0 ) THEN
            IF ( XWTOPT(JCAT,NLINE(JCAT,JJ))/=XUNDEF ) ZCNT_UNDEF = ZCNT_UNDEF + 1.
          ENDIF
        ENDDO
        !
        IF (ZCNT_UNDEF/=0.) THEN
          ZDENOM = (ZSUM2-ZSUM1)/ZCNT_UNDEF
          DO JJ=1,SIZE(NLINE,2)
            IF ( NLINE(JCAT,JJ)/=0 ) THEN
              IF ( XWTOPT(JCAT,NLINE(JCAT,JJ))/=XUNDEF ) THEN
                XWTOPT(JCAT,NLINE(JCAT,JJ)) = XWTOPT(JCAT,NLINE(JCAT,JJ)) - ZDENOM
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        ZSUM2=SUM(XWTOPT(JCAT,:),MASK=XWTOPT(JCAT,:)<XUNDEF)
        !
      ENDIF
      !
    ENDIF
    !
  ENDDO
  ! 
  ! * 3. Read Asat files
  ! 
  WRITE(*,*) 'Read Asat files'
  ALLOCATE(ZAS(KI))
  CALL OPEN_FILE('ASCII ',NUNIT,'surfcont_init.map','FORMATTED',HACTION='READ ')
  CALL READ_FILE_ISBAMAP(UG, NUNIT,ZAS,KI)
  CALL CLOSE_FILE('ASCII ',NUNIT)
  CALL PACK_SAME_RANK(KR_NATURE,ZAS,XAS_NATURE)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('RESTART_COUPL_TOPD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE RESTART_COUPL_TOPD
