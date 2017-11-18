!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #######################
MODULE MODI_READ_SURF_ISBA_PAR_n
CONTAINS
      SUBROUTINE READ_SURF_ISBA_PAR_n (DTCO, U, GCP, KPATCH, HPROGRAM, HREC, KLUOUT, KSIZE, &
                                       KVERSION, KBUGFIX, ODATA, PFIELD, KRESP, HCOMMENT, HDIR)
!     #######################
!
!!    MODIFICATIONS
!!    -------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_READ_SURF
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_TO_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER, INTENT(IN) :: KPATCH
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*),        INTENT(IN) :: HREC   ! name of the article to be read
!
INTEGER,                 INTENT(IN) :: KLUOUT
INTEGER,                 INTENT(IN) :: KSIZE
INTEGER,                 INTENT(IN) :: KVERSION
INTEGER,                 INTENT(IN) :: KBUGFIX
LOGICAL, DIMENSION(:),   INTENT(INOUT) :: ODATA
!
REAL, DIMENSION(:,:),    INTENT(OUT):: PFIELD ! array containing the data field

INTEGER                  ,INTENT(OUT) :: KRESP      ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR       ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YREC
 CHARACTER(LEN=3) :: YVEG
REAL, DIMENSION(KSIZE, NVEGTYPE)  :: ZFIELD
REAL, DIMENSION(SIZE(PFIELD,1),1,KPATCH) :: ZFIELD_PATCH
REAL, DIMENSION(SIZE(PFIELD,1),1,NVEGTYPE) :: ZFIELD_VEGTYPE
 CHARACTER(LEN=1)   :: YDIR
INTEGER :: INI, JP, IPATCH, JV, JV2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
INI = SIZE(PFIELD,1)
!
ZFIELD(:,:) = 0.
!
IF (KVERSION<7) THEN
  !
  ! fields were written by patch
  CALL READ_SURF(HPROGRAM,HREC,ZFIELD(:,1:KPATCH),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
  ! case zoom
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELD(:,1:KPATCH),PFIELD(:,1:KPATCH))
  ELSE
    ! classical case
    PFIELD(:,1:KPATCH) = ZFIELD(:,1:KPATCH)
  ENDIF
  !
  ! classical case
  IF (SIZE(PFIELD,2)==NVEGTYPE) THEN
    DO JP = 1, KPATCH
      ZFIELD_PATCH(:,1,JP) = PFIELD(:,JP)
    ENDDO
    ! patchs shared on vegtypes
    CALL PUT_ON_ALL_VEGTYPES(INI,1,KPATCH,NVEGTYPE,ZFIELD_PATCH,ZFIELD_VEGTYPE)
    PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
  ENDIF
  !
ELSE
  !
  IF (KVERSION>8 .OR. (KVERSION==8 .AND. KBUGFIX>=1)) THEN
    !
    DO JV = 1,NVEGTYPE
      IF (ODATA(JV)) THEN
        WRITE(YVEG,FMT='(A1,I2.2)') 'V',JV
        YREC = TRIM(ADJUSTL(HREC))//YVEG
        CALL READ_SURF(HPROGRAM,YREC,ZFIELD(:,JV),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
      ELSE

        IF (HREC(1:3)=='LAI'.OR.HREC(1:10)=='ALBNIR_VEG'.OR.HREC(1:10)=='ALBVIS_VEG' &
               .OR. HREC(1:6)=='H_TREE') THEN
          IF (JV<=3) ZFIELD(:,JV) = 0.
          IF (HREC(1:6)=='H_TREE'.AND.((JV>=7.AND.JV<=12).OR.JV>=18)) ZFIELD(:,JV) = 0.
          ODATA(JV) = .TRUE.
        ENDIF

        IF (.NOT.ODATA(JV)) THEN
          DO JV2=JV,1,-1
            IF (ODATA(JV2)) THEN
              ZFIELD(:,JV) = ZFIELD(:,JV2)
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO
    !
  ELSE
    !
    ! field written by vegtype
    CALL READ_SURF(HPROGRAM,HREC,ZFIELD(:,:),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
    !
  ENDIF
  !
  ! case zoom
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELD(:,:),ZFIELD_VEGTYPE(:,1,:))
  ELSE
    ! classical case
    ZFIELD_VEGTYPE(:,1,:) = ZFIELD(:,:)
  ENDIF
  !
  ! case mode_read_extern
  IF (SIZE(PFIELD,2).NE.NVEGTYPE) THEN
    IPATCH = SIZE(PFIELD,2)
    PFIELD(:,:) = 0.
    DO JV = 1, NVEGTYPE
      JP = VEGTYPE_TO_PATCH(JV,IPATCH)
      ! artefact to simplify in mode_read_extern: we take the upper value
      PFIELD(:,JP) = MAX(PFIELD(:,JP),ZFIELD_VEGTYPE(:,1,JV))
    ENDDO
  ELSE
    ! classical case
    PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
  ENDIF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE READ_SURF_ISBA_PAR_n
END MODULE MODI_READ_SURF_ISBA_PAR_n
