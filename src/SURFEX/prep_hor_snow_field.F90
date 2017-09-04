!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_SNOW_FIELD (DTCO, G, U, GCP, HPROGRAM,      &
                                HFILE,HFILETYPE,                &
                                HFILEPGD,HFILEPGDTYPE,          &
                                KLUOUT,OUNIF,HSNSURF,KPATCH,    &
                                KTEB_PATCH,                     &
                                KL,TNPSNOW, TPTIME,             &
                                PUNIF_WSNOW, PUNIF_RSNOW,       &
                                PUNIF_TSNOW, PUNIF_LWCSNOW,     &
                                PUNIF_ASNOW, OSNOW_IDEAL,       &
                                PUNIF_SG1SNOW, PUNIF_SG2SNOW,   &
                                PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,   &
                                PVEGTYPE_PATCH, PPATCH,         &
                                KSIZE_P, KR_P, PDEPTH  )
!     #######################################################
!
!!****  *PREP_HOR_SNOW_FIELD* - reads, interpolates and prepares a snow field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!     V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      B. Decharme  10/2013, Phasage Arpège-Climat
!!      M. Lafaysse 11/2012, snow liquid water content
!!      B. Decharme  04/2014, external init with FA files
!!                            new init for ES
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TYPE_SNOW
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODD_GRID_GRIB, ONLY : CINMODEL
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_SNOW
USE MODD_PREP,           ONLY : LINTERP,CINTERP_TYPE,CINGRID_TYPE, CMASK
!
USE MODD_SNOW_PAR, ONLY : XANSMAX
!
USE MODI_PREP_GRIB_GRID
USE MODI_PREP_SNOW_GRIB
USE MODI_PREP_SNOW_UNIF
USE MODI_PREP_SNOW_EXTERN
USE MODI_PREP_SNOW_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_VEGTYPE_TO_PATCH
USE MODI_PACK_SAME_RANK
USE MODI_GET_PREP_INTERP
!
USE MODE_SNOW3L, ONLY : SNOW3LGRID
!
USE MODI_ABOR1_SFX
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
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT (INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
 CHARACTER(LEN=10)               :: HSNSURF   ! type of field
INTEGER,            INTENT(IN)  :: KPATCH    ! patch number for output scheme
INTEGER,            INTENT(IN) :: KTEB_PATCH
TYPE(NSURF_SNOW), INTENT(INOUT) :: TNPSNOW    ! snow fields
INTEGER,            INTENT(IN)  :: KL        ! number of points
TYPE(DATE_TIME),    INTENT(IN)  :: TPTIME    ! date and time
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_WSNOW ! prescribed snow content (kg/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_RSNOW ! prescribed density (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_TSNOW ! prescribed temperature (K)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_LWCSNOW ! prescribed snow liquid water content (kg/m3)
REAL,               INTENT(IN)  :: PUNIF_ASNOW ! prescribed albedo (-)
LOGICAL,            INTENT(INOUT)  :: OSNOW_IDEAL
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG1SNOW !
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG2SNOW !
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_HISTSNOW !
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_AGESNOW !
!
REAL,DIMENSION(:,:,:),  INTENT(IN) :: PVEGTYPE_PATCH ! fraction of each vegtype per patch
REAL,DIMENSION(:,:),  INTENT(IN) :: PPATCH ! fraction of each patch
INTEGER, DIMENSION(:), INTENT(IN) :: KSIZE_P
INTEGER,DIMENSION(:,:),  INTENT(IN) :: KR_P
!
REAL,DIMENSION(:,:,:),INTENT(IN), OPTIONAL :: PDEPTH ! thickness of each snow layer
!
!*      0.2    declarations of local variables
!
TYPE FOUT
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT
END TYPE FOUT
TYPE NFOUT
  TYPE(FOUT), DIMENSION(:), ALLOCATABLE :: AL
END TYPE NFOUT
TYPE (NFOUT) :: ZW
TYPE(SURF_SNOW), POINTER :: SK
!
REAL, POINTER, DIMENSION(:,:,:)     :: ZFIELDIN   ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:,:) :: ZFIELDOUTP ! field interpolated   horizontally
REAL, POINTER, DIMENSION(:,:,:) :: ZFIELDOUTV !
REAL, ALLOCATABLE, DIMENSION(:)   :: ZD        ! snow depth (x, kpatch)
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZHEAT     ! work array (x, output snow grid, kpatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZGRID     ! grid array (x, output snow grid, kpatch)
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDEPTH, ZPATCH
!
TYPE (DATE_TIME)              :: TZTIME_GRIB    ! current date and time
INTEGER                       :: JP, IP    ! loop on patches
INTEGER                       :: JL    ! loop on layers
INTEGER :: INFOMPI, INL, INP, ISNOW_NLAYER, IMASK, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*      1.     Does the field exist?
!
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELD',0,ZHOOK_HANDLE)
!
ISNOW_NLAYER = TNPSNOW%AL(1)%NLAYER
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
NULLIFY (ZFIELDIN, ZFIELDOUTP, ZFIELDOUTV)
!
IF (YDCTL%LPART1) THEN
  IF (OUNIF) THEN
    CALL PREP_SNOW_UNIF(KLUOUT,HSNSURF,ZFIELDIN, TPTIME, OSNOW_IDEAL,       &
                        PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW,              &
                        PUNIF_LWCSNOW, PUNIF_ASNOW, PUNIF_SG1SNOW,          &
                        PUNIF_SG2SNOW, PUNIF_HISTSNOW, PUNIF_AGESNOW,       &
                        ISNOW_NLAYER                                     )
  ELSE IF (HFILETYPE=='GRIB  ') THEN
    CALL PREP_GRIB_GRID(HFILE,KLUOUT,CINMODEL,CINGRID_TYPE,CINTERP_TYPE,TZTIME_GRIB)
    IF (NRANK==NPIO) CALL PREP_SNOW_GRIB(HPROGRAM,HSNSURF,HFILE,KLUOUT,ISNOW_NLAYER,ZFIELDIN)
  ELSE IF (HFILETYPE=='MESONH' .OR. HFILETYPE=='ASCII ' .OR. HFILETYPE=='LFI   '&
          .OR. HFILETYPE=='FA    '.OR. HFILETYPE=='AROME ') THEN
    CALL PREP_SNOW_EXTERN(GCP,HPROGRAM,HSNSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE, &
                          KLUOUT,ZFIELDIN,OSNOW_IDEAL,ISNOW_NLAYER,KTEB_PATCH)
  ELSE IF (HFILETYPE=='BUFFER') THEN
    CALL PREP_SNOW_BUFFER(G, U, HPROGRAM,HSNSURF,KLUOUT,ISNOW_NLAYER,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_SNOW_FIELD: data file type not supported : '//HFILETYPE)
  END IF
ENDIF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      3.     Horizontal interpolation
!
CALL PREP_CTL_INT_PART2 (YDCTL, HSNSURF, 'SNOW', CMASK, ZFIELDIN)
!
IF (YDCTL%LPART3) THEN
!
  IF (NRANK==NPIO) THEN
    INL = SIZE(ZFIELDIN,2)
    INP = SIZE(ZFIELDIN,3)
  ELSEIF (.NOT.ASSOCIATED(ZFIELDIN)) THEN
   ALLOCATE(ZFIELDIN(0,0,0))
  ENDIF
!
  IF (NPROC>1) THEN
#ifdef SFX_MPI
    CALL MPI_BCAST(INL,KIND(INL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(INP,KIND(INP)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
  ENDIF
!
  ALLOCATE(ZFIELDOUTP(KL,INL,INP))
!
! ZPATCH is the array of output patches put on the input patches
  ALLOCATE(ZPATCH(KL,INP))
  ZPATCH(:,:) = 0.
!
! if the number of input patches is NVEGTYPE
  IF (INP==NVEGTYPE) THEN
    DO JP = 1,NVEGTYPE
    ! each vegtype takes the output contribution of the patch it is in
      IP = VEGTYPE_TO_PATCH(JP,KPATCH)
      ZPATCH(:,JP) = PVEGTYPE_PATCH(:,JP,IP)
    ENDDO
  ENDIF
!
  CALL GET_PREP_INTERP(INP,KPATCH,ZPATCH,PPATCH,ZPATCH,KR_P)
!
! the same for depth that is defined on the output patches
  IF (PRESENT(PDEPTH)) THEN
  !
    ALLOCATE(ZDEPTH(KL,INP))
    ZDEPTH(:,:) = 0.
  !
    IF (INP==NVEGTYPE) THEN
      DO JP = 1,NVEGTYPE
        IP = VEGTYPE_TO_PATCH(JP,KPATCH)
        ZDEPTH(:,JP) = PDEPTH(:,1,IP)
      ENDDO
    ENDIF
    !
    CALL GET_PREP_INTERP(INP,KPATCH,ZDEPTH,PDEPTH(:,1,:),ZDEPTH,KR_P)
    !
  ENDIF
!
  DO JP = 1, INP
    ! ZDEPTH and ZPATCH are defined on the size on the patch for the snow: use of
    ! the mask
    IF (PRESENT(PDEPTH)) THEN
      LINTERP(:) = ( ZDEPTH(:,JP) /= 0. .AND. ZDEPTH(:,JP) /= XUNDEF )
    ENDIF
    LINTERP(:) = (LINTERP(:) .AND. ZPATCH(:,JP)>0.)
    !* horizontal interpolation
    CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELDIN(:,:,JP),ZFIELDOUTP(:,:,JP))
    !
    LINTERP(:) = .TRUE.
  END DO
  !
  DEALLOCATE(ZFIELDIN, ZPATCH )
  IF (PRESENT(PDEPTH)) DEALLOCATE(ZDEPTH)
  !
ENDIF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      4.     Transformation from vegtype grid to patch grid, if any
!
CALL PREP_CTL_INT_PART4 (YDCTL, HSNSURF, 'SNOW', CMASK, ZFIELDIN, ZFIELDOUTP)

IF (YDCTL%LPART5) THEN
!
  ALLOCATE(ZW%AL(KPATCH))
  !
  IF (KPATCH/=INP.and.INP/=1) THEN
    !
    ALLOCATE(ZFIELDOUTV(KL,INL,NVEGTYPE))
    CALL PUT_ON_ALL_VEGTYPES(KL,INL,INP,NVEGTYPE,ZFIELDOUTP,ZFIELDOUTV)
    !
    !*      6.     Transformation from vegtype grid to patch grid
    !
    DEALLOCATE(ZFIELDOUTP)
    !
    DO JP = 1,KPATCH
      !
      ALLOCATE(ZW%AL(JP)%ZOUT(KSIZE_P(JP),INL))
      !
      CALL VEGTYPE_GRID_TO_PATCH_GRID(JP, KPATCH, PVEGTYPE_PATCH(1:KSIZE_P(JP),:,JP),  &
                                      PPATCH(1:KSIZE_P(JP),JP), KR_P(1:KSIZE_P(JP),JP), &
                                      ZFIELDOUTV, ZW%AL(JP)%ZOUT)
    ENDDO
    !
    DEALLOCATE(ZFIELDOUTV)
    !
  ELSEIF (INP==1) THEN
    !
    DO JP = 1,KPATCH
      !
      ALLOCATE(ZW%AL(JP)%ZOUT(KSIZE_P(JP),INL))
      !
      CALL PACK_SAME_RANK(KR_P(1:KSIZE_P(JP),JP),ZFIELDOUTP(:,:,1),ZW%AL(JP)%ZOUT)
      !
    ENDDO
    !
  ELSE
    !
    DO JP = 1,KPATCH
      !
      ALLOCATE(ZW%AL(JP)%ZOUT(KSIZE_P(JP),INL))
      !
      CALL PACK_SAME_RANK(KR_P(1:KSIZE_P(JP),JP),ZFIELDOUTP(:,:,JP),ZW%AL(JP)%ZOUT)
      !
    ENDDO
    !
    DEALLOCATE(ZFIELDOUTP)
    !
  ENDIF
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  !*      5.     Defines normalized output grid, if depths of snow layers are present
  !
  IF (PRESENT(PDEPTH) .AND. .NOT.OSNOW_IDEAL) THEN
    !
    ALLOCATE(ZD(SIZE(PDEPTH,1)))
    !
    ALLOCATE(ZGRID(SIZE(PDEPTH,1),ISNOW_NLAYER,KPATCH))
    DO JP = 1,KPATCH
      !* total snow depth
      !
      ZD(:)=0.
      DO JL = 1,ISNOW_NLAYER
        WHERE (PDEPTH(1:KSIZE_P(JP),JL,JP)/=XUNDEF) ZD(1:KSIZE_P(JP)) = ZD(1:KSIZE_P(JP)) + PDEPTH(1:KSIZE_P(JP),JL,JP)
      END DO
      !
      !* grid at center of layers
      !
      ZGRID(1:KSIZE_P(JP),1,JP) = PDEPTH(1:KSIZE_P(JP),1,JP)
      IF(ISNOW_NLAYER>1) THEN
        DO JL = 2,ISNOW_NLAYER
          ZGRID(1:KSIZE_P(JP),JL,JP) = ZGRID(1:KSIZE_P(JP),JL-1,JP) + PDEPTH(1:KSIZE_P(JP),JL,JP)
        ENDDO
      ENDIF
      !
      ! * normalized grid
      !
      DO JL=1,ISNOW_NLAYER
        WHERE (ZD(1:KSIZE_P(JP))/=0.)
          ZGRID(1:KSIZE_P(JP),JL,JP) = ZGRID(1:KSIZE_P(JP),JL,JP) / ZD(1:KSIZE_P(JP))
        ELSEWHERE
          ZGRID(1:KSIZE_P(JP),JL,JP) = 1.0
        END WHERE
      END DO
      !
    ENDDO
    !
    DEALLOCATE(ZD)
    !
  ELSEIF (.NOT.OSNOW_IDEAL) THEN
    IF (HSNSURF(1:3)=='RHO' .OR. HSNSURF(1:3)=='HEA') THEN
      WRITE(KLUOUT,*) 'when interpolation profiles of snow pack quantities,'
      WRITE(KLUOUT,*) 'depth of snow layers must be given'
      CALL ABOR1_SFX('PREP_HOR_SNOW_FIELD: DEPTH OF SNOW LAYERS NEEDED')
    END IF
  END IF
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  !*      6.     Return to historical variable
  !
  DO JP = 1,KPATCH
    !
    SK => TNPSNOW%AL(JP)
    !
    SELECT CASE (HSNSURF(1:3))
      !
      CASE('WWW')  ! total snow content
        !
        IF (OSNOW_IDEAL) THEN
          SK%WSNOW(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSE
          DO JL=1,SIZE(SK%WSNOW,2)
            SK%WSNOW(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          ENDDO
        ENDIF
        !
        DO JL = 1,ISNOW_NLAYER
          WHERE(PPATCH(1:KSIZE_P(JP),JP)==0.)
            SK%WSNOW(:,JL) = XUNDEF
          END WHERE
        ENDDO
        !
      CASE('DEP')  ! snow thickness
        !
        IF (OSNOW_IDEAL.OR.ISNOW_NLAYER==INL) THEN
          SK%DEPTH(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSE
          CALL SNOW3LGRID(SK%DEPTH(:,:),ZW%AL(JP)%ZOUT(:,1))
          !DO JL=1,SIZE(SK%DEPTH,2)
          !  SK%DEPTH(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          !ENDDO
        ENDIF
        !
        DO JL = 1,ISNOW_NLAYER
          WHERE(PPATCH(1:KSIZE_P(JP),JP)==0.)
            SK%DEPTH(:,JL) = XUNDEF
          END WHERE
        ENDDO
      !
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !
      CASE('RHO')
        !
        IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
          SK%RHO(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSEIF(INL==1) THEN
          DO JL = 1,ISNOW_NLAYER
            SK%RHO(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          ENDDO
        ELSE
          !* interpolation on snow levels
          CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%RHO)
        ENDIF
        !
        !* mask for areas where there is no snow
        DO JL=1,ISNOW_NLAYER
          WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%RHO(:,JL) = XUNDEF
        END DO
        !
        !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !
      CASE('ALB')
        !
        SK%ALB(:) = ZW%AL(JP)%ZOUT(:,1)
        !
        !* mask for areas where there is no snow
        WHERE(PDEPTH(1:KSIZE_P(JP),1,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),1,JP)==XUNDEF)  SK%ALB(:) = XUNDEF
      !
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !
      CASE('HEA')
        !
        IF (SK%SCHEME=='3-L' .OR. SK%SCHEME=='CRO') THEN
          !
          IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
            SK%HEAT(:,:) = ZW%AL(JP)%ZOUT(:,:)
          ELSEIF(INL==1) THEN
            DO JL = 1,ISNOW_NLAYER
              SK%HEAT(:,JL) = ZW%AL(JP)%ZOUT(:,1)
            ENDDO
          ELSE
            !* interpolation of heat on snow levels
            CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%HEAT)
          ENDIF
          !
          !* mask for areas where there is no snow
          DO JL=1,ISNOW_NLAYER
            WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%HEAT(:,JL) = XUNDEF
          END DO
          !
        ELSE IF (SK%SCHEME=='1-L') THEN
          !* interpolation of heat on snow levels
          ALLOCATE(ZHEAT(KSIZE_P(JP),ISNOW_NLAYER))
          !
          IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
            ZHEAT(:,:) = ZW%AL(JP)%ZOUT(:,:)
          ELSEIF(INL==1) THEN
            DO JL = 1,ISNOW_NLAYER
              ZHEAT(:,JL) = ZW%AL(JP)%ZOUT(:,1)
            ENDDO
          ELSE
            CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),ZHEAT)
          ENDIF
          !
          !* transformation from heat to temperature
          CALL SNOW_HEAT_TO_T_WLIQ(ZHEAT,SK%RHO,SK%T)
          WHERE (SK%T>XTT) SK%T = XTT
          !
          DEALLOCATE(ZHEAT)
          !
          !* mask for areas where there is no snow
          DO JL=1,ISNOW_NLAYER
            WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%T(:,JL) = XUNDEF
          END DO
          !
        END IF
        !
        !
      CASE('SG1')
        !
        IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
          SK%GRAN1(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSEIF(INL==1) THEN
         DO JL = 1,ISNOW_NLAYER
             SK%GRAN1(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          ENDDO
        ELSE
          !* interpolation of heat on snow levels
          CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%GRAN1)
        ENDIF
        !
        !* mask for areas where there is no snow
        DO JL=1,ISNOW_NLAYER
          WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%GRAN1(:,JL) = XUNDEF
        END DO
        !
      CASE('SG2')
        !
        IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
          SK%GRAN2(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSEIF(SIZE(ZW%AL(JP)%ZOUT,2)==1) THEN
          DO JL = 1,ISNOW_NLAYER
            SK%GRAN2(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          ENDDO
        ELSE
          !* interpolation of heat on snow levels
          CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%GRAN2)
        ENDIF
        !
        !* mask for areas where there is no snow
        DO JL=1,ISNOW_NLAYER
          WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%GRAN2(:,JL) = XUNDEF
        END DO
        !
      CASE('HIS')
        !
        IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
          SK%HIST(:,:) = ZW%AL(JP)%ZOUT(:,:)
        ELSEIF(INL==1) THEN
          DO JL = 1,ISNOW_NLAYER
            SK%HIST(:,JL) = ZW%AL(JP)%ZOUT(:,1)
          ENDDO
        ELSE
          !* interpolation of heat on snow levels
          CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%HIST)
        ENDIF
        !
        !* mask for areas where there is no snow
        DO JL=1,ISNOW_NLAYER
          WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%HIST(:,JL) = XUNDEF
        END DO
        !
      CASE('AGE')
        !
        IF (SK%SCHEME=='3-L'.AND.(.NOT.OSNOW_IDEAL).AND.(.NOT.OUNIF))THEN
          SK%AGE(:,:) = 0.0
        ELSE
          IF (OSNOW_IDEAL.OR.INL==ISNOW_NLAYER) THEN
            SK%AGE(:,:) = ZW%AL(JP)%ZOUT(:,:)
          ELSEIF(INL==1) THEN
            DO JL = 1,ISNOW_NLAYER
              SK%AGE(:,JL) = ZW%AL(JP)%ZOUT(:,1)
            ENDDO
          ELSE
            !* interpolation of heat on snow levels
            CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW%AL(JP)%ZOUT,ZGRID(1:KSIZE_P(JP),:,JP),SK%AGE)
          ENDIF
        ENDIF
        !
        !* mask for areas where there is no snow
        DO JL=1,ISNOW_NLAYER
          WHERE(PDEPTH(1:KSIZE_P(JP),JL,JP)==0. .OR. PDEPTH(1:KSIZE_P(JP),JL,JP)==XUNDEF) SK%AGE(:,JL) = XUNDEF
        END DO
        !
    END SELECT
    !
  ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      7.     Deallocations
!
  IF (PRESENT(PDEPTH) .AND. .NOT.OSNOW_IDEAL) DEALLOCATE(ZGRID    )
  DO JP =1,KPATCH
    DEALLOCATE(ZW%AL(JP)%ZOUT)
  ENDDO
  DEALLOCATE(ZW%AL)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID_NAT
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),     INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JL, JL1  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PGRID1)) :: ZT1
REAL, DIMENSION(SIZE(PT1,1),SIZE(PGRID1)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
INTEGER                       :: JP    ! loop on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
!
ZD2(:,:) = 0.
DO JL=1,SIZE(ZD2,2)
  ZD2(:,JL) = PD2(:,JL)
END DO
!
DO JL=1,SIZE(PGRID1)
  JL1 = MIN(JL,SIZE(PT1,2))
  ZT1(:,JL) = PT1(:,JL1)
  ZD1(:,JL) = PGRID1(JL)
END DO
!
CALL INTERP_GRID_NAT(ZD1,ZT1(:,:),ZD2,PT2(:,:))
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SNOW_FIELD
