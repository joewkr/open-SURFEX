!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_ISBA_FIELD (DTCO, UG, U, USS, GCP, IG, IO, S, NK, NP, NPE, TPTIME,  &
                                HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL,OKEY)
!     #################################################################################
!
!!****  *PREP_HOR_ISBA_FIELD* - reads, interpolates and prepares an ISBA field
!!
!!    PURPOSE
!!    -------
!
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
!!      P. Le Moigne 03/2007, Ajout initialisation par ascllv
!!      B. Decharme  01/2009, Optional Arpege deep soil temperature initialization
!!      M. Lafaysse  07/2012, allow netcdf input files
!!      B. Decharme  07/2012, Bug init uniform snow
!!      M. Lafaysse 11/2012,  snow liquid water content
!!      B. Decharme  03/2014, external init with FA files
!!                            new vertical interpolation
!!      P Samuelsson 10/2014, MEB
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_TYPE_SNOW
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t, ISBA_K_t, ISBA_S_t, &
                        ISBA_PE_t, ISBA_P_t
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_PREP,     ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, &
                          XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT, &
                          LINTERP, CMASK
USE MODD_GRID_GRIB, ONLY : CINMODEL  
!
USE MODD_PREP_ISBA, ONLY : XGRID_SOIL, NGRID_LEVEL, LSNOW_IDEAL,    &
                           XWSNOW, XRSNOW, XTSNOW, XLWCSNOW, XASNOW,  &
                           XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW


USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODI_PREP_GRIB_GRID
USE MODI_READ_PREP_ISBA_CONF
USE MODI_READ_PREP_ISBA_SNOW
USE MODI_PREP_ISBA_ASCLLV
USE MODI_PREP_ISBA_GRIB
USE MODI_PREP_ISBA_UNIF
USE MODI_PREP_ISBA_BUFFER
USE MODI_ABOR1_SFX
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_ISBA_EXTERN
USE MODI_PREP_ISBA_NETCDF
USE MODI_PACK_SAME_RANK
USE MODI_ALLOCATE_GR_SNOW
USE MODI_GET_PREP_INTERP
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(DATE_TIME), INTENT(IN) :: TPTIME
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
LOGICAL, OPTIONAL,  INTENT(INOUT):: OKEY
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILETYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILE_SNOW     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD_SNOW     ! name of file 
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, POINTER, DIMENSION(:,:,:) :: ZFIELDIN   ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:,:) :: ZFIELDOUTP ! field interpolated   horizontally
REAL, POINTER, DIMENSION(:,:,:) :: ZFIELDOUTV !
!
TYPE(NSURF_SNOW) :: TNPSNOW
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
TYPE FOUT
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT
END TYPE FOUT
TYPE NFOUT
  TYPE(FOUT), DIMENSION(:), ALLOCATABLE :: AL
END TYPE NFOUT
TYPE (NFOUT) :: ZW
TYPE(NFOUT) :: ZF
!
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZDG       ! out T grid (x, output soil grid, npatch)
TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
!
 CHARACTER(LEN=3) :: YSNOW_SCHEME
INTEGER :: ISNOW_NLAYER
!
INTEGER, DIMENSION(IO%NPATCH) :: ISIZE_P
INTEGER, DIMENSION(SIZE(IG%XLAT),IO%NPATCH) :: IR_P
!
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
LOGICAL                       :: GUNIF_SNOW! flag for prescribed uniform field
INTEGER                       :: JP    ! loop on patches
INTEGER                       :: JVEG  ! loop on vegtypes
INTEGER                       :: INI, INL, INP, JI, JL! Work integer
INTEGER                       :: INFOMPI
INTEGER, DIMENSION(SIZE(IG%XLAT)) :: IWORK
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZVEGTYPE_PATCH
REAL, DIMENSION(:,:), ALLOCATABLE :: ZPATCH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_ISBA_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                         HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'NATURE'
!
INI=SIZE(IG%XLAT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_VEG ') THEN
  CALL READ_PREP_ISBA_SNOW(HPROGRAM, YSNOW_SCHEME, ISNOW_NLAYER, YFILE_SNOW, YFILETYPE_SNOW,&
                           YFILEPGD_SNOW, YFILEPGDTYPE_SNOW, GUNIF_SNOW)
  !
  DO JP = 1,IO%NPATCH
    NPE%AL(JP)%TSNOW%SCHEME = YSNOW_SCHEME
    NPE%AL(JP)%TSNOW%NLAYER = ISNOW_NLAYER
    ISIZE_P(JP) = NP%AL(JP)%NSIZE_P
    IR_P(:,JP) = 0
    IR_P(1:ISIZE_P(JP),JP) = NP%AL(JP)%NR_P
  ENDDO
  !
  IF(.NOT.GUNIF_SNOW.AND.LEN_TRIM(YFILE_SNOW)==0.AND.LEN_TRIM(YFILETYPE_SNOW)==0)THEN
    IF(LEN_TRIM(YFILE)/=0.AND.LEN_TRIM(YFILETYPE)/=0)THEN
      YFILE_SNOW        = YFILE
      YFILETYPE_SNOW    = YFILETYPE
      YFILEPGD_SNOW     = YFILEPGD
      YFILEPGDTYPE_SNOW = YFILEPGDTYPE       
    ELSE
      GUNIF_SNOW=.TRUE.
      IF(ALL(XWSNOW==XUNDEF))XWSNOW=0.0
    ENDIF
  ENDIF
  !
  ALLOCATE(TNPSNOW%AL(IO%NPATCH))
  ALLOCATE(ZVEGTYPE_PATCH(SIZE(S%XVEGTYPE_PATCH,1),SIZE(S%XVEGTYPE_PATCH,2),SIZE(S%XVEGTYPE_PATCH,3)))
  ALLOCATE(ZPATCH(SIZE(S%XPATCH,1),SIZE(S%XPATCH,2)))
  ZVEGTYPE_PATCH(:,:,:) = 0.
  ZPATCH(:,:) = 0.
  DO JP = 1,IO%NPATCH
    CALL PACK_SAME_RANK(NP%AL(JP)%NR_P,S%XVEGTYPE_PATCH(:,:,JP),ZVEGTYPE_PATCH(1:NP%AL(JP)%NSIZE_P,:,JP))
    CALL PACK_SAME_RANK(NP%AL(JP)%NR_P,S%XPATCH(:,JP),ZPATCH(1:NP%AL(JP)%NSIZE_P,JP))
    TNPSNOW%AL(JP)%SCHEME = NPE%AL(JP)%TSNOW%SCHEME
    TNPSNOW%AL(JP)%NLAYER = NPE%AL(JP)%TSNOW%NLAYER
  ENDDO
  !
  CALL PREP_HOR_SNOW_FIELDS(DTCO, IG, U, GCP, HPROGRAM, HSURF,   &
                            YFILE_SNOW, YFILETYPE_SNOW,          &
                            YFILEPGD_SNOW, YFILEPGDTYPE_SNOW,    &
                            ILUOUT, GUNIF_SNOW, IO%NPATCH, 1,    &
                            INI,TNPSNOW, TPTIME,                 &
                            XWSNOW, XRSNOW, XTSNOW, XLWCSNOW,    &
                            XASNOW, LSNOW_IDEAL, XSG1SNOW,       &
                            XSG2SNOW, XHISTSNOW, XAGESNOW, YDCTL,&
                            PVEGTYPE_PATCH=ZVEGTYPE_PATCH,       &
                            PPATCH=ZPATCH, KSIZE_P=ISIZE_P,      &
                            KR_P=IR_P, OKEY=OKEY   )
  !
  DEALLOCATE(ZPATCH)
  DEALLOCATE(ZVEGTYPE_PATCH)
  !
  DO JP = 1,IO%NPATCH
    PEK => NPE%AL(JP)
    CALL ALLOCATE_GR_SNOW(PEK%TSNOW,NP%AL(JP)%NSIZE_P)
    PEK%TSNOW%WSNOW = TNPSNOW%AL(JP)%WSNOW
    PEK%TSNOW%RHO   = TNPSNOW%AL(JP)%RHO
    PEK%TSNOW%ALB   = TNPSNOW%AL(JP)%ALB
    IF (PEK%TSNOW%SCHEME/='D95') PEK%TSNOW%HEAT = TNPSNOW%AL(JP)%HEAT
    IF (PEK%TSNOW%SCHEME=='CRO'.OR.PEK%TSNOW%SCHEME=='3-L') &
      PEK%TSNOW%AGE = TNPSNOW%AL(JP)%AGE
    IF (PEK%TSNOW%SCHEME=='CRO') THEN
      PEK%TSNOW%GRAN1 = TNPSNOW%AL(JP)%GRAN1
      PEK%TSNOW%GRAN2 = TNPSNOW%AL(JP)%GRAN2
      PEK%TSNOW%HIST = TNPSNOW%AL(JP)%HIST
    ENDIF
    !
    CALL TYPE_SNOW_INIT(TNPSNOW%AL(JP))
  ENDDO
  DEALLOCATE(TNPSNOW%AL)
  !
  DEALLOCATE(XWSNOW)
  DEALLOCATE(XRSNOW)
  DEALLOCATE(XTSNOW)
  DEALLOCATE(XLWCSNOW)
  DEALLOCATE(XSG1SNOW)
  DEALLOCATE(XSG2SNOW)
  DEALLOCATE(XHISTSNOW)
  DEALLOCATE(XAGESNOW)
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of input  configuration (Grid and interpolation type)
!
NULLIFY (ZFIELDIN, ZFIELDOUTP, ZFIELDOUTV)
!
IF (YDCTL%LPART1) THEN
!
  IF (GUNIF) THEN
    CALL PREP_ISBA_UNIF(ILUOUT,HSURF,ZFIELDIN)
  ELSE IF (YFILETYPE=='ASCLLV') THEN
    CALL PREP_ISBA_ASCLLV(DTCO, UG, U, USS, HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='GRIB  ') THEN
    CALL PREP_GRIB_GRID(YFILE,ILUOUT,CINMODEL,CINGRID_TYPE,CINTERP_TYPE,TZTIME_GRIB)        
    IF (NRANK==NPIO) CALL PREP_ISBA_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)        
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '&
          .OR.YFILETYPE=='FA    '.OR. YFILETYPE=='AROME ') THEN
    CALL PREP_ISBA_EXTERN(DTCO, IO, U, GCP, HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN,OKEY)
  ELSE IF (YFILETYPE=='BUFFER') THEN
    CALL PREP_ISBA_BUFFER(IG, U, HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='NETCDF') THEN
    CALL PREP_ISBA_NETCDF(DTCO, U, HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_ISBA_FIELD: data file type not supported : '//YFILETYPE)
  END IF
!
  INL = SIZE(ZFIELDIN,2)
  INP = SIZE(ZFIELDIN,3)
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Horizontal interpolation
!
 CALL PREP_CTL_INT_PART2 (YDCTL, HSURF, CMASK, 'NATURE', ZFIELDIN)
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
  ALLOCATE(ZFIELDOUTP(INI,INL,INP))
!
! ZPATCH is the array of output patches put on the input patches
  ALLOCATE(ZPATCH(INI,INP))
  ZPATCH(:,:) = 0.
!
  CALL GET_PREP_INTERP(INP,IO%NPATCH,S%XVEGTYPE,S%XPATCH,ZPATCH)
!
  DO JP = 1, INP
  ! we interpolate each point the output patch is present
    LINTERP(:) = (ZPATCH(:,JP) > 0.)
    CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZFIELDIN(:,:,JP),ZFIELDOUTP(:,:,JP))
    LINTERP = .TRUE.
  END DO
!
  DEALLOCATE(ZFIELDIN,ZPATCH)
!
ENDIF
!
 CALL PREP_CTL_INT_PART4 (YDCTL, HSURF, 'NATURE', CMASK, ZFIELDIN, ZFIELDOUTP)
!
IF (YDCTL%LPART5) THEN
!
  INL = SIZE (ZFIELDOUTP,2)
  INP = SIZE (ZFIELDOUTP,3)
!
  IF (TRIM(HSURF)/="ZS") THEN
  !
    ALLOCATE(ZW%AL(IO%NPATCH))
  !
    IF (IO%NPATCH/=INP) THEN
    !
      ALLOCATE(ZFIELDOUTV(INI,INL,NVEGTYPE))
      CALL PUT_ON_ALL_VEGTYPES(INI,INL,INP,NVEGTYPE,ZFIELDOUTP,ZFIELDOUTV)
    !
    !*      6.     Transformation from vegtype grid to patch grid
    !
      DEALLOCATE(ZFIELDOUTP)
    !
      DO JP = 1,IO%NPATCH
        PK => NP%AL(JP)
        !
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,INL))
        !
        CALL VEGTYPE_GRID_TO_PATCH_GRID(JP, IO%NPATCH, PK%XVEGTYPE_PATCH, PK%XPATCH,&
                                      PK%NR_P, ZFIELDOUTV, ZW%AL(JP)%ZOUT)
      ENDDO
      !
      DEALLOCATE(ZFIELDOUTV)
    !
    ELSE
    !
      DO JP = 1,IO%NPATCH
      !
        PK => NP%AL(JP)
      !
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,INL))
      !
        CALL PACK_SAME_RANK(PK%NR_P,ZFIELDOUTP(:,:,JP),ZW%AL(JP)%ZOUT)
      !
      ENDDO
    !
      DEALLOCATE(ZFIELDOUTP)
    !
    ENDIF
  !
  ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      7.     Return to historical variable
!
!
  SELECT CASE (HSURF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
    CASE('ZS     ') 
      ALLOCATE(XZS_LS(INI))
      XZS_LS(:) = ZFIELDOUTP(:,1,1)
      DEALLOCATE(ZFIELDOUTP)
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WG     ')
    !
      ALLOCATE(ZF%AL(IO%NPATCH))
    !
      DO JP = 1,IO%NPATCH
        KK => NK%AL(JP)
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
      !
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER))
      !
      !* interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT,PK%XDG,ZF%AL(JP)%ZOUT)
      !
      !* retrieves soil water content from soil relative humidity
        ALLOCATE(PEK%XWG(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XWG(:,:)=XUNDEF
        IF(IO%CISBA=='DIF')THEN
          IWORK(1:PK%NSIZE_P)=PK%NWG_LAYER(:)
        ELSE
          IWORK(1:PK%NSIZE_P)=SIZE(PEK%XWG,2)
        ENDIF
        DO JI=1,PK%NSIZE_P
          IF(IWORK(JI)==NUNDEF)CYCLE
          INL=IWORK(JI)
          DO JL=1,INL
            PEK%XWG(JI,JL) = KK%XWWILT(JI,JL) + ZF%AL(JP)%ZOUT(JI,JL) * (KK%XWFC(JI,JL)-KK%XWWILT(JI,JL))
            PEK%XWG(JI,JL) = MAX(MIN(PEK%XWG(JI,JL),KK%XWSAT(JI,JL)),XWGMIN)
          ENDDO
        ENDDO
        !
        WHERE(ZF%AL(JP)%ZOUT(:,:)==XUNDEF) PEK%XWG(:,:)=XUNDEF
        !
        DEALLOCATE(ZF%AL(JP)%ZOUT)
      ENDDO
      !
      DEALLOCATE(ZF%AL)
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WGI    ')
      ALLOCATE(ZF%AL(IO%NPATCH))
      !
      DO JP = 1,IO%NPATCH
        KK => NK%AL(JP)
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
        !
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER))
        !
        !* interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT,PK%XDG,ZF%AL(JP)%ZOUT)
        !
        !* retrieves soil ice content from soil relative humidity
        ALLOCATE(PEK%XWGI(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XWGI(:,:)=0.0
        IF(IO%CISBA=='DIF')THEN
          IWORK(1:PK%NSIZE_P)=PK%NWG_LAYER(:)
        ELSE
          IWORK(1:PK%NSIZE_P)=2
        ENDIF  
        DO JI=1,PK%NSIZE_P
          IF(IWORK(JI)==NUNDEF)CYCLE
          INL=IWORK(JI)
          DO JL=1,INL
            PEK%XWGI(JI,JL) = ZF%AL(JP)%ZOUT(JI,JL) * KK%XWSAT(JI,JL)
            PEK%XWGI(JI,JL) = MAX(MIN(PEK%XWGI(JI,JL),KK%XWSAT(JI,JL)),0.)
          ENDDO
        END DO
        !
        WHERE(ZF%AL(JP)%ZOUT(:,:)==XUNDEF ) PEK%XWGI(:,:)=XUNDEF
        WHERE(PEK%XWGI(:,:)<=1.0E-10)PEK%XWGI(:,:)=0.0
        !
        DEALLOCATE(ZF%AL(JP)%ZOUT)
        !
      ENDDO
      !
      DEALLOCATE(ZF%AL)
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('TG     ') 
      IF(IO%LTEMP_ARP)THEN
        INL=IO%NTEMPLAYER_ARP
      ELSE
        INL=IO%NGROUND_LAYER
      ENDIF
      !
      DO JP = 1,IO%NPATCH
        !
        PK => NP%AL(JP)
        PEK => NPE%AL(JP)
        !
        ALLOCATE(ZDG(SIZE(PK%XDG,1),INL))
        ALLOCATE(PEK%XTG(PK%NSIZE_P,INL))
        !
        IF (IO%CISBA=='2-L'.OR.IO%CISBA=='3-L') THEN
          ZDG(:,1) = 0.01
          ZDG(:,2) = 0.40                    ! deep temperature for force-restore taken at 20cm
          IF(IO%CISBA=='3-L') ZDG(:,3) = 5.00   ! climatological temperature, usually not used       
          IF(IO%LTEMP_ARP)THEN
            ZDG(:,3) = 1.0
            DO JL=4,INL
              ZDG(:,JL) = ZDG(:,JL-1)+1.0
            ENDDO
          ENDIF
        ELSE
          !* diffusion method, the soil grid is the same as for humidity
          ZDG(:,:) = PK%XDG(:,:)
        END IF
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT,ZDG,PEK%XTG)
        !
        DEALLOCATE(ZDG)
        !
      ENDDO
      !
      !
      !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WR     ')
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XWR(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XWR(:) = ZW%AL(JP)%ZOUT(:,1)
        
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WRL    ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XWRL(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XWRL(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
     !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WRLI   ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XWRLI(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XWRLI(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('WRVN   ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XWRVN(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XWRVN(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('TV     ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XTV(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XTV(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('TL     ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XTL(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XTL(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('TC     ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XTC(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XTC(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('QC     ') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XQC(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XQC(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('LAI    ') 
    !* LAI is updated only if present and pertinent (evolutive LAI) in input file
      DO JP = 1,IO%NPATCH
        IF (ANY(ZW%AL(JP)%ZOUT(:,:)/=XUNDEF)) THEN
          ALLOCATE(NPE%AL(JP)%XLAI(NP%AL(JP)%NSIZE_P))
          NPE%AL(JP)%XLAI(:) = ZW%AL(JP)%ZOUT(:,1)
        ENDIF
      ENDDO
    !
    !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    CASE('ICE_STO') 
      DO JP = 1,IO%NPATCH
        ALLOCATE(NPE%AL(JP)%XICE_STO(NP%AL(JP)%NSIZE_P))
        NPE%AL(JP)%XICE_STO(:) = ZW%AL(JP)%ZOUT(:,1)
      ENDDO
    !
  END SELECT
  !
  IF (TRIM(HSURF)/="ZS") THEN
    DO JP = 1,IO%NPATCH
      DEALLOCATE(ZW%AL(JP)%ZOUT)
    ENDDO
    DEALLOCATE(ZW%AL)
  ENDIF
!
ENDIF
!-------------------------------------------------------------------------------------
!
!*      8.     Deallocations
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID_NAT
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JI,JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
!
INTEGER :: ILAYER1, ILAYER2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IF (SIZE(PT1,2)==3) THEN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 1. case with only 3 input levels (typically coming from 'UNIF')
!     -----------------------------
!
  IF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN

    !* Possible LTEMP_ARP case
    IF(SIZE(PT2,2)>3)THEN
      ILAYER1=3
      ILAYER2=SIZE(PT2,2)
    ELSE
      ILAYER1=SIZE(PT2,2)
      ILAYER2=0
    ENDIF
    !* historical 2L or 3L ISBA version
    PT2(:,1:ILAYER1) = PT1(:,1:ILAYER1) 
    !* Possible LTEMP_ARP case
    IF(ILAYER2>0)THEN
      DO JL=ILAYER1+1,ILAYER2
        PT2(:,JL) = PT2(:,ILAYER1)
      ENDDO
    ENDIF

  ELSEIF(IO%CISBA=='DIF')THEN

    !surface layer (generally 0.01m imposed)
    PT2(:,1) = PT1(:,1) 
    !second layer
    PT2(:,2) = 0.25*PT1(:,1)+0.75*PT1(:,2)
    !others layers
    DO JI=1,SIZE(PT1,1)
      DO JL=3,IO%NGROUND_LAYER
        IF(PD2(JI,JL)<=PK%XDG2(JI))THEN 
          !root layers
          PT2(JI,JL) = PT1(JI,2)
        ELSE
          !deep layers
          PT2(JI,JL) = PT1(JI,3)
        ENDIF
      END DO
    END DO 
  END IF    
!  
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 2. case with fine grid as input (general case)
!     ----------------------------
!
  DO JL=1,SIZE(PT1,2)
     ZD1(:,JL) = PGRID1(JL)
  ENDDO
!
  ZD2(:,:) = PD2(:,:)
  CALL INTERP_GRID_NAT(ZD1,PT1(:,:),ZD2,PT2(:,:))
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ENDIF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_ISBA_FIELD
