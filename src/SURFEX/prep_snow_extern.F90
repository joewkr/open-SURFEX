!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SNOW_EXTERN (GCP,HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                             KLUOUT,PFIELD,OSNOW_IDEAL,KLAYER,KTEB_PATCH)
!     #################################################################################
!
!
!!****  *PREP_SNOW_EXTERN*  
!!
!!    PURPOSE
!!    -------
!       Read and prepare initial snow fields from external files
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
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
!!         * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    ?
!!       02/2014 E. Martin : cor. for passing from from multilayer to a single layer
!!      B. Decharme  04/2014, external init with FA files
!!                            improve vertical interpolation
!-------------------------------------------------------------------------------
!
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_TYPE_SNOW
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW, NGRID_LEVEL
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_TOWN_PRESENCE
USE MODI_ABOR1_SFX
USE MODI_PREP_GRID_EXTERN
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_ALLOCATE_GR_SNOW
USE MODI_DEALLOC_GR_SNOW
USE MODI_INTERP_GRID_NAT
USE MODI_READ_GR_SNOW
USE MODI_READ_SURF
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_READ_TEB_PATCH
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=10),  INTENT(IN)  :: HSURF     ! type of field
CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of file
CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER  :: PFIELD    ! field to interpolate horizontally
LOGICAL,            INTENT(INOUT)  :: OSNOW_IDEAL
INTEGER,            INTENT(IN)  :: KLAYER    ! Number of layer of output snow scheme
INTEGER,            INTENT(IN) :: KTEB_PATCH
!
!*      0.2    declarations of local variables
!
TYPE(SURF_SNOW)                    :: TZSNOW ! snow characteristics

REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD       ! work field on input snow grid
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTEMP        ! snow temperature
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWLIQ        ! liquid water snow pack content
REAL, DIMENSION(:),   ALLOCATABLE :: ZD           ! total snow depth
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDEPTH       ! thickness of each layer (m)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZGRID        ! normalized input grid
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK_P
!
LOGICAL                           :: GTOWN          ! town variables written in the file
CHARACTER(LEN=12)                 :: YRECFM         ! record name
INTEGER                           :: IRESP          ! error return code
INTEGER                           :: IVERSION_PGD, IVERSION_PREP       ! SURFEX version
LOGICAL                           :: GOLD_NAME      ! old name flag 
INTEGER                           :: IBUGFIX_PGD, IBUGFIX_PREP        ! SURFEX bug version
INTEGER                           :: IVEGTYPE       ! actual number of vegtypes
INTEGER                           :: JL         ! loop on snow vertical grids
INTEGER                           :: JI             ! loop on pts
INTEGER                           :: INI
CHARACTER(LEN=8)                  :: YAREA          ! area treated ('ROOF','ROAD','VEG ')
CHARACTER(LEN=3)                  :: YPREFIX        ! prefix to identify patch
INTEGER                           :: IPATCH         ! number of input patch
INTEGER                           :: ITEB_PATCH     ! number of input patch for TEB
INTEGER                           :: JP         ! loop on patch
CHARACTER(LEN=6)                  :: YMASK          ! type of tile mask
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      3.     Area being treated
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_EXTERN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
YAREA='        '
YAREA(1:4) = HSURF(7:10)
!
IF (YAREA(1:4)=='VEG ') THEN
  IVEGTYPE = NVEGTYPE
  YMASK = 'NATURE'
  YPREFIX = '   '  
ELSE
  IVEGTYPE = 1
  YMASK    = 'TOWN  '
  IPATCH   = 1
  YPREFIX = '   '  
END IF
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
!* reading of version of the file being read
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
CALL READ_SURF(HFILEPGDTYPE,'VERSION',IVERSION_PGD,IRESP,HDIR='-')
CALL READ_SURF(HFILEPGDTYPE,'BUG',IBUGFIX_PGD,IRESP,HDIR='-')
GOLD_NAME=(IVERSION_PGD<7 .OR. (IVERSION_PGD==7 .AND. IBUGFIX_PGD<3))
!
 CALL PREP_GRID_EXTERN(GCP,HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
 CALL TOWN_PRESENCE(HFILEPGDTYPE,GTOWN,HDIR='-')
!
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,YMASK)
!
IF (YAREA(1:4)=='VEG ') THEN
  YRECFM = 'PATCH_NUMBER'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
ELSE
  IF (.NOT.GOLD_NAME) THEN
    IF (YAREA(1:4)=='ROOF') YAREA(1:4) = 'RF  '
    IF (YAREA(1:4)=='ROAD') YAREA(1:4) = 'RD  '
  ENDIF
  IF (GTOWN) THEN   
    CALL READ_TEB_PATCH(HFILEPGD,HFILEPGDTYPE,IVERSION_PGD,IBUGFIX_PGD,ITEB_PATCH,HDIR='-')
  ELSE
    ITEB_PATCH = 1
  ENDIF    
  IF (ITEB_PATCH>1) THEN
    WRITE(YPREFIX,FMT='(A,I1,A)') 'T',MIN(KTEB_PATCH,ITEB_PATCH),'_'
  END IF  
END IF
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
 CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
 CALL READ_SURF(HFILETYPE,'VERSION',IVERSION_PREP,IRESP,HDIR='-')
 CALL READ_SURF(HFILETYPE,'BUG',IBUGFIX_PREP,IRESP,HDIR='-')
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
IF (NRANK/=NPIO) INI = 0
!
!-------------------------------------------------------------------------------------
!
!*      4.     Reading of snow data
!              ---------------------
!
ALLOCATE(IMASK_P(INI))
DO JI = 1,INI
  IMASK_P(JI) = JI
ENDDO
!
DO JP = 1,IPATCH
  !
  IF (YAREA(1:2)=='RO' .OR. YAREA(1:2)=='GA' .OR. YAREA(1:2)=='RF' .OR. YAREA(1:2)=='RD') THEN
    IF (.NOT. GTOWN) THEN
      TZSNOW%SCHEME='1-L'
      TZSNOW%NLAYER=1
      CALL ALLOCATE_GR_SNOW(TZSNOW,INI)
    ELSE
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,YMASK)
      CALL READ_GR_SNOW(HFILETYPE,TRIM(YAREA),YPREFIX,INI,INI,IMASK_P,0,TZSNOW, &
                        HDIR='E',KVERSION=IVERSION_PREP,KBUGFIX=IBUGFIX_PREP)
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    ENDIF
  ELSE
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,YMASK)
    CALL READ_GR_SNOW(HFILETYPE,TRIM(YAREA),YPREFIX,INI,INI,IMASK_P,JP,TZSNOW, &
                      HDIR='E',KVERSION=IVERSION_PREP,KBUGFIX=IBUGFIX_PREP,KNPATCH=IPATCH)
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      5.     Total snow content
  !              ------------------
  !
  IF (NRANK==NPIO) THEN
    !
    SELECT CASE (HSURF(1:3))
      CASE ('WWW')
        IF (OSNOW_IDEAL) THEN
          IF (JP<=1) ALLOCATE(PFIELD(INI,KLAYER,IPATCH))
          PFIELD(:,:,JP) = TZSNOW%WSNOW(:,1:KLAYER)
        ELSE
          IF (JP<=1) ALLOCATE(PFIELD(INI,1,IPATCH))
          PFIELD(:,:,JP) = 0.
          DO JL=1,TZSNOW%NLAYER
            PFIELD(:,1,JP) = PFIELD(:,1,JP) + TZSNOW%WSNOW(:,JL)
          END DO 
          WHERE ( PFIELD(:,1,JP)>XUNDEF ) PFIELD(:,1,JP)=XUNDEF
        ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      6.     Albedo
  !              ------
  !
    CASE ('ALB')
      IF (JP<=1) ALLOCATE(PFIELD(INI,1,IPATCH))
      PFIELD(:,1,JP) = TZSNOW%ALB(:)
  !
  !-------------------------------------------------------------------------------------
  !
  !*      7.     Total depth to snow grid
  !              ------------------------
  !
    CASE ('DEP')
      IF (OSNOW_IDEAL.OR.TZSNOW%NLAYER==KLAYER) THEN    
        IF (JP<=1) ALLOCATE(PFIELD(INI,KLAYER,IPATCH))  
        PFIELD(:,:,JP) = TZSNOW%WSNOW(:,1:KLAYER)/TZSNOW%RHO(:,1:KLAYER)
        WHERE(TZSNOW%WSNOW(:,1:KLAYER)==XUNDEF) PFIELD(:,:,JP)=XUNDEF
      ELSE     
        ALLOCATE(ZD(INI))
        ZD(:) = 0.0
        DO JL=1,TZSNOW%NLAYER
          WHERE (TZSNOW%WSNOW(:,JL)/=XUNDEF)
            ZD(:) = ZD(:) + TZSNOW%WSNOW(:,JL)/TZSNOW%RHO(:,JL)
          ENDWHERE
        END DO
        IF (JP<=1) ALLOCATE(PFIELD(INI,1,IPATCH))
        PFIELD(:,1,JP) = ZD(:)
        DEALLOCATE(ZD)
      ENDIF
  !
  !-------------------------------------------------------------------------------------
  !
  !*      8.     Density or heat profile
  !              -----------------------
  !
    CASE ('RHO','HEA','SG1','SG2','HIS','AGE')
  !
      SELECT CASE (TZSNOW%SCHEME)
        CASE ('D95','1-L','EBA')
          IF (JP<=1) ALLOCATE(PFIELD(INI,1,IPATCH))   
          !* computes output physical variable
          IF (HSURF(1:3)=='RHO') PFIELD(:,1,JP) = TZSNOW%RHO(:,1)
          IF (HSURF(1:3)=='HEA') THEN
            ALLOCATE(ZTEMP(INI,TZSNOW%NLAYER))
            ALLOCATE(ZWLIQ(INI,TZSNOW%NLAYER))
            IF (TZSNOW%SCHEME=='D95'.OR.TZSNOW%SCHEME=='EBA') ZTEMP(:,1) = XTT-2.
            IF (TZSNOW%SCHEME=='1-L') ZTEMP(:,1) = TZSNOW%T(:,1)
            ZWLIQ(:,:) = 0.0
            CALL SNOW_T_WLIQ_TO_HEAT(PFIELD(:,:,JP),TZSNOW%RHO,ZTEMP,ZWLIQ)
            DEALLOCATE(ZTEMP)
            DEALLOCATE(ZWLIQ)
          END IF
          IF (HSURF(1:3)=='SG1') PFIELD(:,1,JP) = -20.0
          IF (HSURF(1:3)=='SG2') PFIELD(:,1,JP) = 80.0
          IF (HSURF(1:3)=='HIS') PFIELD(:,1,JP) = 0.0
          IF (HSURF(1:3)=='AGE') PFIELD(:,1,JP) = 3.0

        CASE ('3-L','CRO')
          ALLOCATE(ZFIELD(INI,TZSNOW%NLAYER))
          !* input physical variable
          IF (HSURF(1:3)=='RHO') ZFIELD(:,:) = TZSNOW%RHO (:,1:TZSNOW%NLAYER)
          IF (HSURF(1:3)=='HEA') ZFIELD(:,:) = TZSNOW%HEAT(:,1:TZSNOW%NLAYER)
          IF (HSURF(1:3)=='AGE') ZFIELD(:,:) = TZSNOW%AGE(:,1:TZSNOW%NLAYER)
          IF (TZSNOW%SCHEME=='CRO')THEN
            IF (HSURF(1:3)=='SG1') ZFIELD(:,:) = TZSNOW%GRAN1(:,1:TZSNOW%NLAYER)
            IF (HSURF(1:3)=='SG2') ZFIELD(:,:) = TZSNOW%GRAN2(:,1:TZSNOW%NLAYER)
            IF (HSURF(1:3)=='HIS') ZFIELD(:,:) = TZSNOW%HIST(:,1:TZSNOW%NLAYER)
          ELSE
           IF (HSURF(1:3)=='SG1') ZFIELD(:,:) = -20.0
           IF (HSURF(1:3)=='SG2') ZFIELD(:,:) = 80.0
           IF (HSURF(1:3)=='HIS') ZFIELD(:,:) = 0.0                  
          ENDIF    
          !
          IF (OSNOW_IDEAL) THEN 
            IF (JP<=1) ALLOCATE(PFIELD(INI,KLAYER,IPATCH))                  
            PFIELD(:,:,JP) = ZFIELD(:,:)
          ELSE
            !
            IF (JP<=1) ALLOCATE(PFIELD(INI,NGRID_LEVEL,IPATCH))
            !* input snow layer thickness
            ALLOCATE(ZDEPTH(INI,TZSNOW%NLAYER))
            ZDEPTH(:,:) = TZSNOW%WSNOW(:,:)/TZSNOW%RHO(:,:)
            !
            !* total depth
            ALLOCATE(ZD(INI))
            ZD(:) = 0.
            DO JL=1,TZSNOW%NLAYER
              ZD(:) = ZD(:) + ZDEPTH(:,JL)
            ENDDO
            !
            !* input normalized grid
            ALLOCATE(ZGRID(INI,TZSNOW%NLAYER))
            DO JI=1,INI
              IF(ZD(JI)==0.0)THEN
                DO JL = 1,TZSNOW%NLAYER
                  ZGRID(JI,JL)=REAL(JL)/REAL(TZSNOW%NLAYER)
                ENDDO
              ELSE
                DO JL = 1,TZSNOW%NLAYER
                  IF(JL==1)THEN
                    ZGRID(JI,JL)=ZDEPTH(JI,JL)/ ZD(JI)
                  ELSE
                    ZGRID(JI,JL) = ZGRID(JI,JL-1) + ZDEPTH(JI,JL)/ZD(JI)
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
            DEALLOCATE(ZDEPTH)
            DEALLOCATE(ZD)
            !    
            ! * interpolation of profile onto fine normalized snow grid
            CALL INTERP_GRID_NAT(ZGRID(:,:),ZFIELD(:,:),    &
                               XGRID_SNOW(:), PFIELD(:,:,JP))
            DEALLOCATE(ZGRID)
          ENDIF
          DEALLOCATE(ZFIELD)

        END SELECT
        !* put field form patch to all vegtypes    
    END SELECT
  !
  ENDIF
  !
  CALL DEALLOC_GR_SNOW(TZSNOW)
  !
ENDDO
!
DEALLOCATE(IMASK_P)
!
!-------------------------------------------------------------------------------------
!
!*      9.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_EXTERN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_EXTERN
