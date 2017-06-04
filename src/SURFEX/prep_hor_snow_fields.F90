!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_SNOW_FIELDS (DTCO, G, U, GCP, HPROGRAM,HSURF,&
                                HFILE,HFILETYPE,             &
                                HFILEPGD,HFILEPGDTYPE,       &
                                KLUOUT,OUNIF,KPATCH,         &
                                KTEB_PATCH, KL,TNPSNOW, TPTIME, &
                                PUNIF_WSNOW, PUNIF_RSNOW,    &
                                PUNIF_TSNOW, PUNIF_LWCSNOW,  &
                                PUNIF_ASNOW, OSNOW_IDEAL,    &
                                PUNIF_SG1SNOW, PUNIF_SG2SNOW,&
                                PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,&
                                PVEGTYPE_PATCH, KSIZE_P, KR_P,&
                                PPATCH, OKEY                 )  
!     #######################################################
!
!
!!****  *PREP_HOR_SNOW_FIELDS* - prepares all snow fields for one surface scheme.
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
!!      B. Decharme 10/2013, Phasage Arpege-Climat
!!      B. Decharme 04/2014, Init permsnow
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SNOW_PAR,       ONLY : XAGLAMIN, XAGLAMAX
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODI_ALLOCATE_GR_SNOW
USE MODI_PREP_HOR_SNOW_FIELD
USE MODE_SNOW3L
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
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
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
INTEGER,            INTENT(IN)  :: KPATCH    ! patch number for output scheme
INTEGER,            INTENT(IN) :: KTEB_PATCH
INTEGER,            INTENT(IN)  :: KL        ! number of points
TYPE(NSURF_SNOW), INTENT(INOUT) :: TNPSNOW   ! snow fields
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

REAL,DIMENSION(:,:,:),  INTENT(IN ), OPTIONAL :: PVEGTYPE_PATCH ! fraction of each vegtype per patch
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL :: KSIZE_P
INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: KR_P
REAL,DIMENSION(:,:),    INTENT(IN ), OPTIONAL :: PPATCH         ! fraction of each patch
LOGICAL,                INTENT(OUT), OPTIONAL :: OKEY
!
!
!*      0.2    declarations of local variables
!
TYPE(SURF_SNOW), POINTER :: SK
 CHARACTER(LEN=10)                   :: YSNSURF   ! type of field
REAL,ALLOCATABLE,DIMENSION(:)     :: ZWRHO     ! total snow content from rho profile alone
REAL,ALLOCATABLE,DIMENSION(:,:,:) :: ZDEPTH    ! snow depth of each layer
REAL,ALLOCATABLE,DIMENSION(:)     :: ZDTOT     ! total snow depth
INTEGER, DIMENSION(KPATCH) :: ISIZE_P
INTEGER,DIMENSION(KL,KPATCH) :: IR_P    ! fraction of each patch
REAL,DIMENSION(KL,KPATCH)         :: ZPATCH    ! fraction of each patch
REAL,DIMENSION(:,:,:), ALLOCATABLE  :: ZVEGTYPE_PATCH    ! fraction of each vegtype per patch
!
INTEGER                             :: JP, ISNOW_NLAYER    ! loop counter on patches
INTEGER                             :: JL, JI, ISIZE   ! loop counter on layers
INTEGER                             :: IVERSION  ! surface version
CHARACTER(LEN=16)                   :: YRECFM    ! record name
INTEGER                             :: IRESP     ! error return code
LOGICAL                             :: GGLACIER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',0,ZHOOK_HANDLE)
!
ISNOW_NLAYER = TNPSNOW%AL(1)%NLAYER
!
IF (PRESENT(KSIZE_P)) THEN
  ISIZE_P(:) = KSIZE_P(:)
ELSE
  ISIZE_P(:) = KL
ENDIF
!
IF (PRESENT(KR_P)) THEN
  IR_P(:,:) = KR_P(:,:)
ELSE
  DO JI = 1,KL
    IR_P(JI,:) = JI
  ENDDO
ENDIF
!
IF (PRESENT(PPATCH)) THEN
   ZPATCH = PPATCH
ELSE
   ZPATCH = 1.
ENDIF
IF (PRESENT(PVEGTYPE_PATCH)) THEN
  ALLOCATE(ZVEGTYPE_PATCH(KL,SIZE(PVEGTYPE_PATCH,2),KPATCH))
  ZVEGTYPE_PATCH = PVEGTYPE_PATCH
ELSE
  ALLOCATE(ZVEGTYPE_PATCH(KL,1,KPATCH))
  ZVEGTYPE_PATCH = 1.
ENDIF
!
!*      1.     Allocation of output field
!
DO JP = 1,KPATCH
  CALL ALLOCATE_GR_SNOW(TNPSNOW%AL(JP),ISIZE_P(JP))
ENDDO
!
!---------------------------------------------------------------------------
!
!*      2.     Find if PERMSNOW must be done
!
IF(PRESENT(OKEY))THEN
  !  
  IF ( (HFILETYPE=='MESONH' .OR. HFILETYPE=='ASCII ' .OR. HFILETYPE=='LFI   '.OR. HFILETYPE=='FA    ') &
       .AND. (HSURF=='SN_VEG ')  ) THEN
    !       
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
    YRECFM='VERSION'
    CALL READ_SURF(HFILETYPE,YRECFM,IVERSION,IRESP)    
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    !  
    IF(IVERSION>7)THEN       
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
      YRECFM='GLACIER'
      CALL READ_SURF(HFILETYPE,YRECFM,GGLACIER,IRESP)    
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)  
      IF(GGLACIER)OKEY=.FALSE.
    ENDIF
    !
  ENDIF
  !  
  IF(OSNOW_IDEAL)OKEY=.FALSE.
  !
ENDIF
!
!---------------------------------------------------------------------------
!
!*      3.     Treatment of total snow content (kg/m2)
!
YSNSURF='WWW'//HSURF
CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE,  &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,&
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,             &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,  &                      
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P         )  
!
!----------------------------------------------------------------------------
!
!*      4.     Treatment of total snow depth
!
DO JP = 1,KPATCH
  ALLOCATE(TNPSNOW%AL(JP)%DEPTH(ISIZE_P(JP),ISNOW_NLAYER))
ENDDO
!
YSNSURF='DEP'//HSURF
CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE,  &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,&
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,             &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,  &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P        )
!
!* snow layer thickness definition
!
ALLOCATE(ZDEPTH(KL,ISNOW_NLAYER,KPATCH))
!
DO JP = 1,KPATCH
  !
  IF (OSNOW_IDEAL .OR. ISNOW_NLAYER==1 .OR. TNPSNOW%AL(JP)%SCHEME=='3-L') THEN
    ZDEPTH(1:ISIZE_P(JP),:,JP) = TNPSNOW%AL(JP)%DEPTH(:,:)
  ELSEIF (TNPSNOW%AL(1)%SCHEME=='CRO') THEN
    ALLOCATE(ZDTOT(ISIZE_P(JP)))
    ZDTOT(:) = 0.0
    DO JL=1,ISNOW_NLAYER
       ZDTOT(:) = ZDTOT(:) + TNPSNOW%AL(JP)%DEPTH(:,JL)
    END DO
    CALL SNOW3LGRID(ZDEPTH(1:ISIZE_P(JP),:,JP),ZDTOT(:))
    DEALLOCATE(ZDTOT)
  ENDIF
  !
ENDDO
!
DO JP = 1,KPATCH
  DEALLOCATE(TNPSNOW%AL(JP)%DEPTH)
ENDDO
!
!----------------------------------------------------------------------------
!
!*      4.     Snow density profile
!              --------------------
!
!* density profile
YSNSURF='RHO'//HSURF
CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,           &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME, &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,     &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                  &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,       &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH  )  
!
!----------------------------------------------------------------------------
!
!*      5.     Snow water content profile
!              --------------------------
!
IF (.NOT.OSNOW_IDEAL) THEN
  !
  ALLOCATE(ZWRHO(KL))
  ALLOCATE(ZDTOT(KL))
  !
  DO JP = 1,KPATCH
    !
    SK => TNPSNOW%AL(JP)
    !
    ZWRHO(:) = 0.0
    ZDTOT(:) = 0.0
    !
    ISIZE = ISIZE_P(JP)
    !
    !* snow depth estimated from rho profile
    DO JL=1,ISNOW_NLAYER
      WHERE (ZPATCH(1:ISIZE,JP)>0. .AND. SK%RHO(:,JL)/=XUNDEF)
        ZWRHO(1:ISIZE) = ZWRHO(1:ISIZE) + SK%RHO(:,JL) * ZDEPTH(1:ISIZE,JL,JP)
      ELSEWHERE
        ZWRHO(1:ISIZE) = XUNDEF
      END WHERE
    ENDDO
    !
    DO JL = 1,ISNOW_NLAYER
      !* modification of snow depth: coherence between rho profile, total snow and total depth
      WHERE(ZPATCH(1:ISIZE,JP)>0. .AND. ZWRHO(1:ISIZE)/=0. &
           .AND. ZWRHO(1:ISIZE)/=XUNDEF .AND. SK%WSNOW(:,1)>0.0)
        ZDTOT(1:ISIZE) = ZDTOT(1:ISIZE) + ZDEPTH(1:ISIZE,JL,JP) * SK%WSNOW(:,1) / ZWRHO(1:ISIZE)
      ENDWHERE
    END DO
    !
    IF (ISNOW_NLAYER > 1) THEN
      CALL SNOW3LGRID(ZDEPTH(1:ISIZE,:,JP),ZDTOT(1:ISIZE))
    ELSE
      ZDEPTH(1:ISIZE,1,JP) = ZDTOT(1:ISIZE)
    ENDIF
    !
    !* snow content profile for each grid level
    DO JL=1,ISNOW_NLAYER
      WHERE(ZPATCH(1:ISIZE,JP)>0..AND.SK%RHO(:,JL)/=XUNDEF.AND.ZDTOT(1:ISIZE)>0.)
        SK%WSNOW(:,JL) = SK%RHO(:,JL) * ZDEPTH(1:ISIZE,JL,JP)
      ELSEWHERE(ZPATCH(1:ISIZE,JP)>0..AND.(SK%RHO(:,JL)==XUNDEF.OR.ZDTOT(1:ISIZE)==0.0))
        SK%WSNOW(:,JL) = 0.0
      ELSEWHERE
        SK%WSNOW(:,JL) = XUNDEF
      END WHERE
    END DO
    !
  END DO
  !
  DEALLOCATE(ZWRHO)
  DEALLOCATE(ZDTOT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*      6.     Albedo, snow heat content, and age
!              ----------------------------------
!
!* albedo
YSNSURF='ALB'//HSURF
 CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,         &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME, &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH  ) 
!
IF (TNPSNOW%AL(1)%SCHEME/='D95') THEN
  !
  !* heat in snowpack profile
  YSNSURF='HEA'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                           HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,          &
                           KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME, &
                           PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                           PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                           PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                           ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH    )
  !
ENDIF
!
IF (TNPSNOW%AL(1)%SCHEME=='CRO'.OR. TNPSNOW%AL(1)%SCHEME=='3-L') THEN
  !
  !* age in snowpack profile
  YSNSURF='AGE'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH  )   
  !
  DO JP = 1,KPATCH
    WHERE(TNPSNOW%AL(JP)%WSNOW(:,1)>0.0 .AND. TNPSNOW%AL(JP)%WSNOW(:,1)/=XUNDEF .AND. &
          TNPSNOW%AL(JP)%AGE(:,1)==0.0  .AND. TNPSNOW%AL(JP)%ALB(:)<XAGLAMIN)
      TNPSNOW%AL(JP)%ALB(:)=(XAGLAMIN+XAGLAMAX)/2.0
    ENDWHERE
  ENDDO
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*      7.     Crocus specific parameters
!              --------------------------
!
IF (TNPSNOW%AL(1)%SCHEME=='CRO') THEN
  !
  YSNSURF='SG1'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH )   
  !
  YSNSURF='SG2'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH  )   
  !
  YSNSURF='HIS'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, G, U, GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TNPSNOW, TPTIME, &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW, YDCTL,      &
                         ZVEGTYPE_PATCH, ZPATCH, ISIZE_P, IR_P, PDEPTH=ZDEPTH  )   
  !
ENDIF
!
!*      8.     Deallocations
!
DEALLOCATE(ZDEPTH  )
DEALLOCATE(ZVEGTYPE_PATCH)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SNOW_FIELDS
