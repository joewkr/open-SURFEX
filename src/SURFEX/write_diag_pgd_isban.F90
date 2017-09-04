!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_PGD_ISBA_n (DTCO, HSELECT, U, CHI, NCHI, OSURF_DIAG_ALBEDO, &
                                        IO, S, K, NP, NPE, ISS, HPROGRAM)
!     #########################################
!
!!****  *WRITE_DIAG_PGD_ISBA_n* - writes the ISBA physiographic diagnostic fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    10/2004 by P. Le Moigne: add XZ0REL, XVEGTYPE_PATCH
!!      Modified    11/2005 by P. Le Moigne: limit length of VEGTYPE_PATCH field names
!!      Modified    11/2013 by B. Decharme : XPATCH now in writesurf_isban.F90
!!      Modified    10/2014 by P. Samuelsson: MEB variables
!!      Modified    06/2014 by B. Decharme : add XVEGTYPE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t, CH_ISBA_NP_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_AGRI,       ONLY : LAGRIP
!
!
USE MODD_IO_SURF_FA, ONLY : LFANOCOMPACT, LPREP
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_WRITE_FIELD_1D_PATCH
USE MODI_WRITE_TFIELD_1D_PATCH
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(CH_ISBA_NP_t), INTENT(INOUT) :: NCHI
LOGICAL, INTENT(IN) :: OSURF_DIAG_ALBEDO
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(SSO_t), INTENT(INOUT) :: ISS
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
REAL, DIMENSION(U%NSIZE_NATURE,IO%NPATCH) :: ZWORK
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK1
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK2
!
REAL, DIMENSION(U%NSIZE_NATURE,SIZE(NP%AL(1)%XDG,2)) :: ZDG   ! Work array
REAL, DIMENSION(U%NSIZE_NATURE) :: ZDG2
REAL, DIMENSION(U%NSIZE_NATURE) :: ZDTOT
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YLVLV, YPAS
CHARACTER(LEN=4)  :: YLVL
 CHARACTER(LEN=2) :: YPAT
!
INTEGER         :: JI, JL, JP, ILAYER, ILU, IMASK
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',0,ZHOOK_HANDLE)
!
ILU = U%NSIZE_NATURE
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_VEG_EVOLUTION.OUT.nc')
!
!-------------------------------------------------------------------------------
!
!* Leaf Area Index
!
IF (IO%CPHOTO=='NON' .OR. IO%CPHOTO=='AST') THEN
  !
  YRECFM='LAI'
  YCOMMENT='leaf area index (-)'
  !
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XLAI(:),ILU,S%XWORK_WR)
  ENDDO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Vegetation fraction
!
YRECFM='VEG'
YCOMMENT='vegetation fraction (-)'
!
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XVEG(:),ILU,S%XWORK_WR)
ENDDO
!
!* Surface roughness length (without snow)
!
YRECFM='Z0VEG'
YCOMMENT='surface roughness length (without snow) (m)'
!
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XZ0(:),ILU,S%XWORK_WR)
ENDDO
!
IF (ISIZE_LMEB_PATCH>0) THEN
  !
  YRECFM='GNDLITTER'
  YCOMMENT='MEB: ground litter fraction (-)'
  !
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XGNDLITTER(:),ILU,S%XWORK_WR)
ENDDO
  !
  YRECFM='Z0LITTER'
  YCOMMENT='MEB: ground litter roughness length (without snow) (m)'
  !
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XZ0LITTER(:),ILU,S%XWORK_WR)
ENDDO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Soil depth for each patch
!
DO JL=1,SIZE(NP%AL(1)%XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A2,I1)') 'DG',JL
  ELSE
    WRITE(YRECFM,FMT='(A2,I2)') 'DG',JL
  ENDIF
  YCOMMENT='soil depth'//' (M)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NP%AL(JP)%XDG(:,JL),ILU,S%XWORK_WR)
ENDDO
END DO
!
!* Averaged Soil depth
!
IF(IO%NPATCH>1)THEN
!
  ZDG(:,:)=0.0
  DO JP=1,IO%NPATCH
    PK => NP%AL(JP)
    DO JL=1,SIZE(PK%XDG,2)
      DO JI=1, PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZDG(IMASK,JL) = ZDG(IMASK,JL) + PK%XPATCH(JI)*PK%XDG(JI,JL)
      ENDDO
    ENDDO
  ENDDO
!
  DO JL=1,SIZE(NP%AL(1)%XDG,2)
    WRITE(YLVL,'(I4)')JL
    YRECFM='DG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='averaged soil depth layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//' (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZDG(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
IF(IO%CISBA=='DIF')THEN
  !
  ALLOCATE(ZWORK2(ILU,IO%NPATCH))
  !
  ZDG2 (:)=0.0
  ZDTOT(:)=0.0
  ZWORK2(:,:)=XUNDEF
  DO JP=1,IO%NPATCH
    PK => NP%AL(JP)
     DO JI=1,PK%NSIZE_P
       IMASK = PK%NR_P(JI)
       ZDG2(IMASK) = ZDG2(IMASK) + PK%XPATCH(JI) * PK%XDG2(JI)
        JL = PK%NWG_LAYER(JI)
        IF(JL/=NUNDEF)THEN
          ZWORK2(IMASK,JP) = PK%XDG(JI,JL)
          ZDTOT(IMASK) = ZDTOT(IMASK) + PK%XPATCH(JI) * PK%XDG(JI,JL)
        ENDIF
     ENDDO
  ENDDO
  !
  !* Root depth
  !
  YRECFM='DROOT_DIF'
  YCOMMENT='Root depth in ISBA-DIF'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NP%AL(JP)%XDROOT(:),ILU,S%XWORK_WR)
  ENDDO
  !
  YRECFM='DG2_DIF'
  YCOMMENT='DG2 depth in ISBA-DIF'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NP%AL(JP)%XDG2(:),ILU,S%XWORK_WR)
  ENDDO
  !
  IF(IO%NPATCH>1)THEN
    YRECFM='DG2_DIF_ISBA'
    YCOMMENT='Averaged DG2 depth in ISBA-DIF'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZDG2(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !* Runoff depth
  !
  YRECFM='RUNOFFD'
  YCOMMENT='Runoff deph in ISBA-DIF'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NP%AL(JP)%XRUNOFFD(:),ILU,S%XWORK_WR)
  ENDDO
  !
  !* Total soil depth for mositure
  !
    YCOMMENT='Total soil depth for moisture in ISBA-DIF'
  DO JP = 1,IO%NPATCH
    WRITE(YPAT,'(I2)') JP
    YRECFM='DTOT_DIF'
    YRECFM=ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//'P'//ADJUSTL(YPAT(:LEN_TRIM(YPAT)))
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK2(:,JP),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
  DEALLOCATE(ZWORK2)
  !
  IF(IO%NPATCH>1)THEN
    YRECFM='DTOTDF_ISBA'
    YCOMMENT='Averaged Total soil depth for moisture in ISBA-DIF'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZDTOT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !* Root fraction for each patch
  !
  ALLOCATE(ZWORK1(ILU))
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    DO JL=1,SIZE(PK%XROOTFRAC,2)
      IF (JL<10) THEN
        WRITE(YRECFM,FMT='(A8,I1)') 'ROOTFRAC',JL
      ELSE
        WRITE(YRECFM,FMT='(A8,I2)') 'ROOTFRAC',JL
      ENDIF
      YCOMMENT='root fraction by layer (-)'
      ZWORK1(:)=XUNDEF
      DO JI=1,SIZE(PK%XDG,1)
        IF(JL<=PK%NWG_LAYER(JI).AND.PK%NWG_LAYER(JI)/=NUNDEF) THEN
          ZWORK1(JI) = PK%XROOTFRAC(JI,JL)
        ENDIF
      ENDDO
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,ZWORK1(1:PK%NSIZE_P),ILU,S%XWORK_WR)
    ENDDO
  END DO
  DEALLOCATE(ZWORK1)
  !
  !* SOC fraction for each layer
  !
  IF(IO%LSOC)THEN
    DO JL=1,SIZE(NP%AL(1)%XDG,2)
     IF (JL<10) THEN
       WRITE(YRECFM,FMT='(A7,I1)') 'FRACSOC',JL
     ELSE
       WRITE(YRECFM,FMT='(A7,I2)') 'FRACSOC',JL
     ENDIF
     YCOMMENT='SOC fraction by layer (-)'
     CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XFRACSOC(:,JL),IRESP,HCOMMENT=YCOMMENT)
    ENDDO
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
DO JL=1,SIZE(NP%AL(1)%XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A4,I1)') 'WSAT',JL
  ELSE
    WRITE(YRECFM,FMT='(A4,I2)') 'WSAT',JL
  ENDIF
  YCOMMENT='soil porosity by layer (m3/m3)'
  CALL WRITE_SURF(HSELECT, &
                  HPROGRAM,YRECFM,K%XWSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(NP%AL(1)%XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A3,I1)') 'WFC',JL
  ELSE
    WRITE(YRECFM,FMT='(A3,I2)') 'WFC',JL
  ENDIF
  YCOMMENT='field capacity by layer (m3/m3)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XWFC(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(NP%AL(1)%XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A5,I1)') 'WWILT',JL
  ELSE
    WRITE(YRECFM,FMT='(A5,I2)') 'WWILT',JL
  ENDIF
  YCOMMENT='wilting point by layer (m3/m3)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XWWILT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
!-------------------------------------------------------------------------------
! For Earth System Model
IF(LFANOCOMPACT.AND..NOT.LPREP)THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
YRECFM='Z0REL'
YCOMMENT='orography roughness length (M)'
!
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ISS%XZ0REL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* Runoff soil ice depth for each patch
!
IF(IO%CHORT=='SGH'.AND.IO%CISBA/='DIF')THEN
  YRECFM='DICE'
  YCOMMENT='soil ice depth for runoff (m)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NP%AL(JP)%XD_ICE(:),ILU,S%XWORK_WR)
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type in the grid cell
!
DO JL=1,SIZE(S%XVEGTYPE_PATCH,2)
  WRITE(YPAS,'(I2)') JL
  YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  WRITE(YRECFM,FMT='(A9)') 'VEGTYPE'//YLVLV
  YCOMMENT='fraction of each vegetation type in the grid cell'//' (-)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XVEGTYPE(:,JL),IRESP,HCOMMENT=YCOMMENT)
END DO
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type for each patch
!
IF(IO%NPATCH>1.AND.SIZE(S%XVEGTYPE_PATCH,2)/=SIZE(S%XVEGTYPE_PATCH,3))THEN
!
  DO JL=1,SIZE(S%XVEGTYPE_PATCH,2)
    WRITE(YPAS,'(I2)') JL
    YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    WRITE(YRECFM,FMT='(A9)') 'VEGTY_'//YLVLV
    YCOMMENT='fraction of each vegetation type in each patch'//' (-)'
    DO JP = 1,IO%NPATCH
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                  NP%AL(JP)%NR_P,NP%AL(JP)%XVEGTYPE_PATCH(:,JL),ILU,S%XWORK_WR)
    ENDDO
  END DO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* other surface parameters
!
YRECFM='RSMIN'
YCOMMENT='minimum stomatal resistance (sm-1)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XRSMIN(:),ILU,S%XWORK_WR)
ENDDO
!
YRECFM='GAMMA'
YCOMMENT='coefficient for RSMIN calculation (-)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XGAMMA(:),ILU,S%XWORK_WR)
ENDDO
!
YRECFM='CV'
YCOMMENT='vegetation thermal inertia coefficient (-)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XCV(:),ILU,S%XWORK_WR)
ENDDO
!
YRECFM='RGL'
YCOMMENT='maximum solar radiation usable in photosynthesis (-)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XRGL(:),ILU,S%XWORK_WR)
ENDDO
!
YRECFM='EMIS_ISBA'
YCOMMENT='surface emissivity (-)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XEMIS(:),ILU,S%XWORK_WR)
ENDDO
!
YRECFM='WRMAX_CF'
YCOMMENT='coefficient for maximum water interception (-)'
DO JP = 1,IO%NPATCH
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NPE%AL(JP)%XWRMAX_CF(:),ILU,S%XWORK_WR)
ENDDO
!
IF (ISIZE_LMEB_PATCH>0) THEN
  !
  YRECFM='H_VEG'
  YCOMMENT='MEB: height of vegetation (m)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XH_VEG(:),ILU,S%XWORK_WR)
  ENDDO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (OSURF_DIAG_ALBEDO) THEN
!
!* Soil albedos
!
!
  YRECFM='ALBNIR_S'
  YCOMMENT='soil near-infra-red albedo (-)'
  DO JP=1,IO%NPATCH
    CALL UNPACK_SAME_RANK(NP%AL(JP)%NR_P, NPE%AL(JP)%XALBNIR_SOIL, ZWORK(:,JP))
    WHERE (ZWORK(:,JP)/=XUNDEF) ZWORK(:,1) = ZWORK(:,JP)
  ENDDO
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
  YRECFM='ALBVIS_S'
  YCOMMENT='soil visible albedo (-)'
  DO JP=1,IO%NPATCH
    CALL UNPACK_SAME_RANK(NP%AL(JP)%NR_P, NPE%AL(JP)%XALBVIS_SOIL, ZWORK(:,JP))
    WHERE (ZWORK(:,JP)/=XUNDEF) ZWORK(:,1) = ZWORK(:,JP)
  ENDDO
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
  YRECFM='ALBUV_S'
  YCOMMENT='soil UV albedo (-)'
  DO JP=1,IO%NPATCH
    CALL UNPACK_SAME_RANK(NP%AL(JP)%NR_P, NPE%AL(JP)%XALBUV_SOIL, ZWORK(:,JP))
    WHERE (ZWORK(:,JP)/=XUNDEF) ZWORK(:,1) = ZWORK(:,JP)
  ENDDO
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK(:,1),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* albedos
!
  YRECFM='ALBNIR'
  YCOMMENT='total near-infra-red albedo (-)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XALBNIR(:),ILU,S%XWORK_WR)
  ENDDO
!
!-------------------------------------------------------------------------------
!
  YRECFM='ALBVIS'
  YCOMMENT='total visible albedo (-)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XALBVIS(:),ILU,S%XWORK_WR)
  ENDDO
!
!-------------------------------------------------------------------------------
!
  YRECFM='ALBUV'
  YCOMMENT='total UV albedo (-)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XALBUV(:),ILU,S%XWORK_WR)
  ENDDO
!
END IF
!
!-------------------------------------------------------------------------------
!
!* chemical soil resistances
!
IF (CHI%CCH_DRY_DEP=='WES89' .AND. CHI%SVI%NBEQ>0) THEN
  YRECFM='SOILRC_SO2'
  YCOMMENT='bare soil resistance for SO2 (?)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NCHI%AL(JP)%XSOILRC_SO2(:),ILU,S%XWORK_WR)
  ENDDO
  !
  YRECFM='SOILRC_O3'
  YCOMMENT='bare soil resistance for O3 (?)'
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NCHI%AL(JP)%XSOILRC_O3(:),ILU,S%XWORK_WR)
  ENDDO
END IF
!
!-------------------------------------------------------------------------------
!
IF (LAGRIP .AND. (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') ) THEN
!
!* seeding and reaping
!
  YRECFM='TSEED'
  YCOMMENT='date of seeding (-)'
  !
  DO JP = 1,IO%NPATCH
    CALL  WRITE_TFIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%TSEED(:),ILU,S%TDATE_WR)
  ENDDO
!
  YRECFM='TREAP'
  YCOMMENT='date of reaping (-)'
!
  DO JP = 1,IO%NPATCH
    CALL  WRITE_TFIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%TREAP(:),ILU,S%TDATE_WR)
  ENDDO
!
!-------------------------------------------------------------------------------
!
!* irrigated fraction
!
  YRECFM='IRRIG'
  YCOMMENT='flag for irrigation (irrigation if >0.) (-)'
!
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XIRRIG(:),ILU,S%XWORK_WR)
  ENDDO
!
!-------------------------------------------------------------------------------
!
!* water supply for irrigation
!
  YRECFM='WATSUP'
  YCOMMENT='water supply during irrigation process (mm)'
!
  DO JP = 1,IO%NPATCH
    CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NPE%AL(JP)%XWATSUP(:),ILU,S%XWORK_WR)
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_PGD_ISBA_n
