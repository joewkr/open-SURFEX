!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n (DTV, IO, S, K, KDIM, HPROGRAM)
!     ################################################
!
!!****  *READ_PGD_TEB_GREENROOF_PAR_n* - reads ISBA physiographic fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!!      C. de Munck  02/2012 : added parameterisation for sedum species under NVT_TROG 
!-------------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
!
USE MODD_CSTS,                 ONLY : XDAY
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE, NVT_GRAS, NVT_TROG
!paramètres ci-dessus à initialiser pour les GR (sauf XPAR_OM_GR, XPAR_SAND_GR, XPAR_CLAY_GR qui sont lues) 
USE MODD_PREP_TEB_GREENROOF,   ONLY : NGRID_LEVEL, XGRID_SOIL
!
USE MODI_READ_SURF
USE MODI_VEG_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_EMIS_FROM_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                               :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12)                     :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100)                    :: YCOMMENT       ! Comment string
INTEGER                               :: JI             ! loop index
INTEGER                               :: JTIME          ! loop index
INTEGER                               :: JLAYER         ! loop index
!
LOGICAL :: GAGRI_TO_GRASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    Reading of PGD file
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',0,ZHOOK_HANDLE)
!
GAGRI_TO_GRASS=.FALSE.
!
YRECFM='GR_NTIME'
 CALL READ_SURF(HPROGRAM,YRECFM,DTV%NTIME,IRESP)
!
! Read type of green roof
YRECFM='D_TYPE_GR'
 CALL READ_SURF(HPROGRAM,YRECFM,IO%CTYP_COV,IRESP)
!
DTV%LIMP_VEG=.FALSE.
DTV%LIMP_Z0=.FALSE.
DTV%LIMP_EMIS=.FALSE.
!
! Read green roof OM fraction
DO JLAYER=1,IO%NGROUND_LAYER
  !WRITE(YRECFM,FMT='(A8,I1.1)') 'D_OM_GR0',JLAYER
  WRITE(YRECFM,FMT='(A7,I2.2)') 'D_OM_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSOC(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof SAND fraction
DO JLAYER=1,IO%NGROUND_LAYER
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_SAND_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_SAND_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,K%XSAND(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof CLAY fraction
DO JLAYER=1,IO%NGROUND_LAYER
  !WRITE(YRECFM,FMT='(A10,I1.1)') 'D_CLAY_GR0',JLAYER
  WRITE(YRECFM,FMT='(A9,I2.2)') 'D_CLAY_GR',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,K%XCLAY(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! Read green roof LAI
ALLOCATE(DTV%XPAR_LAI    (KDIM,DTV%NTIME,1))
DO JTIME=1,DTV%NTIME
  WRITE(YRECFM,FMT='(A8,I2.2)') 'D_LAI_GR',JTIME
  CALL READ_SURF(HPROGRAM,YRECFM,DTV%XPAR_LAI(:,JTIME,1),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!-------------------------------------------------------------------------------
!
!*       2.    Definition of ISBA parameters
!              -----------------------------
!
ALLOCATE(DTV%XPAR_VEG        (KDIM,DTV%NTIME,1))
ALLOCATE(DTV%XPAR_RSMIN      (KDIM,1))
ALLOCATE(DTV%XPAR_GAMMA      (KDIM,1))
ALLOCATE(DTV%XPAR_WRMAX_CF   (KDIM,1))
ALLOCATE(DTV%XPAR_RGL        (KDIM,1))
ALLOCATE(DTV%XPAR_CV         (KDIM,1))
ALLOCATE(DTV%XPAR_DG         (KDIM,IO%NGROUND_LAYER,1))
ALLOCATE(DTV%XPAR_ROOTFRAC   (KDIM,IO%NGROUND_LAYER,1))
ALLOCATE(DTV%XPAR_DICE       (KDIM,1))
ALLOCATE(DTV%XPAR_Z0         (KDIM,DTV%NTIME,1))
ALLOCATE(DTV%XPAR_Z0_O_Z0H   (KDIM,1))
ALLOCATE(DTV%XPAR_ALBNIR_VEG (KDIM,1,1))
ALLOCATE(DTV%XPAR_ALBVIS_VEG (KDIM,1,1))
ALLOCATE(DTV%XPAR_ALBUV_VEG  (KDIM,1,1))
ALLOCATE(DTV%XPAR_ALBNIR_SOIL(KDIM,1,1))
ALLOCATE(DTV%XPAR_ALBVIS_SOIL(KDIM,1,1))
ALLOCATE(DTV%XPAR_ALBUV_SOIL (KDIM,1,1))
ALLOCATE(DTV%XPAR_EMIS       (KDIM,DTV%NTIME,1))
ALLOCATE(DTV%XPAR_VEGTYPE    (KDIM,NVEGTYPE))
ALLOCATE(DTV%XPAR_GMES       (KDIM,1))
ALLOCATE(DTV%XPAR_RE25       (KDIM,1))
ALLOCATE(DTV%XPAR_BSLAI      (KDIM,1))
ALLOCATE(DTV%XPAR_LAIMIN     (KDIM,1))
ALLOCATE(DTV%XPAR_SEFOLD     (KDIM,1))
ALLOCATE(DTV%XPAR_GC         (KDIM,1))
ALLOCATE(DTV%XPAR_DMAX       (KDIM,1))
ALLOCATE(DTV%XPAR_F2I        (KDIM,1))
ALLOCATE(DTV%LPAR_STRESS    (KDIM,1))
ALLOCATE(DTV%XPAR_H_TREE     (KDIM,1))
ALLOCATE(DTV%XPAR_CE_NITRO   (KDIM,1))
ALLOCATE(DTV%XPAR_CF_NITRO   (KDIM,1))
ALLOCATE(DTV%XPAR_CNA_NITRO  (KDIM,1))
!
DTV%XPAR_VEG          (:,:,:) = XUNDEF
DTV%XPAR_RSMIN          (:,:) = XUNDEF
DTV%XPAR_GAMMA          (:,:) = XUNDEF
DTV%XPAR_WRMAX_CF       (:,:) = XUNDEF
DTV%XPAR_RGL            (:,:) = XUNDEF
DTV%XPAR_CV             (:,:) = XUNDEF
DTV%XPAR_DG           (:,:,:) = XUNDEF
DTV%XPAR_DICE           (:,:) = XUNDEF
DTV%XPAR_ROOTFRAC     (:,:,:) = XUNDEF
DTV%XPAR_Z0           (:,:,:) = XUNDEF
DTV%XPAR_Z0_O_Z0H       (:,:) = XUNDEF
DTV%XPAR_ALBNIR_VEG     (:,:,:) = XUNDEF
DTV%XPAR_ALBVIS_VEG     (:,:,:) = XUNDEF
DTV%XPAR_ALBUV_VEG      (:,:,:) = XUNDEF
DTV%XPAR_ALBNIR_SOIL    (:,:,:) = XUNDEF
DTV%XPAR_ALBVIS_SOIL    (:,:,:) = XUNDEF
DTV%XPAR_ALBUV_SOIL     (:,:,:) = XUNDEF
DTV%XPAR_EMIS         (:,:,:) = XUNDEF
DTV%XPAR_VEGTYPE      (:,:) = XUNDEF
DTV%XPAR_GMES           (:,:) = XUNDEF
DTV%XPAR_RE25           (:,:) = XUNDEF
DTV%XPAR_BSLAI          (:,:) = XUNDEF
DTV%XPAR_LAIMIN         (:,:) = XUNDEF
DTV%XPAR_SEFOLD         (:,:) = XUNDEF
DTV%XPAR_GC             (:,:) = XUNDEF
DTV%XPAR_DMAX           (:,:) = XUNDEF
DTV%XPAR_F2I            (:,:) = XUNDEF
DTV%LPAR_STRESS        (:,:) = .FALSE.
DTV%XPAR_H_TREE         (:,:) = XUNDEF
DTV%XPAR_CE_NITRO       (:,:) = XUNDEF
DTV%XPAR_CF_NITRO       (:,:) = XUNDEF
DTV%XPAR_CNA_NITRO      (:,:) = XUNDEF
!
!---------------------------------------------------------------------------
! Vegtypes adapted to greenroofs:
!--------------------------------
! NPATCH = 1 
! 2D cases : all greenroofs have same vegetation (defined by CTYP_GR)
! (CTYP_GR == 'GRASS') <=> NVT_GRAS (10)
!  ** OR **
! (CTYP_GR == 'SEDUM') <=> NVT_TROG (11)
! NB1: => no aggregation of vegetype parameters needed 
! NB2: Functions existing for gardens are used for initial greenroofs
!      This will need to be refined specifically for greenroofs
!
DTV%XPAR_VEGTYPE(:,:) = 0.
IF (IO%CTYP_COV == 'GRASS') DTV%XPAR_VEGTYPE(:, NVT_GRAS) = 1.
IF (IO%CTYP_COV == 'SEDUM') DTV%XPAR_VEGTYPE(:, NVT_TROG) = 1.
!--------------------------------------------------------------------------
!
! Critical normilized soil water content for stress parameterisation
DTV%XPAR_F2I(:,:) = 0.3
!
! Ratio between roughness length for momentum and heat
DTV%XPAR_Z0_O_Z0H(:,:) = 10.
!
! Defensive/offensive strategy (1/0)
DTV%LPAR_STRESS(:,:) = .FALSE. 
!
DO JI=1,KDIM
! 
! Vegetation albedo: near-IR, visible, and UV albedo
! * Will need to be adapted to greenroof GRASS and SEDUM species *
! * vérifier si/où l'abedo ds l'UV est utilisé *
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_ALBNIR_VEG(JI,:,:)= 0.3
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_ALBNIR_VEG(JI,:,:)= 0.154 ! mesures ONERA/Doya (2011)

 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_ALBVIS_VEG(JI,:,:)= 0.10
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_ALBVIS_VEG(JI,:,:)= 0.154 ! mesures ONERA/Doya (2011)

 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_ALBUV_VEG(JI,:,:) = 0.0800
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_ALBUV_VEG(JI,:,:) = 0.1250
!
! Min stomatal resistance  
 !IF(XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  XPAR_RSMIN(JI)= 40 (dans isba & garden)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_RSMIN(JI,:)= 120  ! for GRASS
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_RSMIN(JI,:)= 150. ! for SEDUM
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_RSMIN(JI)= 120.
! 
! Gamma parameter 
! (* Check if values needs to be refined for GRASS and SEDUM *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_GAMMA(JI,:)= 0.
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_GAMMA(JI,:)= 0.
!
! Wrmax_cf 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_WRMAX_CF(JI,:)= 0.2
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_WRMAX_CF(JI,:)= 0.2
!
! Rgl 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_RGL(JI,:)= 100.
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_RGL(JI,:)= 100.
!
! Cv 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_CV(JI,:)= 2.E-5
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_CV(JI,:)= 2.E-5
!
!! Mesophyll conductance (m s-1) 
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_GMES(JI,:)= 0.020
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_GMES(JI,:)= 0.020
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )  XPAR_GMES(JI)= 0.003
!
! Ecosystem Respiration (kg/kg.m.s-1)
! (* Check if needs to be refined for GRASS and SEDUM greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0.  )  DTV%XPAR_RE25(JI,:)= 3.0E-7
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG  )>0.)  DTV%XPAR_RE25(JI,:)= 3.0E-7
!
! Cuticular conductance (m s-1)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_GC(JI,:)= 0.00025
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_GC(JI,:)= 0.00025        
!
! Ratio d(biomass)/d(lai) (kg/m2)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_BSLAI(JI,:)= 0.36
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_BSLAI(JI,:)= 0.06
!
! Maximum air saturation deficit tolerate by vegetation (kg/kg)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_DMAX(JI,:)= 0.1
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_DMAX(JI,:)= 0.1
!
! e-folding time for senescence (days)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_SEFOLD(JI,:)=  90.* XDAY
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_SEFOLD(JI,:)=  60.* XDAY
!
! Minimum LAI (m2/m2)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_LAIMIN (JI,:) = 0.3
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_LAIMIN (JI,:) = 0.3
!
! Leaf aera ratio sensitivity to nitrogen concentration
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_CE_NITRO(JI,:)= 5.56
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_CE_NITRO(JI,:)= 3.79
!
! Lethal minimum value of leaf area ratio
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_CF_NITRO(JI,:)=  6.73
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )  DTV%XPAR_CF_NITRO(JI,:)=  9.84
!
! Nitrogen concentration of active biomass
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )  DTV%XPAR_CNA_NITRO(JI,:)= 1.9
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG )>0.)  DTV%XPAR_CNA_NITRO(JI,:)= 1.3
!
! Depth of greenroof ground layers
 DTV%XPAR_DG(JI, 1,:) = XGRID_SOIL(NGRID_LEVEL - 5)
 DTV%XPAR_DG(JI, 2,:) = XGRID_SOIL(NGRID_LEVEL - 4)
 DTV%XPAR_DG(JI, 3,:) = XGRID_SOIL(NGRID_LEVEL - 3)
 DTV%XPAR_DG(JI, 4,:) = XGRID_SOIL(NGRID_LEVEL - 2)
 DTV%XPAR_DG(JI, 5,:) = XGRID_SOIL(NGRID_LEVEL - 1)
 DTV%XPAR_DG(JI, 6,:) = XGRID_SOIL(NGRID_LEVEL - 0)
!
! Root fractions
 DTV%XPAR_ROOTFRAC(JI, 1,:)  = 0.04
 DTV%XPAR_ROOTFRAC(JI, 2,:)  = 0.36
 DTV%XPAR_ROOTFRAC(JI, 3,:)  = 0.68
 DTV%XPAR_ROOTFRAC(JI, 4,:)  = 1.
 DTV%XPAR_ROOTFRAC(JI, 5,:)  = 1.
 DTV%XPAR_ROOTFRAC(JI, 6,:)  = 1.
!
! Depth of the soil column for the calculation of the frozen soil fraction (m)
 DTV%XPAR_DICE(JI,:) = DTV%XPAR_DG(JI,1,:) 
!
DO JTIME=1,DTV%NTIME
! Leaf Area Index

! Fraction of vegetation on greenroof
!* Will need to be refined for greenroofs *)
  !XPAR_VEG (JI,1,JTIME) = VEG_FROM_LAI (XPAR_LAI_GR(JI,JTIME),   &
  !                                       XPAR_VEGTYPE(JI,:),GAGRI_TO_GRASS)  
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTV%XPAR_VEG (JI,JTIME,:) = 0.9
 !IF(XPAR_VEGTYPE(JI,NVT_TROG)>0. )   XPAR_VEG (JI,JTIME) = 1.0
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTV%XPAR_VEG (JI,JTIME,:) = 0.95

! Roughness length for momentum
!* Will need to be refined for greenroofs *)
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTV%XPAR_Z0 (JI,JTIME,:) = 0.01
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTV%XPAR_Z0 (JI,JTIME,:) = 0.01
 !                                        
! Emissivity
!* Will need to be refined for greenroofs *)
  !XPAR_EMIS (JI,1,JTIME) = EMIS_FROM_VEG (XPAR_VEG    (JI,1,JTIME),&
  !                                         XPAR_VEGTYPE(JI,:))  
 IF(DTV%XPAR_VEGTYPE(JI,NVT_GRAS)>0. )   DTV%XPAR_EMIS (JI,JTIME,:) = 0.95 
 IF(DTV%XPAR_VEGTYPE(JI,NVT_TROG)>0. )   DTV%XPAR_EMIS (JI,JTIME,:) = 0.83 ! Feng. et al. (2010)

END DO
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_PAR_n
