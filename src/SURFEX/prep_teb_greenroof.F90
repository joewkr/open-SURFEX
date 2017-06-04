!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GREENROOF (DTCO, UG, U, USS, GCP, TG, TOP, IO, S, K, P, PEK,  &
                               HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!     #################################################################################
!
!!****  *PREP_TEB_GREENROOF* - Prepares ISBA fields for greenroofs
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    Based on "prep_teb_garden"
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODI_PREP_HOR_TEB_GREENROOF_FIELD
USE MODI_PREP_VER_TEB_VEG
!
                                ! A FAIRE :
                                ! IL FAUT RAJOUTER TSNOW
                                ! ----------------------
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
USE MODD_CSTS,           ONLY : XTT
USE MODD_SNOW_PAR,       ONLY : XZ0SN
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_CO2V_PAR,       ONLY : XCC_NIT, XCA_NIT, XANFMINIT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE MODN_PREP_ISBA
USE MODE_POS_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
TYPE(GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
INTEGER,            INTENT(IN)  :: KPATCH
!
!*      0.2    declarations of local variables
!
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!*      1.1    Default
!
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.1    Soil Water reservoirs
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.6    LAI
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, &
                                   HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitations: 
!
! 3.1  If whole ice reservoir is empty (grib from ecmwf case) and surface temperature is
!      lower than -10C, then ice content is maximum and water content minimum
!
IF (ALL(PEK%XWGI(:,:)==0.)) THEN
   WHERE(PEK%XTG(:,1:SIZE(PEK%XWG,2)) < XTT-10.)
      PEK%XWGI(:,:) = K%XWSAT(:,:)-XWGMIN
      PEK%XWG (:,:) = XWGMIN
   END WHERE
ENDIF
!
!
! 3.2.  Total water content should not exceed saturation:
WHERE(PEK%XWG(:,:) /= XUNDEF .AND. (PEK%XWG(:,:) + PEK%XWGI(:,:)) > K%XWSAT(:,:) )
   PEK%XWGI(:,:) = K%XWSAT(:,:) - PEK%XWG(:,:)
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      4.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_TEB_VEG(P, PEK, IO, TOP%XZS)
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields
!
ALLOCATE(PEK%XRESA(SIZE(PEK%XLAI)))
PEK%XRESA(:) = 100.
!
!-------------------------------------------------------------------------------------
!
!*      6.     Isba-Ags prognostic fields
!
IF (IO%CPHOTO /= 'NON') THEN
!
   ALLOCATE(PEK%XAN(SIZE(PEK%XLAI)))
   PEK%XAN = 0.
!
   ALLOCATE(PEK%XANDAY(SIZE(PEK%XLAI)))
   PEK%XANDAY = 0.
!
   ALLOCATE(PEK%XANFM(SIZE(PEK%XLAI)))
   PEK%XANFM = XANFMINIT
!
   ALLOCATE(PEK%XLE(SIZE(PEK%XLAI)))
   PEK%XLE = 0.
!
ENDIF
!
IF (IO%CPHOTO == 'AST') THEN
!
   ALLOCATE(PEK%XBIOMASS(SIZE(PEK%XLAI),IO%NNBIOMASS))
   PEK%XBIOMASS(:,1) = 0.
!
   ALLOCATE(PEK%XRESP_BIOMASS(SIZE(PEK%XLAI),IO%NNBIOMASS))
   PEK%XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (IO%CPHOTO == 'NIT' .OR. IO%CPHOTO == 'NCB') THEN
!
   ALLOCATE(PEK%XBIOMASS(SIZE(PEK%XLAI),IO%NNBIOMASS))
   PEK%XBIOMASS(:,1) = PEK%XLAI(:) * P%XBSLAI_NITRO(:)
   PEK%XBIOMASS(:,2) = MAX( 0., (PEK%XBIOMASS(:,1)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - PEK%XBIOMASS(:,1) )  
   PEK%XBIOMASS(:,3:IO%NNBIOMASS) = 0.
!
   ALLOCATE(PEK%XRESP_BIOMASS(SIZE(PEK%XLAI),IO%NNBIOMASS))
   PEK%XRESP_BIOMASS(:,:) = 0.
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_GREENROOF
