!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_ISBA_n (IO, S, KK, PK, PEK, KPATCH, PZENITH, PSW_BANDS, &
                              PDIR_ALB_WITH_SNOW,PSCA_ALB_WITH_SNOW, PEMIST,  &
                              PDIR_SW, PSCA_SW     )
!     ####################################################################
!
!!****  *UPDATE_RAD_ISBA_n * - Calculate snow/flood fraction, dir/dif albedo
!!                             and emissivity at t+1 in order to close the 
!!                             energy budget between the atmospheric model 
!!                             and surfex  
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
!,ZEMIST,PEMIST,ZPUT0)!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      P. Samuelsson 02/2012 MEB
!!      A. Boone      03/2015 MEB-use TR_ML scheme for SW radiation
!!------------------------------------------------------------------
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY: ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_TYPE_SNOW
!
USE MODD_CSTS,      ONLY : XTT
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SNOW_PAR,  ONLY : XRHOSMIN_ES,XRHOSMAX_ES,XSNOWDMIN,XEMISSN
USE MODD_WATER_PAR, ONLY : XALBSCA_WAT, XEMISWAT, XALBWATICE, XEMISWATICE 
USE MODD_MEB_PAR,   ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
!
USE MODE_SURF_FLOOD_FRAC
USE MODE_SURF_SNOW_FRAC      
USE MODE_MEB,       ONLY : MEB_SHIELD_FACTOR, MEBPALPHAN
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_FROM_NIR_VIS
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ISBA_SNOW_FRAC
USE MODI_ISBA_EMIS_MEB
USE MODI_RADIATIVE_TRANSFERT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
INTEGER, INTENT(IN) :: KPATCH
!
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(:,:), INTENT(OUT)  :: PDIR_ALB_WITH_SNOW ! Total direct albedo at t+1
REAL, DIMENSION(:,:), INTENT(OUT)  :: PSCA_ALB_WITH_SNOW ! Total diffuse albedo at t+1
REAL, DIMENSION(:),   INTENT(OUT)  :: PEMIST             ! Total emissivity at t+1
!
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(PK%NSIZE_P) :: ZVEG
REAL, DIMENSION(PK%NSIZE_P) :: ZPSNV_A
REAL, DIMENSION(PK%NSIZE_P) :: ZALBF_DIR
REAL, DIMENSION(PK%NSIZE_P) :: ZALBF_SCA
REAL, DIMENSION(PK%NSIZE_P) :: ZEMISF   
REAL, DIMENSION(PK%NSIZE_P) :: ZFF
REAL, DIMENSION(PK%NSIZE_P) :: ZALBNIR_WITH_SNOW
REAL, DIMENSION(PK%NSIZE_P) :: ZALBVIS_WITH_SNOW
REAL, DIMENSION(PK%NSIZE_P) :: ZALBUV_WITH_SNOW
REAL, DIMENSION(PK%NSIZE_P) :: ZZENITH
REAL, DIMENSION(PK%NSIZE_P) :: ZSNOWDEPTH, ZPALPHAN
REAL, DIMENSION(PK%NSIZE_P) :: ZSWUP
REAL, DIMENSION(PK%NSIZE_P) :: ZGLOBAL_SW
REAL, DIMENSION(PK%NSIZE_P) :: ZALBT, ZEMIST
REAL, DIMENSION(PK%NSIZE_P) :: ZPSNA, ZSIGMA_F, ZSIGMA_FN, ZEMISSN
REAL, DIMENSION(PK%NSIZE_P,SIZE(PSW_BANDS)) :: ZDIR_SW, ZSCA_SW
REAL, DIMENSION(PK%NSIZE_P)            :: ZLAIN, ZALBVIS_TSOIL, ZALBNIR_TSOIL
REAL, DIMENSION(PK%NSIZE_P)            :: ZFAPIR, ZFAPAR, ZFAPIR_BS, ZFAPAR_BS
REAL, DIMENSION(PK%NSIZE_P,SIZE(S%XABC)) :: ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN, ZIACAN
LOGICAL, DIMENSION(PK%NSIZE_P)         :: GSHADE
!
REAL, PARAMETER :: ZPUT0 = 0.0
INTEGER :: ISWB
INTEGER :: JSWB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Initialization
!-------------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N',0,ZHOOK_HANDLE)
ISWB   = SIZE(PSW_BANDS)
!
!-------------------------------------------------------------------------------------
!
ZVEG(:) = PEK%XVEG(:)
!
IF(IO%LMEB_PATCH(KPATCH).OR.IO%LFLOOD)THEN
  !
  CALL PACK_SAME_RANK(PK%NR_P,PZENITH(:),ZZENITH(:))
  !
  IF(IO%LMEB_PATCH(KPATCH))THEN
    !
    ZVEG(:)=0.   ! Set veg=0 for MEB to get bare soil conditions for snow cover and
    !            ! flood fraction
    !
    IF(PRESENT(PDIR_SW))THEN
      !
      CALL PACK_SAME_RANK(PK%NR_P,PDIR_SW(:,:), ZDIR_SW(:,:))
      CALL PACK_SAME_RANK(PK%NR_P,PSCA_SW(:,:), ZSCA_SW(:,:))  
      !
    ENDIF
    !
  ENDIF
  !
ENDIF
!   
!-------------------------------------------------------------------------------
!
 CALL ISBA_SNOW_FRAC(PEK%TSNOW%SCHEME, PEK%TSNOW%WSNOW, PEK%TSNOW%RHO, PEK%TSNOW%ALB, &
                     ZVEG, PEK%XLAI, PEK%XZ0, PEK%XPSN, ZPSNV_A, PEK%XPSNG, PEK%XPSNV )  
!
IF ( PEK%TSNOW%SCHEME=='EBA' ) PEK%XPSNV_A(:) = ZPSNV_A(:)
!
!-------------------------------------------------------------------------------
!
! Flood fractions and properties
!
IF(IO%LFLOOD)THEN  
  !
  KK%XFFG(:) = FLOOD_FRAC_GROUND(PEK%XPSNG,KK%XFFLOOD)
  KK%XFFV(:) = FLOOD_FRAC_VEG   (PEK%XLAI,PEK%XPSNV,KK%XFFLOOD)
  KK%XFF (:) = FLOOD_FRAC_NAT   (PEK%XVEG,KK%XFFG,KK%XFFV,KK%XFFLOOD)
  !
  WHERE(KK%XFFLOOD(:)==0.0)
    ZALBF_DIR  (:) = XUNDEF
    ZALBF_SCA  (:) = XUNDEF
    KK%XALBF   (:) = XUNDEF
    KK%XEMISF  (:) = XUNDEF
    KK%XFFROZEN(:) = 0.0
  ELSEWHERE
    WHERE(PEK%XTG(:,1)>=XTT)
      ZALBF_DIR  (:) = ALBEDO_TA96(ZZENITH(:))
      ZALBF_SCA  (:) = XALBSCA_WAT
      KK%XEMISF  (:) = XEMISWAT
      KK%XFFROZEN(:) = 0.0
    ELSEWHERE
      ZALBF_DIR  (:) = XALBWATICE
      ZALBF_SCA  (:) = XALBWATICE
      KK%XEMISF  (:) = XEMISWATICE
      KK%XFFROZEN(:) = 1.0
    END WHERE
    KK%XALBF(:)=0.5*(ZALBF_DIR(:)+ZALBF_SCA(:))
  ENDWHERE
  !
  ZEMISF(:) = KK%XEMISF(:)
  ZFF   (:) = KK%XFF(:)
  !        
ELSE
  ZALBF_DIR (:)=0.0
  ZALBF_SCA (:)=0.0
  ZEMISF    (:)=0.0
  ZFF       (:)=0.0
ENDIF        
!-------------------------------------------------------------------------------
!
IF(IO%LMEB_PATCH(KPATCH))THEN
  !
  ZSNOWDEPTH(:) = SUM(PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:),2)
  ZPALPHAN  (:) = MEBPALPHAN(ZSNOWDEPTH,PEK%XH_VEG)
  !
  KK%XDIR_ALB_WITH_SNOW(:,:) = XUNDEF
  KK%XSCA_ALB_WITH_SNOW(:,:) = XUNDEF
  !
  IF(PRESENT(PDIR_SW))THEN
    !
    ! Albedo
    !
    ! - just extract some parameters for call, but no need to update 
    !   the cummulative variables in this routine:
    !
    DO JSWB=1,ISWB
      ZGLOBAL_SW(:) = ZDIR_SW(:,JSWB) + ZSCA_SW(:,JSWB)
      !
      WHERE(PEK%TSNOW%ALB(:)/=XUNDEF .AND. PEK%TSNOW%ALBVIS(:)/=XUNDEF .AND. PEK%TSNOW%ALBNIR(:)/=XUNDEF)
        ZLAIN(:)         = PEK%XLAI(:)*(1.0-ZPALPHAN(:))
        ZALBVIS_TSOIL(:) = PEK%XALBVIS_SOIL(:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PEK%TSNOW%ALBVIS(:)
        ZALBNIR_TSOIL(:) = PEK%XALBNIR_SOIL(:)*(1.-PEK%XPSN(:)) + PEK%XPSN(:)*PEK%TSNOW%ALBNIR(:)
      ELSEWHERE
        ZLAIN(:)         = PEK%XLAI(:)
        ZALBVIS_TSOIL(:) = PEK%XALBVIS_SOIL(:)
        ZALBNIR_TSOIL(:) = PEK%XALBNIR_SOIL(:)
      END WHERE
      !
      CALL RADIATIVE_TRANSFERT(IO%LAGRI_TO_GRASS, KK%XVEGTYPE,                   &
              PEK%XALBVIS_VEG, ZALBVIS_TSOIL, PEK%XALBNIR_VEG, ZALBNIR_TSOIL,    &
              ZGLOBAL_SW, ZLAIN, ZZENITH, S%XABC,                                &
              PEK%XFAPARC, PEK%XFAPIRC, PEK%XMUS, PEK%XLAI_EFFC, GSHADE, ZIACAN, &              
              ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN,                            &
              ZFAPAR, ZFAPIR, ZFAPAR_BS, ZFAPIR_BS                               )    

      ! Total effective surface (canopy, ground/flooded zone, snow) all-wavelength
      ! albedo: diagnosed from shortwave energy budget closure.
      ! Final note: purely diagnostic - apply limits for night time

      ZALBT(:)      = 1. - (XSW_WGHT_VIS*(ZFAPAR(:)+ZFAPAR_BS(:)) + XSW_WGHT_NIR*(ZFAPIR(:)+ZFAPIR_BS(:)))
      ZSWUP(:)      = ZGLOBAL_SW(:)*ZALBT(:)
      ZALBT(:)      = ZSWUP(:)/MAX(1.E-5, ZGLOBAL_SW(:))
      !
      KK%XDIR_ALB_WITH_SNOW(:,JSWB)=ZALBT(:)
      KK%XSCA_ALB_WITH_SNOW(:,JSWB)=ZALBT(:) 
      !
    END DO
    !
  ENDIF
  !
  ! Emissivity
  !
  ZEMISSN(:)   = XEMISSN
  ZPSNA  (:)   = 0.
  ZSIGMA_F(:)  = 1.0 - MEB_SHIELD_FACTOR(PEK%XLAI,ZPSNA)
  ZSIGMA_FN(:) = 1.0 - MEB_SHIELD_FACTOR(PEK%XLAI,ZPALPHAN)
  !
  CALL ISBA_EMIS_MEB(PEK%XPSN, ZPALPHAN, ZSIGMA_F, ZSIGMA_FN, ZEMISSN, ZEMIST  )
  !
ELSE
  !        
  !  * albedo for near-infra-red and visible over snow-covered and snow-flood-free surface
  !
  ZALBNIR_WITH_SNOW(:) = PEK%XALBNIR(:) * (1.-PEK%XPSN(:)-ZFF(:)) + PEK%TSNOW%ALB (:) * PEK%XPSN(:)   
  ZALBVIS_WITH_SNOW(:) = PEK%XALBVIS(:) * (1.-PEK%XPSN(:)-ZFF(:)) + PEK%TSNOW%ALB (:) * PEK%XPSN(:)  
  ZALBUV_WITH_SNOW (:) = PEK%XALBUV (:) * (1.-PEK%XPSN(:)-ZFF(:)) + PEK%TSNOW%ALB (:) * PEK%XPSN(:)  
  !
  !  * snow-flood-covered surface albedo for each wavelength (needed for outputs)
  !
  CALL ALBEDO_FROM_NIR_VIS(PSW_BANDS,                                              &
                           ZALBNIR_WITH_SNOW,  ZALBVIS_WITH_SNOW, ZALBUV_WITH_SNOW,&
                           KK%XDIR_ALB_WITH_SNOW, KK%XSCA_ALB_WITH_SNOW          )  
  !
  DO JSWB=1,ISWB
    KK%XDIR_ALB_WITH_SNOW(:,JSWB)=KK%XDIR_ALB_WITH_SNOW(:,JSWB) + ZFF(:)*ZALBF_DIR(:)
    KK%XSCA_ALB_WITH_SNOW(:,JSWB)=KK%XSCA_ALB_WITH_SNOW(:,JSWB) + ZFF(:)*ZALBF_SCA(:)
  ENDDO
  !
  !-------------------------------------------------------------------------------
  !
  ! longwave computations for outputs (emissivity for radiative scheme)
  !
  ZEMIST(:) = (1.-PEK%XPSN(:)-ZFF(:))*PEK%XEMIS(:) + PEK%XPSN(:) * XEMISSN + ZFF(:)*ZEMISF(:)
  !
ENDIF
!
!Update albedo with snow for the next time step
!
 CALL UNPACK_SAME_RANK(PK%NR_P,KK%XDIR_ALB_WITH_SNOW, PDIR_ALB_WITH_SNOW,ZPUT0)
 CALL UNPACK_SAME_RANK(PK%NR_P,KK%XSCA_ALB_WITH_SNOW, PSCA_ALB_WITH_SNOW,ZPUT0)
 CALL UNPACK_SAME_RANK(PK%NR_P,ZEMIST,PEMIST,ZPUT0)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE UPDATE_RAD_ISBA_n
