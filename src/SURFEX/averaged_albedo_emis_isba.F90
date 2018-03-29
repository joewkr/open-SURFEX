!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_AVERAGED_ALBEDO_EMIS_ISBA
CONTAINS
      SUBROUTINE AVERAGED_ALBEDO_EMIS_ISBA (IO, S, NK, NP, NPE, &
                                 PZENITH, PTG1, PSW_BANDS, PDIR_ALB, PSCA_ALB, &
                                 PEMIS, PTSRAD, PTSURF, PDIR_SW, PSCA_SW        )
!     ###################################################
!
!!**** ** computes radiative fields used in ISBA
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!!     A. Bogatchev 09/2005 EBA snow option
!!     B. Decharme  2008    The fraction of vegetation covered by snow must be
!                            <= to ZSNG
!!     B. Decharme  2013    new coupling variable and optimization
!!     P. Samuelsson 10/2014 MEB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY: ISBA_S_t, ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t, ISBA_K_t, &
                       ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_TYPE_SNOW
!
USE MODD_CSTS,      ONLY : XSTEFAN
USE MODE_MEB,       ONLY : MEBPALPHAN
!
USE MODI_ALBEDO
USE MODI_AVERAGE_RAD
USE MODI_UPDATE_RAD_ISBA_n
USE MODI_ISBA_LWNET_MEB
USE MODI_UNPACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
REAL, DIMENSION(:,:),   INTENT(IN)   :: PTG1        ! soil surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH
REAL, DIMENSION(:),     INTENT(IN)   :: PSW_BANDS   ! middle wavelength of each band
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB    ! averaged direct albedo  (per wavelength)
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB    ! averaged diffuse albedo (per wavelength)
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS       ! averaged emissivity
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSRAD      ! averaged radiaitve temp.
REAL, DIMENSION(:),     INTENT(OUT)  :: PTSURF      ! surface effective temperature         (K)
!
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PDIR_SW ! Downwelling direct SW radiation
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PSCA_SW ! Downwelling diffuse SW radiation
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
REAL, DIMENSION(SIZE(PZENITH),SIZE(PSW_BANDS),IO%NPATCH) :: ZDIR_ALB_PATCH
!                                                     ! direct albedo
REAL, DIMENSION(SIZE(PZENITH),SIZE(PSW_BANDS),IO%NPATCH) :: ZSCA_ALB_PATCH
!                                                     ! diffuse albedo
REAL, DIMENSION(SIZE(PZENITH),IO%NPATCH) :: ZEMIS_PATCH   ! emissivity with snow-flood
REAL, DIMENSION(SIZE(PZENITH),IO%NPATCH) :: ZTSRAD_PATCH  ! Tsrad
REAL, DIMENSION(SIZE(PZENITH),IO%NPATCH) :: ZTSURF_PATCH  ! Tsurf
REAL, DIMENSION(SIZE(PTG1,1)) :: ZEMIS         ! emissivity with flood
!
REAL, DIMENSION(SIZE(PTG1,1)) :: ZSNOWDEPTH    ! Total snow depth
REAL, DIMENSION(SIZE(PTG1,1)) :: ZPALPHAN      ! Snow/canopy ratio factor
REAL, DIMENSION(SIZE(PTG1,1)) :: ZLW_RAD       ! Fake downwelling LW rad
REAL, DIMENSION(SIZE(PTG1,1)) :: ZLW_UP        ! Upwelling LW rad
REAL, DIMENSION(SIZE(PTG1,1)) :: ZLWNET_N      ! LW net for snow surface
REAL, DIMENSION(SIZE(PTG1,1)) :: ZLWNET_V      ! LW net for canopy veg
REAL, DIMENSION(SIZE(PTG1,1)) :: ZLWNET_G      ! LW net for ground
REAL, DIMENSION(SIZE(PTG1,1)) :: ZDUMMY
REAL, DIMENSION(SIZE(PTG1,1)) :: ZEMISF
REAL, DIMENSION(SIZE(PTG1,1)) :: ZFF
!
LOGICAL :: LEXPLICIT_SNOW ! snow scheme key
!
INTEGER :: JP, JI,ISIZE ! loop on patches
INTEGER :: IMASK ! loop on patches
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    0.      Init
!             ----
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_EMIS_ISBA',0,ZHOOK_HANDLE)
!
PDIR_ALB(:,:)=0.
PSCA_ALB(:,:)=0.
PEMIS   (:)  =0.
PTSRAD  (:)  =0.
PTSURF  (:)  =0.
!
ZDIR_ALB_PATCH(:,:,:)=0.
ZSCA_ALB_PATCH(:,:,:)=0.
ZEMIS_PATCH   (:,:  )=0.
!
LEXPLICIT_SNOW = (NPE%AL(1)%TSNOW%SCHEME=='3-L'.OR.NPE%AL(1)%TSNOW%SCHEME=='CRO')
!
ZTSRAD_PATCH(:,:) = 0.
ZTSURF_PATCH(:,:) = 0.
DO JP = 1,IO%NPATCH
  DO JI = 1,NP%AL(JP)%NSIZE_P
    IMASK = NP%AL(JP)%NR_P(JI)
    ZTSRAD_PATCH (IMASK,JP) = PTG1(JI,JP)
    ZTSURF_PATCH (IMASK,JP) = PTG1(JI,JP)
  ENDDO
ENDDO
!
!
!*    1.      averaged albedo on natural continental surfaces (except prognostic snow)
!             -----------------------------------------------
!
DO JP = 1,IO%NPATCH
  CALL ALBEDO(IO%CALBEDO, NPE%AL(JP) )
ENDDO
!
!*    2.      averaged albedo and emis. on natural continental surfaces (with prognostic snow)
!             ---------------------------------------------------------
!
! A dummy downwelling LW radiation can be used for calculation of radiative surface temp
!
ZLW_RAD(:) = 300.0
!
!* Initialization of albedo for each wavelength, emissivity and snow/flood fractions
!
DO JP = 1,IO%NPATCH
  !
  IF(PRESENT(PDIR_SW))THEN
    !
    ! For the case when MEB patch albedo is requested downweeling SW is needed
    !
    CALL UPDATE_RAD_ISBA_n(IO, S, NK%AL(JP), NP%AL(JP), NPE%AL(JP), JP, PZENITH, PSW_BANDS,   &
                           ZDIR_ALB_PATCH(:,:,JP), ZSCA_ALB_PATCH(:,:,JP), ZEMIS_PATCH(:,JP), &
                           PDIR_SW, PSCA_SW    )
  ELSE
    !
    ! For cases when MEB patch albedo is not requested no downweeling SW is needed
    !
    CALL UPDATE_RAD_ISBA_n(IO, S, NK%AL(JP), NP%AL(JP), NPE%AL(JP), JP, PZENITH, PSW_BANDS, &
                           ZDIR_ALB_PATCH(:,:,JP), ZSCA_ALB_PATCH(:,:,JP), ZEMIS_PATCH(:,JP))
    !
  ENDIF
  !
ENDDO
!
!
!* radiative surface temperature
!
DO JP  =  1,IO%NPATCH
  !
  PEK => NPE%AL(JP)
  PK => NP%AL(JP)
  KK => NK%AL(JP)
  !
  ISIZE = PK%NSIZE_P
  !
  IF(IO%LMEB_PATCH(JP))THEN  ! MEB patches
    !
    !   ZPALPHAN is needed as input to ISBA_LWNET_MEB
    !
    ZSNOWDEPTH(1:ISIZE) = SUM(PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:),2)
    ZPALPHAN  (1:ISIZE) = MEBPALPHAN(ZSNOWDEPTH(1:ISIZE),PEK%XH_VEG(:))
    !
    !   ZLWNET_N,ZLWNET_V,ZLWNET_G are needed for ZLW_UP and ZTSRAD_PATCH
    !
    IF(IO%LFLOOD)THEN
      ZEMISF(1:ISIZE) = KK%XEMISF(:)
      ZFF   (1:ISIZE) = KK%XFF   (:)
    ELSE
      ZEMISF(1:ISIZE) = XUNDEF
      ZFF   (1:ISIZE) = 0.0
    ENDIF
    !
    CALL ISBA_LWNET_MEB(PEK%XLAI, PEK%XPSN, ZPALPHAN(1:ISIZE), PEK%TSNOW%EMIS, &
                        ZEMISF(1:ISIZE), ZFF(1:ISIZE),           &
                        PEK%XTV, PTG1(1:ISIZE,JP), PEK%TSNOW%TS,       &
                        ZLW_RAD(1:ISIZE), ZLWNET_N(1:ISIZE), ZLWNET_V(1:ISIZE), ZLWNET_G(1:ISIZE),   &
                        ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), &
                        ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), &
                        ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE), ZDUMMY(1:ISIZE)   )
    !
    ZLW_UP(1:ISIZE)  = ZLW_RAD(1:ISIZE) - (ZLWNET_V(1:ISIZE) + ZLWNET_G(1:ISIZE) + ZLWNET_N(1:ISIZE))
    !
    !   MEB patch radiative temperature
    !
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      IF (ZEMIS_PATCH(IMASK,JP)/=0.) THEN
        ZTSRAD_PATCH(IMASK,JP) = ((ZLW_UP(JI) - ZLW_RAD(JI)*(1.0-ZEMIS_PATCH(IMASK,JP)))/ &
                                 (XSTEFAN*ZEMIS_PATCH(IMASK,JP)))**0.25
      ENDIF
    END DO
    !
  ELSE   ! Non-MEB patches
    !
    ZEMIS(1:ISIZE) = PEK%XEMIS(:)
    !
    IF(IO%LFLOOD.AND.LEXPLICIT_SNOW)THEN
      WHERE(PEK%XPSN(:)<1.0.AND.PEK%XEMIS(:)/=XUNDEF)
        ZEMIS(1:ISIZE) = ((1.-KK%XFF(:)-PEK%XPSN(:))*PEK%XEMIS(:) + KK%XFF(:)*KK%XEMISF(:)) /(1.-PEK%XPSN(:))
      ENDWHERE
    ENDIF
    !
    IF (.NOT.LEXPLICIT_SNOW) THEN
      CALL UNPACK_SAME_RANK(PK%NR_P,PTG1(1:PK%NSIZE_P,JP),ZTSRAD_PATCH(:,JP),0.)
    ELSE IF (LEXPLICIT_SNOW) THEN
      DO JI = 1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF (PEK%XEMIS(JI)/=XUNDEF .AND. ZEMIS_PATCH(IMASK,JP)/=0.) THEN
          ZTSRAD_PATCH(IMASK,JP) =( ( (1.-PEK%XPSN(JI)) * ZEMIS(JI)*PTG1(JI,JP)**4            &
                                     +    PEK%XPSN(JI) *PEK%TSNOW%EMIS(JI)*PEK%TSNOW%TS(JI)**4 ) )**0.25  &
                                 / ZEMIS_PATCH(IMASK,JP)**0.25
        END IF
      ENDDO
    END IF

  ENDIF
!
END DO
!
!* averaged radiative fields
!
 CALL AVERAGE_RAD(S%XPATCH,                                                  &
                  ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTSRAD_PATCH, &
                  PDIR_ALB,       PSCA_ALB,       PEMIS,       PTSRAD        )
!
!* averaged effective temperature
!
IF(LEXPLICIT_SNOW)THEN
  DO JP = 1,IO%NPATCH
    PEK => NPE%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZTSURF_PATCH(IMASK,JP) = PTG1(JI,JP)*(1.-PEK%XPSN(JI)) + PEK%TSNOW%TS(JI)*PEK%XPSN(JI)
    ENDDO
  ENDDO
ENDIF
!
DO JP=1,IO%NPATCH
  DO JI=1,NP%AL(JP)%NSIZE_P
    IMASK = NP%AL(JP)%NR_P(JI)
    PTSURF(IMASK) = PTSURF(IMASK) + NP%AL(JP)%XPATCH(JI) * ZTSURF_PATCH(IMASK,JP)
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_EMIS_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_ALBEDO_EMIS_ISBA
END MODULE MODI_AVERAGED_ALBEDO_EMIS_ISBA
