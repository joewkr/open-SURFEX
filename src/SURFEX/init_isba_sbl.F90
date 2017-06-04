!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE INIT_ISBA_SBL(IO, K, NP, NPE, SB, PTSTEP, PPA, PPS, PTA, PQA, PRHOA, PU, PV, &
                             PDIR_SW, PSCA_SW, PSW_BANDS, PRAIN, PSNOW, PZREF, PUREF, PSSO_SLOPE )  
!     #################################################################################
!
!!****  *INIT_WATER_SBL* - inits water SBL profiles
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
!!     S. Riette
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2010
!!------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_TYPE_SNOW
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_CSTS,             ONLY : XCPD, XRD, XP00, XG, XLVTT
USE MODD_SURF_ATM,         ONLY : LNOSOF
USE MODD_CANOPY_TURB,      ONLY : XALPSBL
!
USE MODI_CLS_TQ
USE MODI_ISBA_SNOW_FRAC
USE MODI_WET_LEAVES_FRAC
USE MODI_VEG
USE MODI_DRAG
USE MODI_CLS_WIND
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
REAL,               INTENT(IN)   :: PTSTEP   ! timestep of the integration
REAL, DIMENSION(:), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(:), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(:,:),INTENT(IN) :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                            !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN) :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                            !                                        (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(:), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(:), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
REAL, DIMENSION(:), INTENT(IN)  :: PSSO_SLOPE! slope of S.S.O.                         (-)
!                                            ! canopy       
!
!*      0.2    declarations of local variables
!
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(ISBA_P_t), POINTER :: PK
!
!* forcing variables
!
REAL, DIMENSION(SIZE(PTA))   :: ZWIND    ! lowest atmospheric level wind speed           (m/s)
REAL, DIMENSION(SIZE(PTA))   :: ZEXNA    ! Exner function at lowest SBL scheme level     (-)
REAL, DIMENSION(SIZE(PTA))   :: ZQA      ! specific humidity                             (kg/m3)
!
! SBL turbulence scheme
!
REAL, DIMENSION(SIZE(PTA))   ::ZRI
REAL, DIMENSION(SIZE(PTA))   ::ZCD
REAL, DIMENSION(SIZE(PTA))   ::ZCDN
REAL, DIMENSION(SIZE(PTA))   ::ZCH
REAL, DIMENSION(SIZE(PTA))   ::ZTNM
REAL, DIMENSION(SIZE(PTA))   ::ZQNM
REAL, DIMENSION(SIZE(PTA))   ::ZHUNM
REAL, DIMENSION(SIZE(PTA))   ::ZP_SLOPE_COS
REAL, DIMENSION(SIZE(PTA))   ::ZZ0
REAL, DIMENSION(SIZE(PTA))   ::ZZ0H
REAL, DIMENSION(SIZE(PTA))   ::ZEXNS
REAL, DIMENSION(SIZE(PTA))   ::ZTS
REAL, DIMENSION(SIZE(PTA))   ::ZHU
REAL, DIMENSION(SIZE(PTA))   ::ZQS
REAL, DIMENSION(SIZE(PTA))   ::ZZ0EFF
REAL, DIMENSION(SIZE(PTA))   ::ZWG
REAL, DIMENSION(SIZE(PTA))   ::ZWGI
REAL, DIMENSION(SIZE(PTA))   ::ZVEG
REAL, DIMENSION(SIZE(PTA))   ::ZRESA
REAL, DIMENSION(SIZE(PTA))   ::ZHUG
REAL, DIMENSION(SIZE(PTA))   ::ZHUGI
REAL, DIMENSION(SIZE(PTA))   ::ZHV
REAL, DIMENSION(SIZE(PTA))   ::ZCPS
REAL, DIMENSION(SIZE(PTA))   ::ZWRMAX_CF
REAL, DIMENSION(SIZE(PTA))   ::ZWR
REAL, DIMENSION(SIZE(PTA))   ::ZZ0_WITH_SNOW
REAL, DIMENSION(SIZE(PTA))   ::ZPSNG
REAL, DIMENSION(SIZE(PTA))   ::ZPSNV
REAL, DIMENSION(SIZE(PTA))   ::ZPSNV_A
REAL, DIMENSION(SIZE(PTA))   ::ZPSN
REAL, DIMENSION(SIZE(PTA))   ::ZSNOWALB
REAL, DIMENSION(SIZE(PTA))   ::ZFFG
REAL, DIMENSION(SIZE(PTA))   ::ZFFGNOS
REAL, DIMENSION(SIZE(PTA))   ::ZFFV
REAL, DIMENSION(SIZE(PTA))   ::ZFFVNOS
REAL, DIMENSION(SIZE(PTA))   ::ZFF
REAL, DIMENSION(SIZE(PTA))   ::ZRS
REAL, DIMENSION(SIZE(PTA))   ::ZP_GLOBAL_SW
REAL, DIMENSION(SIZE(PTA))   ::ZF2
REAL, DIMENSION(SIZE(PTA))   ::ZF5
REAL, DIMENSION(SIZE(PTA))   ::ZLAI
REAL, DIMENSION(SIZE(PTA))   ::ZGAMMA
REAL, DIMENSION(SIZE(PTA))   ::ZRGL
REAL, DIMENSION(SIZE(PTA))   ::ZRSMIN
REAL, DIMENSION(SIZE(PTA))   ::ZDELTA
REAL, DIMENSION(SIZE(PTA))   ::ZWRMAX
REAL, DIMENSION(SIZE(PTA))   ::ZCLS_WIND_ZON
REAL, DIMENSION(SIZE(PTA))   ::ZCLS_WIND_MER
REAL, DIMENSION(SIZE(PTA))   ::ZSUM
REAL, DIMENSION(SIZE(PTA))   :: ZLEG_DELTA  ! soil evaporation delta fn
REAL, DIMENSION(SIZE(PTA))   :: ZLEGI_DELTA ! soil sublimation delta fn
REAL, DIMENSION(SIZE(PTA))   :: ZLVTT
!
REAL, DIMENSION(:,:), ALLOCATABLE ::ZSNOWSWE
REAL, DIMENSION(:,:), ALLOCATABLE ::ZSNOWRHO
REAL, DIMENSION(:,:), ALLOCATABLE ::ZSUM_LAYER
!
INTEGER                     :: JSWB
INTEGER                     :: JL, JI, JP, IMASK
INTEGER :: ISNOW_LAYER
!
REAL, DIMENSION(SIZE(PTA),IO%NPATCH) ::ZWSNOW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_SBL',0,ZHOOK_HANDLE)
!
ZTS (:) = 0.
ZWG (:) = 0.
ZWGI(:) = 0.
ZZ0 (:) = 0.
!
ZZ0H(:) = 0.
ZVEG(:) = 0.
!
ZRESA(:) = 0.
!
ZRGL  (:) = 0.
ZRSMIN(:) = 0.
ZGAMMA(:) = 0.
!
!Means over patches
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)

  DO JI=1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !
    ZTS (IMASK) = ZTS (IMASK) + PEK%XTG (JI,1) * PK%XPATCH(JI)
    ZWG (IMASK) = ZWG (IMASK) + PEK%XWG (JI,1) * PK%XPATCH(JI)
    ZWGI(IMASK) = ZWGI(IMASK) + PEK%XWGI(JI,1) * PK%XPATCH(JI)
    ZZ0 (IMASK) = ZZ0 (IMASK) + PEK%XZ0(JI)    * PK%XPATCH(JI)
    !
    ZZ0H(IMASK) = ZZ0H(IMASK) + PK%XPATCH(JI) * PEK%XZ0 (JI) / PK%XZ0_O_Z0H(JI)
    ZVEG(IMASK) = ZVEG(IMASK) + PK%XPATCH(JI) * PEK%XVEG(JI) 
    !    
    ZRESA(IMASK) = ZRESA(IMASK) + PK%XPATCH(JI) * PEK%XRESA(JI)
    !
    ZRGL  (IMASK) = ZRGL  (IMASK) + PK%XPATCH(JI) * PEK%XRGL  (JI)
    ZRSMIN(IMASK) = ZRSMIN(IMASK) + PK%XPATCH(JI) * PEK%XRSMIN(JI)
    ZGAMMA(IMASK) = ZGAMMA(IMASK) + PK%XPATCH(JI) * PEK%XGAMMA(JI)
    !  
  ENDDO
ENDDO
!
!We choose to set ZZ0EFF and ZZ0_WITH_SNOW equal to ZZ0
ZZ0EFF(:)        = ZZ0(:)
ZZ0_WITH_SNOW(:) = ZZ0(:)
!
!
ZLAI(:) = 0.
ZWRMAX_CF(:) = 0.
ZWR(:) = 0.
!
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)

  DO JI=1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !
    IF (ZVEG(JI)>0.) THEN
      ZLAI     (IMASK) = ZLAI     (IMASK) + PK%XPATCH(JI) * PEK%XVEG(JI) *  PEK%XLAI(JI)
      ZWRMAX_CF(IMASK) = ZWRMAX_CF(IMASK) + PK%XPATCH(JI) * PEK%XVEG(JI) *  PEK%XWRMAX_CF(JI)
      ZWR      (IMASK) = ZWR      (IMASK) + PK%XPATCH(JI) * PEK%XVEG(JI) *  PEK%XWR(JI)
    ELSEIF (JP==1) THEN
      ZLAI     (IMASK) = PEK%XLAI     (JI)
      ZWRMAX_CF(IMASK) = PEK%XWRMAX_CF(JI)
      ZWR      (IMASK) = PEK%XWR      (JI)
    ENDIF
    !
  ENDDO
ENDDO
!
WHERE (ZVEG(:)>0)
  ZLAI     (:)= ZLAI     (:) / ZVEG(:)
  ZWRMAX_CF(:)= ZWRMAX_CF(:) / ZVEG(:)
  ZWR      (:)= ZWR      (:) / ZVEG(:)
ENDWHERE
!
!
ISNOW_LAYER = NPE%AL(1)%TSNOW%NLAYER
ALLOCATE(ZSNOWSWE  (SIZE(PTA),ISNOW_LAYER))
ALLOCATE(ZSUM_LAYER(SIZE(PTA),ISNOW_LAYER))
ALLOCATE(ZSNOWRHO  (SIZE(PTA),ISNOW_LAYER))
ZSNOWSWE  (:,:) = 0.
ZSUM_LAYER(:,:) = 0.
ZSNOWRHO  (:,:) = 0.
!
DO JL=1,ISNOW_LAYER
  !
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)

    DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      !
      IF (PEK%TSNOW%WSNOW(JI,JL)>0.) THEN
        ZSNOWSWE  (IMASK,JL) = ZSNOWSWE  (IMASK,JL) + PK%XPATCH(JI) * PEK%TSNOW%WSNOW(JI,JL)
        ZSUM_LAYER(IMASK,JL) = ZSUM_LAYER(IMASK,JL) + PK%XPATCH(JI)
      ENDIF
      !
    ENDDO
  ENDDO
  !
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)

    DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      !
      IF (ZSUM_LAYER(IMASK,JL)>0.) THEN  
        ZSNOWRHO(IMASK,JL) = ZSNOWRHO(IMASK,JL) + PK%XPATCH(JI) * PEK%TSNOW%RHO(JI,JL)
      ELSEIF (JP==1) THEN
        ZSNOWRHO(IMASK,JL) = PEK%TSNOW%RHO(JI,JL)
      ENDIF
      !
    ENDDO
  ENDDO
  !
END DO
!
WHERE (ZSNOWSWE(:,:)==0.) ZSNOWRHO(:,:) = XUNDEF
WHERE (ZSUM_LAYER(:,:)>0.) 
  ZSNOWRHO(:,:) = ZSNOWRHO(:,:) / ZSUM_LAYER(:,:)
END WHERE
!
ZSUM(:)=SUM(ZSUM_LAYER(:,:),DIM=2)
DEALLOCATE(ZSUM_LAYER)
! 
ZWSNOW(:,:) = 0.
DO JL = 1,ISNOW_LAYER
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)

    DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      !
      ZWSNOW(IMASK,JP) = ZWSNOW(IMASK,JP) + PEK%TSNOW%WSNOW(JI,JL)
      !
    ENDDO
  ENDDO
ENDDO    
!
ZSNOWALB(:) = 0.
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)

  DO JI=1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !

    IF (ZSUM(IMASK)>0.) THEN
      IF (ZWSNOW(IMASK,JP)>0.) THEN
        ZSNOWALB(IMASK) = ZSNOWALB(IMASK) + PK%XPATCH(JI) * PEK%TSNOW%ALB(JI)
      ENDIF
    ELSEIF (JP==1) THEN
      ZSNOWALB(IMASK) = PEK%TSNOW%ALB(JI)
    ENDIF
    !
  ENDDO
ENDDO
!
WHERE(ZSUM(:)>0)         
  ZSNOWALB(:) = ZSNOWALB(:) / ZSUM(:)      
ENDWHERE
!
!
ZEXNA(:) = (PPA(:)/XP00)**(XRD/XCPD)
ZEXNS(:) = (PPS(:)/XP00)**(XRD/XCPD)
ZQA  (:) = PQA(:) / PRHOA(:)
ZWIND(:) = SQRT(PU**2+PV**2)
!
!We compute the snow fractions
 CALL ISBA_SNOW_FRAC(PEK%TSNOW%SCHEME, ZSNOWSWE, ZSNOWRHO, ZSNOWALB,   &
                     ZVEG, ZLAI, ZZ0, ZPSN, ZPSNV_A, ZPSNG, ZPSNV   )  
!
DEALLOCATE(ZSNOWSWE, ZSNOWRHO)
!
!We compute total shortwave incoming radiation needed by veg
ZP_GLOBAL_SW(:) = 0.
DO JSWB=1,SIZE(PSW_BANDS)
  ZP_GLOBAL_SW(:)   = ZP_GLOBAL_SW(:) + (PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB))
END DO
!
!
!We choose the case HPHOTO=='NON' and a humid soil (ZF2=1) to compute ZRS
ZF2(:)=1.0
 CALL VEG(ZP_GLOBAL_SW, PTA, ZQA, PPS, ZRGL, ZLAI, ZRSMIN, ZGAMMA, ZF2, ZRS)
!Calculation of ZDELTA
 CALL WET_LEAVES_FRAC(ZWR, ZVEG, ZWRMAX_CF, ZZ0_WITH_SNOW, ZLAI, ZWRMAX, ZDELTA)
!
!We choose the case LFLOOD=false
ZFFG   (:) = 0.0
ZFFGNOS(:) = 0.0
ZFFV   (:) = 0.0
ZFFVNOS(:) = 0.0
ZFF    (:) = 0.0
!
ZF5    (:) = 1.0
ZLVTT  (:) = XLVTT
!
ZP_SLOPE_COS(:) = 1./SQRT(1.+PSSO_SLOPE(:)**2)
IF (LNOSOF) ZP_SLOPE_COS(:) = 1.0
!
!We compute ZCD, ZCH and ZRI
 CALL DRAG(IO%CISBA, PEK%TSNOW%SCHEME, IO%CCPSURF,  PTSTEP, ZTS, ZWG, ZWGI, &
           ZEXNS, ZEXNA, PTA, ZWIND, ZQA, PRAIN, PSNOW, PPS, ZRS, ZVEG,    &
           ZZ0, ZZ0EFF, ZZ0H, K%XWFC(:,1), K%XWSAT(:,1), ZPSNG, ZPSNV,   &
           PZREF, PUREF, ZP_SLOPE_COS, ZDELTA, ZF5, ZRESA, ZCH, ZCD, ZCDN, &
           ZRI, ZHUG, ZHUGI, ZHV, ZHU, ZCPS, ZQS, ZFFG, ZFFV, ZFF, ZFFGNOS,&
           ZFFVNOS, ZLEG_DELTA, ZLEGI_DELTA, ZWR, PRHOA, ZLVTT            )  
!
!Initialisation of T, Q, Wind and TKE on all canopy levels
DO JL=1,SB%NLVL
  !
  CALL CLS_TQ(PTA, ZQA, PPA, PPS, PZREF, ZCD, ZCH, ZRI, ZTS, ZHU, ZZ0H, &
              SB%XZ(:,JL), ZTNM, ZQNM, ZHUNM           ) 
  ! 
  SB%XT(:,JL)=ZTNM
  SB%XQ(:,JL)=ZQNM
  !
  CALL CLS_WIND(PU, PV, PUREF, ZCD, ZCDN, ZRI, SB%XZ(:,JL), &
                ZCLS_WIND_ZON, ZCLS_WIND_MER                 )
  !
  SB%XU   (:,JL) = SQRT( ZCLS_WIND_ZON(:)**2 + ZCLS_WIND_MER(:)**2 )
  SB%XTKE (:,JL) = XALPSBL * ZCD(:) * ( PU(:)**2 + PV(:)**2 )
  SB%XP   (:,JL) = PPA(:) + XG * PRHOA(:) * (SB%XZ(:,SB%NLVL) - SB%XZ(:,JL))
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_SBL',1,ZHOOK_HANDLE) 
!
END SUBROUTINE INIT_ISBA_SBL
