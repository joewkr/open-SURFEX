!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_VEGETATION_EVOL
CONTAINS
    SUBROUTINE VEGETATION_EVOL(IO, DTI, PK, PEK, OAGRIP, PTSTEP, KMONTH, KDAY, PTIME, &
                               PLAT, PRHOA, P_CO2, ISSK, PRESP_BIOMASS_INST, PSWDIR)
!   ###############################################################
!!****  *VEGETATION EVOL*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of vegetation parameters
!     at solar midnight in the case of interactive vegetation (ISBA-Ags)
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/03/03
!!      P. Le Moigne 12/2004 : NIT version
!!      P Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays
!!      A.L. Gibelin 04/2009 : Add NCB option
!!      D. Carrer    01/2012 : representation of nitrogen dilution fct of CO2 (from Calvet et al. 2008)
!!      B. Decharme  05/2012 : Optimization and ISBA-DIF coupling
!!      C. Delire    01/2014 : IBIS respiration for tropical evergreen
!!      R. Seferian  05/2015 : expanding of Nitrogen dilution option to the complete formulation proposed by Yin et al. GCB 2002
!!Seferian & Delire  06/2015 : accouting for living woody biomass respiration (expanding work of E Joetzjer to all woody PFTs)
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2, XRESPFACTOR_NIT,       &
                                XCOEFF_MAINT_RESP_ZERO, XSLOPE_MAINT_RESP, &
                                XPARCF, XDILUDEC, ITRANSFERT_ESG
USE MODD_CSTS,           ONLY : XDAY, XTT, XMD
!
USE MODI_ALBEDO
USE MODI_LAIGAIN
USE MODI_LAILOSS
USE MODI_NITRO_DECLINE
USE MODI_EMIS_FROM_VEG
USE MODI_VEG_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_SUBSCALE_Z0EFF
USE MODD_TYPE_DATE_SURF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE_ECOSG, NVEGTYPE, &
                                NVT_TEBD, NVT_TRBE, NVT_BONE,   &
                                NVT_TRBD, NVT_TEBE, NVT_TENE,   &
                                NVT_BOBD, NVT_BOND, NVT_SHRB,   &
                                NVT_TRBE
USE MODD_SURF_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
LOGICAL,              INTENT(IN)    :: OAGRIP  ! agricultural practices
!
REAL,                 INTENT(IN)    :: PTSTEP  ! time step
INTEGER,              INTENT(IN)    :: KMONTH  ! current month
INTEGER,              INTENT(IN)    :: KDAY    ! current day
REAL,                 INTENT(IN)    :: PTIME   ! current time since midnight
REAL,   DIMENSION(:), INTENT(IN)    :: PLAT    ! latitude of each grid point
REAL,   DIMENSION(:), INTENT(IN)    :: PRHOA   ! air density
!
REAL,   DIMENSION(:), INTENT(IN)    :: P_CO2 ! CO2 concentration [ppmm]
!
TYPE(SSO_t), INTENT(INOUT) :: ISSK
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/kgair m/s)
!
REAL, DIMENSION(:),   INTENT(IN),   OPTIONAL :: PSWDIR    ! Global incoming shortwave radiation (W m-2)
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER                   :: ZCOEF1 = 10.0
REAL, PARAMETER                   :: ZCOEF2 = 25.0
REAL, PARAMETER                   :: ZDEPTH = 1.0   !Temp depth m
!
REAL, PARAMETER                   :: ZWOOD_IBIS=0.0125
REAL, PARAMETER                   :: ZROOT_IBIS=1.25
REAL, PARAMETER                   :: ZCIBIS1   =3500.
REAL, PARAMETER                   :: ZCIBIS2   =1./288.
REAL, PARAMETER                   :: ZNDAY     =365.
!
REAL, PARAMETER                   :: ZCDILU1 = -0.048
REAL, PARAMETER                   :: ZCDILU2 = 6.3
REAL, PARAMETER                   :: ZCDILU3 = 371.
! Required for Yin et al., nitrogen dilu param
REAL, PARAMETER                   :: ZPHOTON    = 2.010402e-3 ! conversion coef for W m-2 in photon m-2
REAL, PARAMETER                   :: ZDEPTH_VEG = 0.40        !Depth in meters for daily temperature
REAL, PARAMETER                   :: ZTEMP_VEG  = 23.         !Average temperature of the vegetation
REAL, PARAMETER                   :: ZDECIDUS   = 0.75        !Coef for decidus trees
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XRESP_BIOMASS,1),SIZE(PEK%XRESP_BIOMASS,2)) :: ZRESP_BIOMASS_LAST ! biomass at t-1 (kg_DM/m2/day)
REAL,    DIMENSION(SIZE(PEK%XLAI,1))    :: ZBIOMASS_LEAF   ! temporary leaf biomass
REAL,    DIMENSION(SIZE(PEK%XLAI,1))    :: ZBSLAI_NITRO    ! (Calvet et al. 2008) ratio of biomass to LAI
                                                     ! with representation of nitrogen dilution
REAL,    DIMENSION(SIZE(PEK%XLAI,1)) :: ZCO2, ZCNA_NITRO   ! fct of CO2
REAL,    DIMENSION(SIZE(PEK%XLAI,1)) :: ZPARAM
REAL,    DIMENSION(SIZE(PEK%XLAI,1)) :: ZHTREE, ZSAPFRAC   ! tree height & sap fraction used for estimation of
                                                     ! sapwood fraction
!
REAL                              :: ZLOG2, ZWORK
!
REAL, DIMENSION(SIZE(PEK%XTG,1))      :: ZTG_VEG      ! surface temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))      :: ZTG_SOIL     ! soil temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))      :: ZDG_SOIL     ! soil depth for DIF (m)
REAL                              :: ZWGHT_SOIL   ! Weight for DIF (m)
!
LOGICAL, DIMENSION(SIZE(PEK%XLAI,1))    :: GWOOD,GHERB
LOGICAL, DIMENSION(SIZE(PEK%XLAI,1))    :: GMASK_AGRI
LOGICAL                           :: GMASK
INTEGER                           :: INI, INL, JI, JL, IDEPTH, JTYPE
!
REAL,    DIMENSION(SIZE(PK%XVEGTYPE_PATCH,1),SIZE(PK%XVEGTYPE_PATCH,2)) :: ZPARAM_TYPE
!
! * Azote
REAL,    DIMENSION(SIZE(PEK%XLAI,1)) :: ZFERT
!
REAL :: ZDILUDEC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_EVOL',0,ZHOOK_HANDLE)
!
INI=SIZE(PEK%XTG,1)
INL=SIZE(PEK%XTG,2)
!
ZLOG2 = LOG(2.0)
!
ZTG_SOIL(:) = 0.0
ZTG_VEG (:) = 0.0
!
! Define herbaceous and woody patches
GHERB(:) = ( PK%XVEGTYPE_PATCH(:,NVT_TEBD) + PK%XVEGTYPE_PATCH(:,NVT_TRBE) + PK%XVEGTYPE_PATCH(:,NVT_BONE)    &
&          + PK%XVEGTYPE_PATCH(:,NVT_TRBD) + PK%XVEGTYPE_PATCH(:,NVT_TEBE) + PK%XVEGTYPE_PATCH(:,NVT_TENE)    &
&          + PK%XVEGTYPE_PATCH(:,NVT_BOBD) + PK%XVEGTYPE_PATCH(:,NVT_BOND) + PK%XVEGTYPE_PATCH(:,NVT_SHRB)<0.5)
GWOOD(:) = (.NOT.GHERB (:))
!
! Mask where vegetation evolution is performed (just before solar midnight)
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
! Save RESP_BIOMASS at t-1
IF (GMASK) THEN
  PEK%XRESP_BIOMASS(:,1) = 0.0
  ZRESP_BIOMASS_LAST(:,:) = 0.0
ELSE
  PEK%XRESP_BIOMASS(:,1) = PEK%XRESP_BIOMASS(:,1) + PRESP_BIOMASS_INST(:,1) * (PTSTEP*PRHOA(:)*XMC)/(XPCCO2*XMCO2)
  ZRESP_BIOMASS_LAST(:,:) = PEK%XRESP_BIOMASS(:,:)
ENDIF
!
!*      2.     Interactive vegetation
!              ----------------------
!
!  LAI daily mortality and assimilation
!
ZBIOMASS_LEAF(:) = PEK%XBIOMASS(:,1)
!
IF (GMASK) THEN
  !
  PK%XINCREASE (:,:) = 0.0
  PK%XTURNOVER(:,:) = 0.0
  ZBSLAI_NITRO(:  ) = PK%XBSLAI_NITRO(:)
  !
  IF(IO%LNITRO_DILU)THEN
!
!     * Compute Vegetation temperature
!       We use the temperature of the second layer of the soil (<40cm)
!       since the parametrization employs a daily temperature
!
    IF(IO%CISBA/='DIF')THEN
      ZTG_VEG(:) = PEK%XTG(:,2)
    ELSE
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         ZDG_SOIL(JI)=MIN(ZDEPTH_VEG,PK%XDG(JI,IDEPTH))
      ENDDO
      DO JL=1,INL
        DO JI=1,INI
          ZWGHT_SOIL=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_SOIL(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
          ZTG_VEG(JI)=ZTG_VEG(JI)+PEK%XTG(JI,JL)*ZWGHT_SOIL/ZDG_SOIL(JI)
        ENDDO
      ENDDO
    ENDIF
!
    ZPARAM(:) = 0.0
    ZFERT (:) = 0.0
    DO JTYPE=1,SIZE(PK%XVEGTYPE_PATCH,2)
      IF (NVEGTYPE==NVEGTYPE_ECOSG) THEN
        ZDILUDEC = XDILUDEC(ITRANSFERT_ESG(JTYPE))
      ELSE
        ZDILUDEC = XDILUDEC(JTYPE)
      ENDIF
      DO JI = 1,INI
        ZPARAM_TYPE(JI,JTYPE) = ZDILUDEC * (ZDECIDUS + 1.1 * ZPHOTON * XPARCF * PSWDIR(JI)       &
                              + (ZTG_VEG(JI)-XTT)/ZTEMP_VEG - 0.33 * ZFERT(JI))                         &
                              + (1 - ZDILUDEC) * (1.1 * ZPHOTON * XPARCF * PSWDIR(JI) &
                              + (ZTG_VEG(JI)-XTT)/ZTEMP_VEG - 0.33 * ZFERT(JI))
        ZPARAM(JI) = ZPARAM(JI) + ZPARAM_TYPE(JI,JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ENDDO
    ENDDO

    WHERE((PEK%XCE_NITRO(:)*PEK%XCNA_NITRO(:)+PEK%XCF_NITRO(:))/=0.0.AND.PEK%XCNA_NITRO(:)/=0.0)
      ZCO2        (:) = P_CO2(:)*(XMD/(1.E-6*XMCO2))  ! (ppmm ->  ppm)
      ZCNA_NITRO  (:) = PEK%XCNA_NITRO(:) * &
                   EXP(ZCDILU1*EXP(ZPARAM(:)-PEK%XCNA_NITRO(:)/ZCDILU2) * ALOG(MAX(1.,ZCO2(:)/ZCDILU3)))
      ZBSLAI_NITRO(:) = 1. / (PEK%XCE_NITRO(:)*ZCNA_NITRO(:)+PEK%XCF_NITRO(:))
    ENDWHERE
!
  ENDIF
!
  IF(ANY(PEK%XLAI(:)/=XUNDEF))THEN
    CALL NITRO_DECLINE(IO, PK, PEK, GWOOD, ZBSLAI_NITRO, PLAT, ZBIOMASS_LEAF)
    CALL LAIGAIN(ZBSLAI_NITRO, PEK, ZBIOMASS_LEAF)
  ENDIF
  !
  ! reinitialise  PEK%XANDAY(:) and PEK%XANFM(:)
  PEK%XANDAY(:)=0.0
  PEK%XANFM(:) =0.0
  !
ENDIF
!
!
! * soil temperature in K (over 1m depth for DIF)
!
ZTG_VEG(:) = PEK%XTG(:,1)
!
IF(IO%CISBA/='DIF')THEN
  ZTG_SOIL(:) = PEK%XTG(:,2)
ELSE
  DO JI=1,INI
    IDEPTH=PK%NWG_LAYER(JI)
    ZDG_SOIL(JI)=MIN(ZDEPTH,PK%XDG(JI,IDEPTH))
  ENDDO
  DO JL=1,INL
    DO JI=1,INI
      ZWGHT_SOIL=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_SOIL(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
      ZTG_SOIL(JI)=ZTG_SOIL(JI)+PEK%XTG(JI,JL)*ZWGHT_SOIL/ZDG_SOIL(JI)
    ENDDO
  ENDDO
ENDIF
!
!
! * Respiration of structural biomass pools
!
WHERE(GWOOD(:))
  ! IBIS respiration with either respiration factor rwood=0.0125 - otherwise rroot=1.25
  ! (Kucharik et al, 2000, eq 6-8) Soil temp in K
  PEK%XRESP_BIOMASS(:,2) = PEK%XRESP_BIOMASS(:,2) + PEK%XBIOMASS(:,2) * PTSTEP &
                              * MAX(0.,ZROOT_IBIS*EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_VEG(:)))/(ZNDAY*XDAY))
ELSEWHERE
  PEK%XRESP_BIOMASS(:,2) = PEK%XRESP_BIOMASS(:,2) + PEK%XBIOMASS(:,2) * XRESPFACTOR_NIT    &
                              * EXP((ZLOG2/ZCOEF1)*(ZTG_VEG(:)-XTT-ZCOEF2)) * PTSTEP
  ! before optimization                   * 2.0**((PEK%XTG(:,2)-XTT-ZCOEF2)/ZCOEF1) * PTSTEP
ENDWHERE
!
IF (IO%CPHOTO == 'NIT') THEN
  !
  PEK%XRESP_BIOMASS(:,3) = PEK%XRESP_BIOMASS(:,3) + PEK%XBIOMASS(:,3) * XRESPFACTOR_NIT &
                            * EXP((ZLOG2/ZCOEF1)*(ZTG_SOIL(:)-XTT-ZCOEF2)) * PTSTEP
  ! before optimization                   * 2.0**((PEK%XTG(:,2)-XTT-ZCOEF2)/ZCOEF1) * PTSTEP
  !
ELSEIF (IO%CPHOTO == 'NCB') THEN
  !
  PEK%XRESP_BIOMASS(:,2) = MIN(PEK%XRESP_BIOMASS(:,2), PEK%XBIOMASS(:,2))
  !
  PEK%XRESP_BIOMASS(:,3) = PEK%XRESP_BIOMASS(:,3) + PEK%XBIOMASS(:,3) * &
            MAX( 0., XCOEFF_MAINT_RESP_ZERO * (1. + XSLOPE_MAINT_RESP*(ZTG_VEG(:)-XTT))) * PTSTEP
  PEK%XRESP_BIOMASS(:,3) = MIN(PEK%XRESP_BIOMASS(:,3), PEK%XBIOMASS(:,3))
  !
  WHERE(GWOOD(:))
    ! Resp IBIS (Soil temp in K)
    PEK%XRESP_BIOMASS(:,4) = PEK%XRESP_BIOMASS(:,4) + PEK%XBIOMASS(:,4) * PTSTEP &
                        * MAX(0.,ZROOT_IBIS * EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_SOIL(:)))/(ZNDAY*XDAY))
  ELSEWHERE
    PEK%XRESP_BIOMASS(:,4) = PEK%XRESP_BIOMASS(:,4) + PEK%XBIOMASS(:,4) * &
             MAX( 0., XCOEFF_MAINT_RESP_ZERO * (1. + XSLOPE_MAINT_RESP*(ZTG_SOIL(:)-XTT))) * PTSTEP
  ENDWHERE
  !
  PEK%XRESP_BIOMASS(:,4) = MIN(PEK%XRESP_BIOMASS(:,4), PEK%XBIOMASS(:,4))
  !
  WHERE( (GWOOD(:)).AND.(PEK%XBIOMASS(:,5)>0.) )
    ! IBIS estimation of sapwood fraction based on the height of tree, sapspeed and
    ! max transpiration rates. Conversion from DM to C. To be changed with DGVM.  (Soil temp in K)
    ZHTREE(:) = 2.5*0.75*(PEK%XBIOMASS(:,1)+PEK%XBIOMASS(:,2)+PEK%XBIOMASS(:,3)+&
                          PEK%XBIOMASS(:,4)+PEK%XBIOMASS(:,5)+PEK%XBIOMASS(:,6))*0.4
    ZSAPFRAC(:) = MIN(0.5, MAX(0.05,0.0025/25.*ZHTREE(:)*0.75*400/(PEK%XBIOMASS(:,5)*0.4)))
    !ZSAPFRAC(:) = 0.5

    PEK%XRESP_BIOMASS(:,5) = PEK%XRESP_BIOMASS(:,5) + PEK%XBIOMASS(:,5) * ZSAPFRAC(:) * PTSTEP &
                               * MAX(0.,ZWOOD_IBIS*EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_VEG(:)))/(ZNDAY*XDAY))
    PEK%XRESP_BIOMASS(:,5) = MIN(PEK%XRESP_BIOMASS(:,5), PEK%XBIOMASS(:,5))
  ELSEWHERE
    PEK%XRESP_BIOMASS(:,5) = 0.0
  ENDWHERE
  !
ENDIF
!
! * Instantaneous respiration (kgCO2/kgair m/s)
!
DO JL=2,SIZE(PEK%XRESP_BIOMASS(:,:),2)
   PRESP_BIOMASS_INST(:,JL) = (PEK%XRESP_BIOMASS(:,JL) - ZRESP_BIOMASS_LAST(:,JL)) &
                                 * XPCCO2*XMCO2/(PTSTEP*PRHOA(:)*XMC)
ENDDO
!
!*      3.     Agricultural practices
!              ----------------------
!
IF (OAGRIP) THEN
  !
  GMASK_AGRI(:) = .FALSE.
  WHERE ( PEK%TSEED(:)%TDATE%MONTH /= NUNDEF .AND. ( KMONTH < PEK%TSEED(:)%TDATE%MONTH .OR. &
         (KMONTH == PEK%TSEED(:)%TDATE%MONTH .AND. KDAY < PEK%TSEED(:)%TDATE%DAY) ) )  GMASK_AGRI(:) = .TRUE.
  WHERE ( PEK%TREAP(:)%TDATE%MONTH /= NUNDEF .AND. ( KMONTH > PEK%TREAP(:)%TDATE%MONTH .OR. &
         (KMONTH == PEK%TREAP(:)%TDATE%MONTH .AND. KDAY >= PEK%TREAP(:)%TDATE%DAY) ) ) GMASK_AGRI(:) = .TRUE.
  !
  WHERE (GMASK_AGRI(:))
    PEK%XLAI(:)             = PEK%XLAIMIN(:)
    ZBIOMASS_LEAF(:)    = PEK%XLAI(:) * ZBSLAI_NITRO(:)
  END WHERE

  WHERE (GMASK_AGRI(:))
    PEK%XBIOMASS(:,1)       = 0.0
    PEK%XBIOMASS(:,2)       = 0.0
    PEK%XBIOMASS(:,3)       = 0.0
    PEK%XRESP_BIOMASS(:,2)  = 0.0
    PEK%XRESP_BIOMASS(:,3)  = 0.0
  END WHERE
  !
  IF (IO%CPHOTO == 'NCB') THEN
    !
    WHERE (GMASK_AGRI(:))
      PEK%XBIOMASS(:,4)       = 0.0
      PEK%XBIOMASS(:,5)       = 0.0
      PEK%XBIOMASS(:,6)       = 0.0
      PEK%XRESP_BIOMASS(:,4)  = 0.0
    END WHERE
    !
  ENDIF
  !
ENDIF
!
!*      4.     Physical parameters depending on vegetation
!              -------------------------------------------
!
IF (GMASK) THEN
  !
  ! Evolution of vegetation fraction and roughness length due to LAI change
  IF(.NOT.DTI%LIMP_Z0) THEN
    WHERE( PEK%XVEG(:) > 0. ) &
      PEK%XZ0 (:) = Z0V_FROM_LAI(PEK%XLAI(:),PK%XH_TREE(:),PK%XVEGTYPE_PATCH(:,:),IO%LAGRI_TO_GRASS)
  ENDIF
  IF(.NOT.DTI%LIMP_VEG) THEN
    WHERE( PEK%XVEG(:) > 0. ) &
      PEK%XVEG(:) = VEG_FROM_LAI(PEK%XLAI(:),PK%XVEGTYPE_PATCH(:,:),IO%LAGRI_TO_GRASS)
  ENDIF
  !
  ! Evolution of radiative parameters due to vegetation fraction change
  IF(.NOT.DTI%LIMP_EMIS) THEN
    WHERE( PEK%XVEG(:) > 0. ) PEK%XEMIS(:)= EMIS_FROM_VEG(PEK%XVEG(:),PK%XVEGTYPE_PATCH(:,:))
  ENDIF
  !
  CALL ALBEDO(IO%CALBEDO, PEK )
  !
  ! Evolution of effective roughness length due to new surface roughness length
  !
  IF (ASSOCIATED(ISSK%XAOSIP)) THEN
    IF (SIZE(ISSK%XAOSIP)>0) THEN
      CALL SUBSCALE_Z0EFF(ISSK,PEK%XZ0(:),.FALSE. )
    ENDIF
  ENDIF
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_EVOL',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE VEGETATION_EVOL
END MODULE MODI_VEGETATION_EVOL
