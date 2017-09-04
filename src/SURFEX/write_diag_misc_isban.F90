!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MISC_ISBA_n (DTCO, HSELECT, OSNOWDIMNC, U, OPATCH_BUDGET, D,  &
                                         ND, DM, NDM, IO, S, K, NP, TPSNOW, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MISC_ISBA* - writes the ISBA diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      B. Decharme    2008  Total Albedo, Total SWI and Floodplains
!!      B. Decharme 06/2009  key to write (or not) patch result
!!      A.L. Gibelin 04/09 : Add respiration diagnostics
!!      A.L. Gibelin 05/09 : Add carbon spinup
!!      A.L. Gibelin 07/09 : Suppress RDK and transform GPP as a diagnostic
!!      D. Carrer    04/11 : Add FAPAR and effective LAI
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!!      B. Decharme  09/12 : Carbon fluxes in diag_evap
!!      B. Decharme  09/12   New diag for DIF:
!!                           F2 stress
!!                           Root zone swi, wg and wgi
!!                           swi, wg and wgi comparable to ISBA-FR-DG2 and DG3 layers
!!                           active layer thickness over permafrost
!!                           frozen layer thickness over non-permafrost
!!      B. Decharme  06/13   All snow outputs noted SN
!!                           XTSRAD_NAT instead of XAVG_TSRAD
!!                           delete NWG_SIZE
!!                           water table depth
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_NP_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t, DIAG_MISC_ISBA_NP_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t
!
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM
!
USE MODD_SURF_PAR,        ONLY :   NUNDEF, XUNDEF
!
USE MODD_ASSIM, ONLY : LASSIM, CASSIM_ISBA, NVAR, NOBSTYPE, NBOUTPUT
!
USE MODD_AGRI,            ONLY :   LAGRIP
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_FIELD_2D_PATCH
USE MODI_WRITE_FIELD_1D_PATCH
USE MODI_END_IO_SURF_n
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
LOGICAL, INTENT(IN) :: OSNOWDIMNC
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
LOGICAL, INTENT(IN) :: OPATCH_BUDGET
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_NP_t), INTENT(INOUT) :: ND
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DM
TYPE(DIAG_MISC_ISBA_NP_t), INTENT(INOUT) :: NDM
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(SURF_SNOW), INTENT(IN) :: TPSNOW
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=1) :: YVAR, YOBS, YTIM
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YLVL
 CHARACTER(LEN=20) :: YFORM
!
REAL, DIMENSION(SIZE(DM%XSWI,1)) :: ZMAX
INTEGER           :: JL, JJ, JVAR, JOBS, JP, JI, JT, JK, ISIZE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
IF ( DM%LPROSNOW ) THEN
  CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_PROGNOSTIC.OUT.nc')
ELSE
  CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_DIAGNOSTICS.OUT.nc')
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (DM%LSURF_MISC_BUDGET) THEN
  !
  !*       2.     Miscellaneous fields :
  !
  !-------------------------------------------------------------------------------
  !
  !        2.1    Halstead coefficient
  !               --------------------
  !
  YRECFM='HV_ISBA'
  YCOMMENT='Halstead coefficient averaged over tile nature (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XHV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.2    Snow fractions
  !               --------------
  !
  YRECFM='PSNG_ISBA'
  YCOMMENT='snow fraction over ground averaged over tile nature (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XPSNG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSNV_ISBA'
  YCOMMENT='snow fraction over vegetation averaged over tile nature (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XPSNV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSN_ISBA'
  YCOMMENT='total snow fraction averaged over tile nature (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XPSN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.3    Total Albedo and surface temperature
  !               ------------------------------------
  !
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    YRECFM='TS_ISBA'
    YCOMMENT='total surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,D%XTS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSRAD_ISBA'
    YCOMMENT='total radiative surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,S%XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  !        2.4    Soil Wetness Index, Water content and active layer depth
  !               --------------------------------------------------------
  !
  IF(IO%CISBA=='DIF')THEN
    ZMAX(:) = 0.
    !
    DO JP = 1,IO%NPATCH
      DO JI = 1,NP%AL(JP)%NSIZE_P
        JJ = NP%AL(JP)%NR_P(JI)
        !
        IF (NP%AL(JP)%NWG_LAYER(JI)/=NUNDEF.AND.NP%AL(JP)%NWG_LAYER(JI)>ZMAX(JJ)) THEN
          ZMAX(JJ) = NP%AL(JP)%NWG_LAYER(JI)
        ENDIF
      ENDDO
    ENDDO
    !
    DO JJ=1,SIZE(DM%XSWI,1)

      DO JL = 1,IO%NGROUND_LAYER
        IF(JL>ZMAX(JJ))THEN
          DM%XSWI (JJ,JL) = XUNDEF
          DM%XTSWI(JJ,JL) = XUNDEF
        ENDIF
      ENDDO

    ENDDO
  ENDIF
  !
  DO JL=1,IO%NGROUND_LAYER
    !
    WRITE(YLVL,'(I2)') JL
    !
    YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JL >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index for layer ',JL,' (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSWI(:,JL),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JL >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) for layer ',JL,' (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XTSWI(:,JL),IRESP,HCOMMENT=YCOMMENT)
    !
  END DO
  !
  YRECFM='SWI_T_ISBA'
  YCOMMENT='soil wetness index over the soil column (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_SWI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSWI_T_ISBA'
  YCOMMENT='total soil wetness index over the soil column (-)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGTOT_T_ISBA'
  YCOMMENT='total water content (liquid+solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_TWG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGI_T_ISBA'
  YCOMMENT='total ice content (solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGTOT_ISBA'
  YCOMMENT='total volumetric water content (liquid+solid) over the soil column (m3/m3)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_WG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (.NOT.LALLOW_ADD_DIM) THEN
    YRECFM='WGI_ISBA'
    YCOMMENT='total volumetric ice content (solid) over the soil column (m3/m3)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XSOIL_WGI(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(IO%CISBA=='DIF') THEN
    !
    IF (DM%LSURF_MISC_DIF)THEN
      !
      YRECFM='TSWI_D2_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG2 reservoir (-)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD2_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D2_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD2_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D2_ISBA'
      YCOMMENT='ice content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD2_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='TSWI_D3_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG3 reservoir (-)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD3_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D3_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD3_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D3_ISBA'
      YCOMMENT='ice content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFRD3_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
      !
    ENDIF
    !
    YRECFM='ALT_ISBA'
    YCOMMENT='active layer thickness over permafrost (m)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XALT(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FLT_ISBA'
    YCOMMENT='frozen layer thickness over non-permafrost (m)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFLT(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.5    Snow outputs
  !               -------------
  !
  YRECFM='WSN_T_ISBA'
  YCOMMENT='Total_snow_reservoir (kg/m2)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XTWSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DSN_T_ISBA'
  YCOMMENT='Total_snow_depth (m)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XTDSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSN_T_ISBA'
  YCOMMENT='Total_snow_temperature (K)'
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XTTSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (TPSNOW%SCHEME=='CRO' .AND. DM%LPROSNOW) THEN
    !
    YCOMMENT='accumulated snow thickness for past 1 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SD_1DY_ISBA',DM%XSNDPT_1DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow thickness for past 3 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SD_3DY_ISBA',DM%XSNDPT_3DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow thickness for past 5 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SD_5DY_ISBA',DM%XSNDPT_5DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow thickness for past 7 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SD_7DY_ISBA',DM%XSNDPT_7DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow water equivalent for past 1 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SWE_1DY_ISBA',DM%XSNSWE_1DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow water equivalent for past 3 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SWE_3DY_ISBA',DM%XSNSWE_3DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow water equivalent for past 5 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SWE_5DY_ISBA',DM%XSNSWE_5DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='accumulated snow water equivalent for past 7 days'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SWE_7DY_ISBA',DM%XSNSWE_7DY(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='Penetration of ram resistance sensor (2 daN)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'RAMSOND_ISBA',DM%XSNRAM_SONDE(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='Thickness of wet snow at the top of the snowpack'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'WET_TH_ISBA',DM%XSN_WETTHCKN(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YCOMMENT='Thickness of refrozen snow at the top of the snowpack'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'REFRZTH_ISBA',DM%XSN_REFRZNTHCKN(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.6    SGH scheme
  !               ----------
  !
  IF(IO%CRUNOFF=='SGH '.OR.IO%CRUNOFF=='DT92')THEN
    YRECFM='FSAT_ISBA'
    YCOMMENT='Soil saturated fraction (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFSAT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(IO%CRAIN=='SGH ')THEN
    YRECFM='MUF_ISBA'
    YCOMMENT='fraction of the grid cell reached by the rainfall (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XMUF(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !        2.7    Flooding scheme
  !               ---------------
  !
  IF(IO%LFLOOD)THEN
    !
    YRECFM='FFG_ISBA'
    YCOMMENT='flood fraction over ground averaged over tile nature (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFFG(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFV_ISBA'
    YCOMMENT='flood fraction over vegetation averaged over tile nature (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFFV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FF_ISBA'
    YCOMMENT='total flood fraction averaged over tile nature (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XFF(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFLOOD_ISBA'
    YCOMMENT='Grid-cell potential flood fraction (-)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XFFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PIFLOOD_ISBA'
    YCOMMENT='Grid-cell Potential_floodplain_infiltration (kg/m2/s)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XPIFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.8    Total LAI
  !               ---------
  !
  IF(IO%CPHOTO/='NON'.OR.IO%NPATCH>1)THEN
    YRECFM='LAI_ISBA'
    YCOMMENT='leaf area index (m2/m2)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DM%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !        2.9    Water table depth
  !               -----------------
  !
  IF(IO%LWTD)THEN
    !
    YRECFM='FWTD_ISBA'
    YCOMMENT='grid-cell fraction of water table to rise'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XFWTD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WTD_ISBA'
    YCOMMENT='water table depth from RRM model or observation (m)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XWTD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !*       3.     Miscellaneous fields for each patch :
  !               -------------------------------------
  !
  ISIZE = U%NSIZE_NATURE
  !
  !----------------------------------------------------------------------------
  !User wants (or not) patch output
  IF(OPATCH_BUDGET .AND. IO%NPATCH>1)THEN
    !----------------------------------------------------------------------------
    !
    !        3.1    Soil Wetness Index and active layer depth
    !               -----------------------------------------
    !
    DO JL=1,IO%NGROUND_LAYER
      !
      WRITE(YLVL,'(I2)') JL
      !
      YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//'_'
      YFORM='(A39,I1.1,A4)'
      IF (JL >= 10)  YFORM='(A39,I2.2,A4)'
      WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index per patch for layer ',JL,' (-)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XSWI(:,JL),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//'_'
      YFORM='(A39,I1.1,A4)'
      IF (JL >= 10)  YFORM='(A39,I2.2,A4)'
      WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) per patch for layer ',JL,' (-)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XTSWI(:,JL),ISIZE,S%XWORK_WR)
      ENDDO
      !
    END DO
    !
    IF(IO%CISBA=='DIF')THEN
      !

      YRECFM='ALT_'
      YCOMMENT='active layer thickness over permafrost per patch (m)'
      WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) per patch for layer ',JL,' (-)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XALT(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YRECFM='FLT_'
      YCOMMENT='frozen layer thickness over non-permafrost per patch (m)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XFLT(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
    ENDIF
    !
    !        3.2    Snow fractions
    !               --------------
    !
    YRECFM='PSNG_'
    YCOMMENT='snow fraction per patch over ground '
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XPSNG(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='PSNV_'
    YCOMMENT='snow fraction per patch over vegetation'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XPSNV(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='PSN_'
    YCOMMENT='total snow fraction per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XPSN(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    !        3.3    SGH scheme
    !               ----------
    !
    IF(IO%CRUNOFF=='SGH '.OR.IO%CRUNOFF=='DT92')THEN
      YRECFM='FSAT_'
      YCOMMENT='Soil saturated fraction per patch (-)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XFSAT(:),ISIZE,S%XWORK_WR)
      ENDDO
    ENDIF
    !
    !        3.3    Flood fractions
    !               --------------
    !
    IF(IO%LFLOOD)THEN
      !
      YRECFM='FFG_'
      YCOMMENT='flood fraction per patch over ground '
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XFFG(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YRECFM='FFV_'
      YCOMMENT='flood fraction per patch over vegetation'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XFFV(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YRECFM='FF_'
      YCOMMENT='total flood fraction per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,NDM%AL(JP)%XFF(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
    ENDIF
    !
    !        3.4    Total Albedo
    !               ------------
    !
    !
    IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
      !
      YRECFM='TS_'
      YCOMMENT='total surface temperature (isba+snow) per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,ND%AL(JP)%XTS(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YRECFM='TSRAD_'
      YCOMMENT='total radiative surface temperature (isba+snow) per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
            NP%AL(JP)%NR_P,ND%AL(JP)%XTSRAD(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
    ENDIF
    !
    !        3.5    Halstead coefficient
    !               --------------------
    !
    YRECFM='HV_'
    YCOMMENT='Halstead coefficient per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XHV(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    !        3.6  Snow outputs
    !        -----------------
    !
    YRECFM='WSN_T_'
    YCOMMENT='X_Y_WSNOW_TOT (kg/m2) per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XTWSNOW(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='DSN_T_'
    YCOMMENT='X_Y_DSNOW_TOT (m) per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XTDSNOW(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='TSN_T_'
    YCOMMENT='X_Y_TSNOW_TOT (k) per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XTTSNOW(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    IF (TPSNOW%SCHEME=='CRO' .AND. DM%LPROSNOW) THEN
      !
      YCOMMENT='accumulated snow thickness for past 1 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SD_1DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNDPT_1DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT=   'accumulated snow thickness for past 3 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SD_3DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNDPT_3DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT=  'accumulated snow thickness for past 5 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SD_5DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNDPT_5DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='accumulated snow thickness for past 7 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SD_7DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNDPT_7DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='accumulated snow water equivalent for past 1 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SWE_1DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNSWE_1DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='accumulated snow water equivalent for past 3 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SWE_3DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNSWE_3DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='accumulated snow water equivalent for past 5 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SWE_5DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNSWE_5DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='accumulated snow water equivalent for past 7 days per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'SWE_7DY_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNSWE_7DY(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='Penetration of ram resistance sensor (2 daN) per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'RAMSOND_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNRAM_SONDE(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='Thickness of wet snow at the top of the snowpack per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'WET_TH_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSN_WETTHCKN(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
      YCOMMENT='Thickness of refrozen snow at the top of the snowpack per patch'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,'REFRZTH_',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSN_REFRZNTHCKN(:),ISIZE,S%XWORK_WR)
      ENDDO
      !
    ENDIF
    !
  ENDIF
  !
  IF((OPATCH_BUDGET.AND. IO%NPATCH>1).OR.IO%NPATCH==1)THEN
    !
    IF ( OSNOWDIMNC ) THEN
      !
      IF ( DM%LVOLUMETRIC_SNOWLIQ ) THEN
        YCOMMENT='snow liquid water (kg m-3)'
      ELSE
        YCOMMENT='snow liquid water (m)'
      ENDIF
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWLIQ',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWLIQ(:,:),ISIZE,'snow_layer',S%XWSN_WR)
      ENDDO
      !
      YCOMMENT='snow temperature (K)'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWTEMP',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWTEMP(:,:),ISIZE,'snow_layer',S%XWSN_WR)
      ENDDO
      !
      YCOMMENT=  'snow layer thickness'
      DO JP=1,IO%NPATCH
        CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWDZ',YCOMMENT,JP,&
              NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWDZ(:,:),ISIZE,'snow_layer',S%XWSN_WR)
      ENDDO
      !
      IF (TPSNOW%SCHEME=='CRO' .AND. DM%LPROSNOW) THEN
        !
        YCOMMENT=  'snow layer dendricity'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWDEND',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWDEND(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer sphericity'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWSPHER',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWSPHER(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer grain size'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWSIZE',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWSIZE(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer specific surface area'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWSSA',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWSSA(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer grain type'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWTYPE',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWTYPEMEPRA(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer ram resistance'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWRAM',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWRAM(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
        YCOMMENT='snow layer shear resistance'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,'SNOWSHEAR',YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWSHEAR(:,:),ISIZE,'snow_layer',S%XWSN_WR)
        ENDDO
        !
      ENDIF
      !
    ELSE
      !
      DO JL=1,TPSNOW%NLAYER
        !
       WRITE(YLVL,'(I2)') JL
        !
        YRECFM='SNOWLIQ'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A18,I1.1,A9)'
        IF (JL >= 10)  YFORM='(A18,I2.2,A9)'
        IF ( DM%LVOLUMETRIC_SNOWLIQ ) THEN
          WRITE(YCOMMENT,YFORM) 'snow liquid water ',JL,' (kg m-3)'
        ELSE
          WRITE(YCOMMENT,YFORM) 'snow liquid water ',JL,' (m)     '
        ENDIF
        WRITE(YCOMMENT,FMT=YFORM) 'snow liquid water',JL,' (m)'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWLIQ(:,JL),ISIZE,S%XWORK_WR)
        ENDDO
        !
        YRECFM='SNOWTEMP'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A16,I1.1,A4)'
        IF (JL >= 10)  YFORM='(A16,I2.2,A4)'
        WRITE(YCOMMENT,FMT=YFORM) 'snow temperature',JL,' (K)'
        DO JP=1,IO%NPATCH
          CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
                NP%AL(JP)%NR_P,NDM%AL(JP)%XSNOWTEMP(:,JL),ISIZE,S%XWORK_WR)
        ENDDO
        !
      END DO
      !
    ENDIF
    !
  ENDIF
  !
  IF (LAGRIP) THEN
    !
    !        2.8    Irrigation threshold
    !               --------------------
    !
    YRECFM='IRRISEUIL'
    YCOMMENT='irrigation threshold per patch'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XSEUIL(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
  ENDIF
  !
  IF (IO%LTR_ML) THEN
    !
    YRECFM='FAPAR'
    YCOMMENT='FAPAR (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XFAPAR(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='FAPIR'
    YCOMMENT='FAPIR (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XFAPIR(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='FAPAR_BS'
    YCOMMENT='FAPAR_BS (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XFAPAR_BS(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='FAPIR_BS'
    YCOMMENT='FAPIR_BS (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XFAPIR_BS(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='DFAPARC'
    YCOMMENT='DFAPARC (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XDFAPARC(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='DFAPIRC'
    YCOMMENT='DFAPIRC (-)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XDFAPIRC(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
    YRECFM='DLAI_EFFC'
    YCOMMENT='DLAI_EFFC (m2/m2)'
    DO JP=1,IO%NPATCH
      CALL WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,JP,&
          NP%AL(JP)%NR_P,NDM%AL(JP)%XDLAI_EFFC(:),ISIZE,S%XWORK_WR)
    ENDDO
    !
  ENDIF
  !
  IF (LASSIM .AND. CASSIM_ISBA=="EKF  ") THEN
    !
    CALL END_IO_SURF_n(HPROGRAM)
    CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_ANALYSIS.OUT.nc')
    !
    DO JVAR = 1,NVAR
      WRITE(YVAR,FMT='(I1.1)') JVAR
      YRECFM="ANA_INCR"//YVAR
      YCOMMENT="by patch"
      DO JP = 1,IO%NPATCH
        CALL WRITE_FIELD_1D_PATCH(HSELECT, HPROGRAM, YRECFM, YCOMMENT, JP,&
                                NP%AL(JP)%NR_P,NP%AL(JP)%XINCR(1:NP%AL(JP)%NSIZE_P,JVAR),ISIZE,S%XWORK_WR)
      ENDDO
      WRITE(YVAR,FMT='(I1.1)') JVAR
      DO JT = 1,NBOUTPUT
        WRITE(YTIM,FMT='(I1.1)') JT
        DO JOBS = 1,NOBSTYPE
          WRITE(YOBS,FMT='(I1.1)') JOBS
          YRECFM="HO"//YVAR//"_"//YOBS//"_"//YTIM
          YCOMMENT="by patch"
          JK = (JT-1)*NOBSTYPE + JOBS
          DO JP = 1,IO%NPATCH
            CALL WRITE_FIELD_1D_PATCH(HSELECT, HPROGRAM, YRECFM, YCOMMENT, JP,&
                                      NP%AL(JP)%NR_P,NP%AL(JP)%XHO(1:NP%AL(JP)%NSIZE_P,JK,JVAR),ISIZE,S%XWORK_WR)
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    !
    DO JT = 1,NBOUTPUT
      WRITE(YTIM,FMT='(I1.1)') JT
      DO JOBS = 1,NOBSTYPE
        WRITE(YOBS,FMT='(I1.1)') JOBS
        YRECFM="INNOV"//YOBS//"_"//YTIM
        YCOMMENT="not by patch"
        JK = (JT-1)*NOBSTYPE + JOBS
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XINNOV(:,JK),IRESP,HCOMMENT=YCOMMENT)
      ENDDO
    ENDDO
    !
    DO JT = 1,NBOUTPUT
      WRITE(YTIM,FMT='(I1.1)') JT
      DO JOBS = 1,NOBSTYPE
        WRITE(YOBS,FMT='(I1.1)') JOBS
        YRECFM="RESID"//YOBS//"_"//YTIM
        YCOMMENT="not by patch"
        JK = (JT-1)*NOBSTYPE + JOBS
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XRESID(:,JK),IRESP,HCOMMENT=YCOMMENT)
      ENDDO
    ENDDO
    !
  ENDIF
  !
ENDIF
!         End of IO
!
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DIAG_MISC_ISBA_n
