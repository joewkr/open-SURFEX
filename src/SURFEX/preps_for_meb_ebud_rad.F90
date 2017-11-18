!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!   ############################################################################
MODULE MODI_PREPS_FOR_MEB_EBUD_RAD
CONTAINS
SUBROUTINE PREPS_FOR_MEB_EBUD_RAD(PPS,                                         &
     PLAICV,PSNOWRHO,PSNOWSWE,PSNOWHEAT,PSNOWLIQ,                              &
     PSNOWTEMP,PSNOWDZ,PSCOND,PHEATCAPS,PEMISNOW,PSIGMA_F,PCHIP,               &
     PTSTEP,PSR,PTA,PVMOD,PSNOWAGE,PPERMSNOWFRAC                               )
!   ############################################################################
!
!!****  *PREPS_FOR_MEB_EBUD_RAD*
!!
!!    PURPOSE
!!    -------
!
!     Get preliminary estimates of certain parameters needed for energy budget
!     solution of snowpack, and some other misc inputs needed by radiation
!     routines for MEB.
!
!!**  METHOD
!!    ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!	A. Boone                * CNRM-GAME, Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_CSTS,                ONLY : XTT, XLMTT, XRHOLW
!
USE MODD_SNOW_PAR,            ONLY : XRHOSMAX_ES, XRHOSMIN_ES, XEMISSN
!
USE MODD_SNOW_METAMO,         ONLY : XSNOWDZMIN
!
USE MODD_SURF_PAR,            ONLY : XUNDEF
!
USE MODE_SNOW3L,              ONLY : SNOW3LTHRM, SNOW3LSCAP
!
USE MODE_MEB,                 ONLY : MEB_SHIELD_FACTOR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1    Declaration of Arguments
!
REAL                                :: PTSTEP   ! time step (s)
REAL, DIMENSION(:),   INTENT(IN)    :: PLAICV
REAL, DIMENSION(:),   INTENT(IN)    :: PPS
REAL, DIMENSION(:),   INTENT(IN)    :: PSR
REAL, DIMENSION(:),   INTENT(IN)    :: PTA
REAL, DIMENSION(:),   INTENT(IN)    :: PVMOD
REAL, DIMENSION(:),   INTENT(IN)    :: PPERMSNOWFRAC
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWHEAT

REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWSWE, PSNOWAGE, PSNOWRHO

REAL, DIMENSION(:),   INTENT(OUT)   :: PSIGMA_F, PCHIP
REAL, DIMENSION(:),   INTENT(OUT)   :: PEMISNOW
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSNOWDZ, PSCOND, PHEATCAPS, PSNOWTEMP, PSNOWLIQ
!
!
!*      0.2    declarations of local variables
!
INTEGER                                            :: JI, JK
REAL, DIMENSION(SIZE(PLAICV,1))                    :: ZPSNA
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEAT, ZSNOWDZN
REAL, DIMENSION(SIZE(PTA))                         :: ZSNOW, ZSNOWHMASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------
! 0) Initialization
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_EBUD_RAD',0,ZHOOK_HANDLE)
!
! Here, as in snow3l (ISBA-ES), we account for several processes
! on the snowpack before surface energy budget computations
! (i.e. snowfall on albedo, density, thickness, and compaction etc...)
!
! First, since snow might not exist, check using snow density as a
! proxy (since several computations below depend on this value). Note
! that if snow doesn't exist, the variables below depending on snow
! density are either zero (e.g. PSNOWDZ) or unused (e.g. PSCOND)
!
WHERE(PSNOWRHO(:,:)==XUNDEF)
   PSNOWRHO(:,:) = XRHOSMIN_ES
ENDWHERE
!
PSNOWDZ(:,:)     = PSNOWSWE(:,:)/PSNOWRHO(:,:) ! diagnose current layer thicknesses (m)
!
! Local working:
!
ZSNOWHEAT(:,:)   = PSNOWHEAT(:,:)*PSNOWSWE(:,:)/PSNOWRHO(:,:) ! J/m3 to J/m2

ZSNOW(:)         = 0.0
DO JK=1,SIZE(PSNOWDZ,2)
   DO JI=1,SIZE(PSNOWDZ,1)
      ZSNOW(JI)  = ZSNOW(JI) + PSNOWDZ(JI,JK)
   ENDDO
ENDDO
!
 CALL SNOW3LFALL(PTSTEP,PSR,PTA,PVMOD,ZSNOW,PSNOWRHO,PSNOWDZ,          &
                 ZSNOWHEAT,ZSNOWHMASS,PSNOWAGE,PPERMSNOWFRAC )

 CALL SNOW3LGRID(ZSNOWDZN,ZSNOW,PSNOWDZ_OLD=PSNOWDZ)

 CALL SNOW3LTRANSF(ZSNOW,PSNOWDZ,ZSNOWDZN,PSNOWRHO,ZSNOWHEAT,PSNOWAGE)

! Snow heat capacity:
!
PHEATCAPS(:,:)   = SNOW3LSCAP(PSNOWRHO)                    ! J m-3 K-1
!
! Snow temperature (K)
!
PSNOWTEMP(:,:)   = XTT + ( ((ZSNOWHEAT(:,:)/MAX(1.E-10,PSNOWDZ(:,:)))  &
                   + XLMTT*PSNOWRHO(:,:))/PHEATCAPS(:,:) )
!
PSNOWLIQ(:,:)    = MAX(0.0,PSNOWTEMP(:,:)-XTT)*PHEATCAPS(:,:)*         &
                   PSNOWDZ(:,:)/(XLMTT*XRHOLW)

PSNOWTEMP(:,:)   = MIN(XTT,PSNOWTEMP(:,:))

! SWE:

PSNOWSWE(:,:)  = PSNOWDZ(:,:)*PSNOWRHO(:,:)

 CALL SNOW3LCOMPACTN(PTSTEP,XSNOWDZMIN,PSNOWRHO,PSNOWDZ,PSNOWTEMP,ZSNOW,PSNOWLIQ)

! Snow thermal conductivity:
!
 CALL SNOW3LTHRM(PSNOWRHO,PSCOND,PSNOWTEMP,PPS)
!
! View factor: (1 - shielding factor)
!
ZPSNA(:)          = 0.
PCHIP(:)          = MEB_SHIELD_FACTOR(PLAICV,ZPSNA)
PSIGMA_F(:)       = 1.0 - PCHIP(:)
!
! snow emissivity
!
PEMISNOW(:)       = XEMISSN
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_EBUD_RAD',1,ZHOOK_HANDLE)
!
 CONTAINS
!####################################################################
SUBROUTINE SNOW3LFALL(PTSTEP,PSR,PTA,PVMOD,PSNOW,PSNOWRHO,PSNOWDZ,        &
                      PSNOWHEAT,PSNOWHMASS,PSNOWAGE,PPERMSNOWFRAC)
!
!!    PURPOSE
!!    -------
!     Calculate changes to snowpack resulting from snowfall.
!     Update mass and heat content of uppermost layer.
!
!
USE MODD_CSTS,     ONLY : XLMTT, XTT, XCI
USE MODD_SNOW_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, &
                          XSNOWFALL_A_SN,         &
                          XSNOWFALL_B_SN,         &
                          XSNOWFALL_C_SN
!
USE MODE_SNOW3L
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PSR, PTA, PVMOD, PPERMSNOWFRAC
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOW
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ, PSNOWHEAT, PSNOWAGE
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOWHMASS
!
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWFALL, ZRHOSNEW,        &
                                       ZSNOW, ZSNOWTEMP,           &
                                       ZSNOWFALL_DELTA, ZSCAP,     &
                                       ZAGENEW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOW3LFALL',0,ZHOOK_HANDLE)
!
INI             = SIZE(PSNOWDZ(:,:),1)
INLVLS          = SIZE(PSNOWDZ(:,:),2)
!
ZRHOSNEW(:)     = XRHOSMIN_ES
ZAGENEW (:)     = 0.0
ZSNOWFALL(:)    = 0.0
ZSCAP(:)        = 0.0
ZSNOW(:)        = PSNOW(:)
!
PSNOWHMASS(:)   = 0.0
!
! 1. Incorporate snowfall into snowpack:
! --------------------------------------
!
!
! Heat content of newly fallen snow (J/m2):
! NOTE for now we assume the snowfall has
! the temperature of the snow surface upon reaching the snow.
! This is done as opposed to using the air temperature since
! this flux is quite small and has little to no impact
! on the time scales of interest. If we use the above assumption
! then, then the snowfall advective heat flux is zero.
!
ZSNOWTEMP(:)  = XTT
ZSCAP    (:)  = SNOW3LSCAP(PSNOWRHO(:,1))
!
WHERE (PSR(:) > 0.0 .AND. PSNOWDZ(:,1)>0.)
  ZSNOWTEMP(:)  = XTT + (PSNOWHEAT(:,1) +                              &
                    XLMTT*PSNOWRHO(:,1)*PSNOWDZ(:,1))/                   &
                    (ZSCAP(:)*MAX(XSNOWDMIN/INLVLS,PSNOWDZ(:,1)))
  ZSNOWTEMP(:)  = MIN(XTT, ZSNOWTEMP(:))
END WHERE
!
WHERE (PSR(:) > 0.0)
!
  PSNOWHMASS(:) = PSR(:)*(XCI*(ZSNOWTEMP(:)-XTT)-XLMTT)*PTSTEP
!
! Snowfall density: Following CROCUS (Pahaut 1976)
!
   ZRHOSNEW(:)   = MAX(XRHOSMIN_ES, XSNOWFALL_A_SN + XSNOWFALL_B_SN*(PTA(:)-XTT)+         &
                     XSNOWFALL_C_SN*SQRT(PVMOD(:)))
!
!
! Fresh snowfall changes the snowpack age,
! decreasing in uppermost snow layer (mass weighted average):
!
   PSNOWAGE(:,1) = (PSNOWAGE(:,1)*PSNOWDZ(:,1)*PSNOWRHO(:,1)+ZAGENEW(:)*PSR(:)*PTSTEP) / &
                   (PSNOWDZ(:,1)*PSNOWRHO(:,1)+PSR(:)*PTSTEP)
!
! Augment total pack depth:
!
   ZSNOWFALL(:)  = PSR(:)*PTSTEP/ZRHOSNEW(:)    ! snowfall thickness (m)
!
   PSNOW(:)      = PSNOW(:) + ZSNOWFALL(:)
!
! Fresh snowfall changes the snowpack
! density, increases the total liquid water
! equivalent: in uppermost snow layer:
!
   PSNOWRHO(:,1) = (PSNOWDZ(:,1)*PSNOWRHO(:,1) + ZSNOWFALL(:)*ZRHOSNEW(:))/     &
                   (PSNOWDZ(:,1)+ZSNOWFALL(:))
!
   PSNOWDZ(:,1)  = PSNOWDZ(:,1) + ZSNOWFALL(:)
!
! Add energy of snowfall to snowpack:
! Update heat content (J/m2) (therefore the snow temperature
! and liquid content):
!
   PSNOWHEAT(:,1)  = PSNOWHEAT(:,1) + PSNOWHMASS(:)
!
END WHERE
!
!
! 2. Case of new snowfall on a previously snow-free surface:
! ----------------------------------------------------------
!
! When snow first falls on a surface devoid of snow,
! redistribute the snow mass throughout the 3 layers:
! (temperature already set in the calling routine
! for this case)
!
ZSNOWFALL_DELTA(:)    = 0.0
WHERE(ZSNOW(:) == 0.0 .AND. PSR(:) > 0.0)
   ZSNOWFALL_DELTA(:) = 1.0
END WHERE
!
DO JJ=1,INLVLS
   DO JI=1,INI
!
      PSNOWDZ(JI,JJ)   = ZSNOWFALL_DELTA(JI)*(ZSNOWFALL(JI) /INLVLS) + &
                        (1.0-ZSNOWFALL_DELTA(JI))*PSNOWDZ(JI,JJ)
!
      PSNOWHEAT(JI,JJ) = ZSNOWFALL_DELTA(JI)*(PSNOWHMASS(JI)/INLVLS) + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWHEAT(JI,JJ)
!
      PSNOWRHO(JI,JJ)  = ZSNOWFALL_DELTA(JI)*ZRHOSNEW(JI)            + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWRHO(JI,JJ)
!
      PSNOWAGE(JI,JJ)  = ZSNOWFALL_DELTA(JI)*(ZAGENEW(JI)/INLVLS)    + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWAGE(JI,JJ)
!
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SNOW3LFALL',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE SNOW3LFALL
!####################################################################
        SUBROUTINE SNOW3LTRANSF(PSNOW,PSNOWDZ,PSNOWDZN,    &
                                PSNOWRHO,PSNOWHEAT,PSNOWAGE)
!
!!    PURPOSE
!!    -------
!     Snow mass,heat and characteristics redistibution in case of
!     grid resizing. Total mass and heat content of the overall snowpack
!     unchanged/conserved within this routine.
!     Same method as in Crocus
!
USE MODD_SNOW_PAR, ONLY : XSNOWCRITD
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:  ), INTENT(IN)    :: PSNOW
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZN
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWHEAT
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWAGE
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JI, JL, JLO
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHON
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEATN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGEN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZTOP_NEW
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZBOT_NEW
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHOO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEATO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGEO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWDZO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZTOP_OLD
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZBOT_OLD
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEAN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZMASTOTN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZMASSDZO
!
REAL, DIMENSION(SIZE(PSNOW)) :: ZPSNOW_OLD, ZPSNOW_NEW
REAL, DIMENSION(SIZE(PSNOW)) :: ZSUMHEAT, ZSUMSWE, ZSUMAGE, ZSNOWMIX_DELTA
!
REAL :: ZPROPOR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!
IF (LHOOK) CALL DR_HOOK('SNOW3LTRANSF',0,ZHOOK_HANDLE)
!
INI        = SIZE(PSNOWRHO,1)
INLVLS     = SIZE(PSNOWRHO,2)
!
ZPSNOW_NEW(:) = 0.0
ZPSNOW_OLD(:) = PSNOW(:)
!
DO JL=1,INLVLS
   DO JI=1,INI
      ZPSNOW_NEW(JI)=ZPSNOW_NEW(JI)+PSNOWDZN(JI,JL)
   ENDDO
ENDDO
!
! initialization of variables describing the initial snowpack
!
ZSNOWDZO  (:,:) = PSNOWDZ  (:,:)
ZSNOWRHOO (:,:) = PSNOWRHO (:,:)
ZSNOWHEATO(:,:) = PSNOWHEAT(:,:)
ZSNOWAGEO (:,:) = PSNOWAGE (:,:)
ZMASSDZO  (:,:) = XUNDEF
!
! 1. Calculate vertical grid limits (m):
! --------------------------------------
!
ZSNOWZTOP_OLD(:,1) = ZPSNOW_OLD(:)
ZSNOWZTOP_NEW(:,1) = ZPSNOW_NEW(:)
ZSNOWZBOT_OLD(:,1) = ZSNOWZTOP_OLD(:,1)-ZSNOWDZO(:,1)
ZSNOWZBOT_NEW(:,1) = ZSNOWZTOP_NEW(:,1)-PSNOWDZN(:,1)
!
DO JL=2,INLVLS
   DO JI=1,INI
      ZSNOWZTOP_OLD(JI,JL) = ZSNOWZBOT_OLD(JI,JL-1)
      ZSNOWZTOP_NEW(JI,JL) = ZSNOWZBOT_NEW(JI,JL-1)
      ZSNOWZBOT_OLD(JI,JL) = ZSNOWZTOP_OLD(JI,JL  )-ZSNOWDZO(JI,JL)
      ZSNOWZBOT_NEW(JI,JL) = ZSNOWZTOP_NEW(JI,JL  )-PSNOWDZN(JI,JL)
   ENDDO
ENDDO
ZSNOWZBOT_OLD(:,INLVLS)=0.0
ZSNOWZBOT_NEW(:,INLVLS)=0.0

WHERE(PSNOWDZN(:,:)==0.)
   ZSNOWZTOP_OLD(:,:) = 0.
   ZSNOWZTOP_NEW(:,:) = 0.
   ZSNOWZBOT_OLD(:,:) = 0.
   ZSNOWZBOT_NEW(:,:) = 0.
END WHERE
!
! 3. Calculate mass, heat, charcateristics mixing due to vertical grid resizing:
! --------------------------------------------------------------------
!
! loop over the new snow layers
! Summ or avergage of the constituting quantities of the old snow layers
! which are totally or partially inserted in the new snow layer
! For snow age, mass weighted average is used.
!
ZSNOWHEAN(:,:)=0.0
ZMASTOTN (:,:)=0.0
ZSNOWAGN (:,:)=0.0
!
DO JL=1,INLVLS
   DO JLO=1, INLVLS
      DO JI=1,INI
        IF((ZSNOWZTOP_OLD(JI,JLO)>ZSNOWZBOT_NEW(JI,JL)).AND.(ZSNOWZBOT_OLD(JI,JLO)<ZSNOWZTOP_NEW(JI,JL)))THEN
!
          ZPROPOR = (MIN(ZSNOWZTOP_OLD(JI,JLO), ZSNOWZTOP_NEW(JI,JL)) &
                  -  MAX(ZSNOWZBOT_OLD(JI,JLO), ZSNOWZBOT_NEW(JI,JL)))&
                  / ZSNOWDZO(JI,JLO)
!
          ZMASSDZO (JI,JLO)=ZSNOWRHOO(JI,JLO)*ZSNOWDZO(JI,JLO)*ZPROPOR
!
          ZMASTOTN (JI,JL)=ZMASTOTN (JI,JL)+ZMASSDZO  (JI,JLO)
          ZSNOWAGN (JI,JL)=ZSNOWAGN (JI,JL)+ZSNOWAGEO (JI,JLO)*ZMASSDZO(JI,JLO)
!
          ZSNOWHEAN(JI,JL)=ZSNOWHEAN(JI,JL)+ZSNOWHEATO(JI,JLO)*ZPROPOR
!
        ENDIF
      ENDDO
    ENDDO
ENDDO
!
! the new layer inherits from the weighted average properties of the old ones
! heat and mass
!
ZSNOWHEATN(:,:)= ZSNOWHEAN(:,:)
WHERE(PSNOWDZN(:,:)==0.)
   ZSNOWAGEN (:,:)= PSNOWAGE(:,:)
   ZSNOWRHON (:,:)= PSNOWRHO(:,:)
   ZSNOWHEATN(:,:)= PSNOWHEAT(:,:)
ELSEWHERE
   ZSNOWAGEN (:,:)= ZSNOWAGN (:,:)/ZMASTOTN(:,:)
   ZSNOWRHON (:,:)= ZMASTOTN (:,:)/PSNOWDZN(:,:)
END WHERE
!
! 4. Vanishing or very thin snowpack check:
! -----------------------------------------
!
! NOTE: ONLY for very shallow snowpacks, mix properties (homogeneous):
! this avoids problems related to heat and mass exchange for
! thin layers during heavy snowfall or signifigant melt: one
! new/old layer can exceed the thickness of several old/new layers.
! Therefore, mix (conservative):
!
ZSUMHEAT(:)       = 0.0
ZSUMSWE(:)        = 0.0
ZSUMAGE(:)        = 0.0
ZSNOWMIX_DELTA(:) = 0.0
!
DO JL=1,INLVLS
   DO JI=1,INI
      IF(PSNOW(JI) < XSNOWCRITD)THEN
         ZSUMHEAT      (JI) = ZSUMHEAT(JI) + PSNOWHEAT(JI,JL)
         ZSUMSWE       (JI) = ZSUMSWE (JI) + PSNOWRHO (JI,JL)*PSNOWDZ(JI,JL)
         ZSUMAGE       (JI) = ZSUMAGE (JI) + PSNOWAGE (JI,JL)
         ZSNOWMIX_DELTA(JI) = 1.0
      ENDIF
   ENDDO
ENDDO
!
! Heat and mass are evenly distributed vertically:
! heat and mass (density and thickness) are constant
! in profile:
!
DO JL=1,INLVLS
   DO JI=1,INI

      IF(PSNOWDZN(JI,JL)>0.)THEN

!
         ZSNOWHEATN(JI,JL) = ZSNOWMIX_DELTA(JI)*(ZSUMHEAT(JI)/INLVLS)  + &
                             (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWHEATN(JI,JL)
!
         PSNOWDZN(JI,JL)   = ZSNOWMIX_DELTA(JI)*(PSNOW(JI)/INLVLS)     + &
                            (1.0-ZSNOWMIX_DELTA(JI))*PSNOWDZN(JI,JL)
!
         ZSNOWRHON(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMSWE(JI)/PSNOW(JI)) + &
                            (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWRHON(JI,JL)
!
         ZSNOWAGEN(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMAGE(JI)/INLVLS)  + &
                             (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWAGEN(JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!
! 5. Update mass (density and thickness) and heat:
! ------------------------------------------------
!
PSNOWDZ  (:,:) = PSNOWDZN  (:,:)
PSNOWRHO (:,:) = ZSNOWRHON (:,:)
PSNOWHEAT(:,:) = ZSNOWHEATN(:,:)
PSNOWAGE (:,:) = ZSNOWAGEN (:,:)
!
IF (LHOOK) CALL DR_HOOK('SNOW3LTRANSF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LTRANSF
!####################################################################
      SUBROUTINE SNOW3LGRID(PSNOWDZ,PSNOW,PSNOWDZ_OLD)
!
!!    PURPOSE
!!    -------
!     Once during each time step, update grid to maintain
!     grid proportions. Similar to approach of Lynch-Steiglitz,
!     1994, J. Clim., 7, 1842-1855. Corresponding mass and
!     heat adjustments made directly after the call to this
!     routine. 3 grid configurations:
!     1) for very thin snow, constant grid spacing
!     2) for intermediate thicknesses, highest resolution at soil/snow
!        interface and at the snow/atmosphere interface
!     3) for deep snow, vertical resoution finest at snow/atmosphere
!        interface (set to a constant value) and increases with snow depth.
!        Second layer can't be more than an order of magnitude thicker
!        than surface layer.
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XSNOWCRITD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:  ), INTENT(IN )           :: PSNOW
REAL, DIMENSION(:,:), INTENT(OUT)           :: PSNOWDZ
REAL, DIMENSION(:,:), INTENT(IN ), OPTIONAL :: PSNOWDZ_OLD
!
!*      0.1    declarations of local variables
!
INTEGER                           :: JJ, JI
!
INTEGER                           :: INLVLS, INI
!
REAL,     DIMENSION(SIZE(PSNOW))  :: ZWORK
!
LOGICAL , DIMENSION(SIZE(PSNOW))  :: GREGRID

! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF1  = (/0.25, 0.50, 0.25/)
REAL, PARAMETER, DIMENSION(2)     :: ZSGCOEF2  = (/0.05, 0.34/)
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF   = (/0.3, 0.4, 0.3/)
!
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: ZSNOWTRANS = 0.20                ! (m)
!
! Minimum snow depth by layer for 6-L or 12-L configuration :
!
REAL, PARAMETER                   ::  ZDZ1=0.01
REAL, PARAMETER                   ::  ZDZ2=0.05
REAL, PARAMETER                   ::  ZDZ3=0.15
REAL, PARAMETER                   ::  ZDZ4=0.50
REAL, PARAMETER                   ::  ZDZ5=1.00
REAL, PARAMETER                   ::  ZDZN0=0.02
REAL, PARAMETER                   ::  ZDZN1=0.1
REAL, PARAMETER                   ::  ZDZN2=0.5
REAL, PARAMETER                   ::  ZDZN3=1.0
!
REAL, PARAMETER                   ::  ZCOEF1 = 0.5
REAL, PARAMETER                   ::  ZCOEF2 = 1.5
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',0,ZHOOK_HANDLE)
!
INLVLS = SIZE(PSNOWDZ(:,:),2)
INI    = SIZE(PSNOWDZ(:,:),1)
!
ZWORK  (:) = 0.0
GREGRID(:) = .TRUE.
!
! 1. Calculate current grid for 3-layer (default) configuration):
! ---------------------------------------------------------------
! Based on formulation of Lynch-Stieglitz (1994)
! except for 3 modifications:
! i) smooth transition here at ZSNOWTRANS
! ii) constant ratio for very thin snow:
! iii) ratio of layer 2 to surface layer <= 10
!
IF(INLVLS == 1)THEN
!
  DO JI=1,INI
     PSNOWDZ(JI,1)  = PSNOW(JI)
  ENDDO
!
ELSEIF(INLVLS == 3)THEN
!
   WHERE(PSNOW <= XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,3) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,2) = PSNOW(:) - PSNOWDZ(:,1) - PSNOWDZ(:,3)
   END WHERE
!
   WHERE(PSNOW <= ZSNOWTRANS .AND. PSNOW > XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = PSNOW(:)*ZSGCOEF1(1)
      PSNOWDZ(:,2) = PSNOW(:)*ZSGCOEF1(2)
      PSNOWDZ(:,3) = PSNOW(:)*ZSGCOEF1(3)
   END WHERE
!
   WHERE(PSNOW > ZSNOWTRANS)
      PSNOWDZ(:,1) = ZSGCOEF2(1)
      PSNOWDZ(:,2) = (PSNOW(:)-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
!
! When using simple finite differences, limit the thickness
! factor between the top and 2nd layers to at most 10
!
      PSNOWDZ(:,2) = MIN(10*ZSGCOEF2(1),  PSNOWDZ(:,2))
      PSNOWDZ(:,3) = PSNOW(:) - PSNOWDZ(:,2) - PSNOWDZ(:,1)
   END WHERE
!
!
! 2. Calculate current grid for 6-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 6)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,6) < ZCOEF1 * MIN(ZDZN1,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,6) > ZCOEF2 * MIN(ZDZN1,PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,6) = MIN(ZDZN1,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:)     = PSNOW(:) - PSNOWDZ(:,1) - PSNOWDZ(:,2) - PSNOWDZ(:,6)
       PSNOWDZ(:,3) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,4) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,5) = ZWORK(:)*ZSGCOEF(3)
!      layer 3 tickness >= layer 2 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,3)-PSNOWDZ(:,2))
       PSNOWDZ(:,3)=PSNOWDZ(:,3)-ZWORK(:)
       PSNOWDZ(:,4)=PSNOWDZ(:,4)+ZWORK(:)
!      layer 5 tickness >= layer 6 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,5)-PSNOWDZ(:,6))
       PSNOWDZ(:,5)=PSNOWDZ(:,5)-ZWORK(:)
       PSNOWDZ(:,4)=PSNOWDZ(:,4)+ZWORK(:)
  ENDWHERE
!
! 3. Calculate current grid for 9-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 9)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,9) < ZCOEF1 * MIN(ZDZN0,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,9) > ZCOEF2 * MIN(ZDZN0,PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3) = MIN(ZDZ3,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,9)= MIN(ZDZN0,PSNOW(:)/INLVLS)
       PSNOWDZ(:,8)= MIN(ZDZN1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,7)= MIN(ZDZN2,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:) = PSNOW(:) - PSNOWDZ(:, 1) - PSNOWDZ(:, 2) - PSNOWDZ(:, 3) &
                           - PSNOWDZ(:, 7) - PSNOWDZ(:, 8) - PSNOWDZ(:, 9)
       PSNOWDZ(:,4) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,5) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,6) = ZWORK(:)*ZSGCOEF(3)
!      layer 4 tickness >= layer 3 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,4)-PSNOWDZ(:,3))
       PSNOWDZ(:,4)=PSNOWDZ(:,4)-ZWORK(:)
       PSNOWDZ(:,5)=PSNOWDZ(:,5)+ZWORK(:)
!      layer 6 tickness >= layer 7 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,6)-PSNOWDZ(:,7))
       PSNOWDZ(:,6)=PSNOWDZ(:,6)-ZWORK(:)
       PSNOWDZ(:,5)=PSNOWDZ(:,5)+ZWORK(:)
  ENDWHERE
!
! 4. Calculate current grid for 12-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 12)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:, 1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,12) < ZCOEF1 * MIN(ZDZN0,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,12) > ZCOEF2 * MIN(ZDZN0,PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3) = MIN(ZDZ3,PSNOW(:)/INLVLS)
       PSNOWDZ(:,4) = MIN(ZDZ4,PSNOW(:)/INLVLS)
       PSNOWDZ(:,5) = MIN(ZDZ5,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,12)= MIN(ZDZN0,PSNOW(:)/INLVLS)
       PSNOWDZ(:,11)= MIN(ZDZN1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,10)= MIN(ZDZN2,PSNOW(:)/INLVLS)
       PSNOWDZ(:, 9)= MIN(ZDZN3,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:) = PSNOW(:) - PSNOWDZ(:, 1) - PSNOWDZ(:, 2) - PSNOWDZ(:, 3) &
                           - PSNOWDZ(:, 4) - PSNOWDZ(:, 5) - PSNOWDZ(:, 9) &
                           - PSNOWDZ(:,10) - PSNOWDZ(:,11) - PSNOWDZ(:,12)
       PSNOWDZ(:,6) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,7) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,8) = ZWORK(:)*ZSGCOEF(3)
!      layer 6 tickness >= layer 5 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,6)-PSNOWDZ(:,5))
       PSNOWDZ(:,6)=PSNOWDZ(:,6)-ZWORK(:)
       PSNOWDZ(:,7)=PSNOWDZ(:,7)+ZWORK(:)
!      layer 8 tickness >= layer 9 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,8)-PSNOWDZ(:,9))
       PSNOWDZ(:,8)=PSNOWDZ(:,8)-ZWORK(:)
       PSNOWDZ(:,7)=PSNOWDZ(:,7)+ZWORK(:)
  ENDWHERE
!
! 4. Calculate other non-optimized grid :
! ---------------------------------------
!
ELSEIF(INLVLS<10.AND.INLVLS/=3.AND.INLVLS/=6.AND.INLVLS/=9) THEN
!
  DO JJ=1,INLVLS
     DO JI=1,INI
        PSNOWDZ(JI,JJ)  = PSNOW(JI)/INLVLS
     ENDDO
  ENDDO
!
  PSNOWDZ(:,INLVLS) = PSNOWDZ(:,INLVLS) + (PSNOWDZ(:,1) - MIN(0.05, PSNOWDZ(:,1)))
  PSNOWDZ(:,1)      = MIN(0.05, PSNOWDZ(:,1))
!
ELSE !(INLVLS>=10 and /=12)
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,     1) < ZCOEF1 * MIN(ZDZ1         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     1) > ZCOEF2 * MIN(ZDZ1         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     2) < ZCOEF1 * MIN(ZDZ2         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     2) > ZCOEF2 * MIN(ZDZ2         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,INLVLS) < ZCOEF1 * MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,INLVLS) > ZCOEF2 * MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
       PSNOWDZ(:,1     ) = MIN(ZDZ1         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2     ) = MIN(ZDZ2         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3     ) = MIN(ZDZ3         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,4     ) = MIN(ZDZ4         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,5     ) = MIN(ZDZ5         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,INLVLS) = MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS)
  ENDWHERE
!
  DO JJ=6,INLVLS-1,1
     DO JI=1,INI
        IF(GREGRID(JI))THEN
          ZWORK(JI) = PSNOWDZ(JI,1)+PSNOWDZ(JI,2)+PSNOWDZ(JI,3)+PSNOWDZ(JI,4)+PSNOWDZ(JI,5)
          PSNOWDZ(JI,JJ) = (PSNOW(JI)-ZWORK(JI)-PSNOWDZ(JI,INLVLS))/(INLVLS-6)
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(PSNOW(JI)==XUNDEF)THEN
         PSNOWDZ(JI,JJ) = XUNDEF
      ELSEIF(.NOT.GREGRID(JI))THEN
         PSNOWDZ(JI,JJ)=PSNOWDZ_OLD(JI,JJ)
      ENDIF
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LGRID
!####################################################################
SUBROUTINE SNOW3LCOMPACTN(PTSTEP,PSNOWDZMIN,PSNOWRHO,PSNOWDZ,PSNOWTEMP,PSNOW,PSNOWLIQ)
!
!!    PURPOSE
!!    -------
!     Snow compaction due to overburden and settling.
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. Method
!     of Brun et al (1989) and Vionnet et al. (2012)
!
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_CSTS,     ONLY : XTT, XG
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES
!
USE MODD_SNOW_METAMO, ONLY : XVVISC1,XVVISC3,XVVISC4, &
                             XVVISC5,XVVISC6,XVRO11
!
USE MODE_SNOW3L,   ONLY : SNOW3LHOLD
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
REAL, INTENT(IN)                    :: PSNOWDZMIN
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWTEMP, PSNOWLIQ
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOW
!
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO2, ZVISCOCITY, ZF1, &
                                                      ZTEMP, ZSMASS, ZSNOWDZ,     &
                                                      ZWSNOWDZ, ZWHOLDMAX
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('SNOW3LCOMPACTN',0,ZHOOK_HANDLE)
!
INI             = SIZE(PSNOWDZ(:,:),1)
INLVLS          = SIZE(PSNOWDZ(:,:),2)
!
ZSNOWRHO2 (:,:) = PSNOWRHO(:,:)
ZSNOWDZ   (:,:) = MAX(PSNOWDZMIN,PSNOWDZ(:,:))
ZVISCOCITY(:,:) = 0.0
ZTEMP     (:,:) = 0.0
!
! 1. Cumulative snow mass (kg/m2):
! --------------------------------
!
ZSMASS(:,:) = 0.0
DO JJ=2,INLVLS
   DO JI=1,INI
      ZSMASS(JI,JJ) = ZSMASS(JI,JJ-1) + PSNOWDZ(JI,JJ-1)*PSNOWRHO(JI,JJ-1)
   ENDDO
ENDDO
! overburden of half the mass of the uppermost layer applied to itself
ZSMASS(:,1) = 0.5 * PSNOWDZ(:,1) * PSNOWRHO(:,1)
!
! 2. Compaction
! -------------
!
!Liquid water effect
!
ZWHOLDMAX(:,:) = SNOW3LHOLD(PSNOWRHO,PSNOWDZ)
ZWHOLDMAX(:,:) = MAX(1.E-10, ZWHOLDMAX(:,:))
ZF1(:,:) = 1.0/(XVVISC5+10.*MIN(1.0,PSNOWLIQ(:,:)/ZWHOLDMAX(:,:)))
!
!Snow viscocity, density and grid thicknesses
!
DO JJ=1,INLVLS
   DO JI=1,INI
!
      IF(PSNOWRHO(JI,JJ) < XRHOSMAX_ES)THEN
!
!       temperature dependence limited to 10K: Schleef et al. (2014)
        ZTEMP     (JI,JJ) = XVVISC4*MIN(10.,ABS(XTT-PSNOWTEMP(JI,JJ)))
!
!       Calculate snow viscocity: Brun et al. (1989), Vionnet et al. (2012)
        ZVISCOCITY(JI,JJ) = XVVISC1*ZF1(JI,JJ)*EXP(XVVISC3*PSNOWRHO(JI,JJ)+ZTEMP(JI,JJ))*PSNOWRHO(JI,JJ)/XVRO11
!
!       Calculate snow density:
        ZSNOWRHO2(JI,JJ) = PSNOWRHO(JI,JJ) + PSNOWRHO(JI,JJ)*PTSTEP &
                         * ( (XG*ZSMASS(JI,JJ)/ZVISCOCITY(JI,JJ)) )
!
!       Conserve mass by decreasing grid thicknesses in response to density increases
        PSNOWDZ(JI,JJ) = PSNOWDZ(JI,JJ)*(PSNOWRHO(JI,JJ)/ZSNOWRHO2(JI,JJ))
!
      ENDIF
!
   ENDDO
ENDDO
!
! 3. Update total snow depth and density profile:
! -----------------------------------------------
!
! Compaction/augmentation of total snowpack depth
!
PSNOW(:) = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      PSNOW(JI) = PSNOW(JI) + PSNOWDZ(JI,JJ)
   ENDDO
ENDDO
!
! Update density (kg m-3):
!
PSNOWRHO(:,:)  = ZSNOWRHO2(:,:)
!
IF (LHOOK) CALL DR_HOOK('SNOW3LCOMPACTN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LCOMPACTN
!####################################################################


END SUBROUTINE PREPS_FOR_MEB_EBUD_RAD
END MODULE MODI_PREPS_FOR_MEB_EBUD_RAD
