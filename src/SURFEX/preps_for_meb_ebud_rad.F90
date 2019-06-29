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
USE MODD_SNOW_PAR,            ONLY : XRHOSMAX_ES, XRHOSMIN_ES, XEMISSN, XSNOWDMIN, &
                                     XSNOWTHRMCOND1
!
USE MODD_CSTS,                ONLY : XTT, XLMTT, XRHOLW, XCI
!
USE MODD_SURF_PAR,            ONLY : XUNDEF
!
USE MODD_SNOW_METAMO,         ONLY : XSNOWDZMIN
!
USE MODE_SNOW3L,              ONLY : SNOW3LTHRM, SNOW3LSCAP, SNOW3LFALL,         &
                                     SNOW3LTRANSF, SNOW3LGRID, SNOW3LCOMPACTN
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
INTEGER                                            :: JI, JK, JJ, INLVLS, ISIZE_SNOW, INI
INTEGER, DIMENSION(SIZE(PTA))                      :: NMASK      ! indices correspondance between arrays
REAL, DIMENSION(SIZE(PLAICV,1))                    :: ZPSNA
REAL, DIMENSION(SIZE(PTA))                         :: ZSNOW, ZSNOWFALL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------
! 0) Initialization
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_EBUD_RAD',0,ZHOOK_HANDLE)
!
INI             = SIZE(PSNOWRHO,1)
INLVLS          = SIZE(PSNOWRHO,2)
!
! Initialize some output variables:
! where snow depth below threshold, set to non-snow values
!
PSNOWTEMP(:,:)      = XTT
PSNOWLIQ (:,:)      = 0.0
PSCOND   (:,:)      = XSNOWTHRMCOND1
PHEATCAPS(:,:)      = XRHOSMIN_ES*XCI
!
! Test variables to check for existance of snow:
!
WHERE(PSNOWRHO(:,:)==XUNDEF)
   PSNOWDZ(:,:)     = 0.
ELSEWHERE
   PSNOWDZ(:,:)     = PSNOWSWE(:,:)/PSNOWRHO(:,:)
ENDWHERE
!
ZSNOWFALL(:)     = PSR(:)*PTSTEP/XRHOSMAX_ES
!
ZSNOW(:)         = 0.0
DO JK=1,INLVLS
   DO JI=1,INI
      ZSNOW(JI)  = ZSNOW(JI) + PSNOWDZ(JI,JK)
   ENDDO
ENDDO
!
! Here, as in snow3l (ISBA-ES), we account for several processes
! on the snowpack before surface energy budget computations
! (i.e. snowfall on albedo, density, thickness, and compaction etc...)
!
! ===============================================================
! === Packing: Only call snow model routines when there is snow on the surface
!              exceeding a minimum threshold OR if the equivalent
!              snow depth falling during the current time step exceeds
!              this limit.
!
! counts the number of points where the computations will be made
!
!
ISIZE_SNOW = 0
NMASK(:)   = 0
!
DO JJ=1,INI
   IF (ZSNOW(JJ) >= XSNOWDMIN .OR. ZSNOWFALL(JJ) >= XSNOWDMIN) THEN
      ISIZE_SNOW = ISIZE_SNOW + 1
      NMASK(ISIZE_SNOW) = JJ
   ENDIF
ENDDO
!
IF (ISIZE_SNOW>0) THEN
   CALL CALL_SNOW_ROUTINES(ISIZE_SNOW,INLVLS,NMASK)
ENDIF
!
! ===============================================================
!
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
!
CONTAINS
!================================================================
SUBROUTINE CALL_SNOW_ROUTINES(KSIZE1,KSIZE2,KMASK)
!
! Make some snow computations only over regions with snow cover or snow falling
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE1
INTEGER, INTENT(IN) :: KSIZE2
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWSWE
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWRHO
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWHEAT
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWTEMP
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWLIQ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWDZ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SCOND
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWAGE
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWDZN
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_HEATCAPS
REAL, DIMENSION(KSIZE1)        :: ZP_SNOW
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWHMASS
REAL, DIMENSION(KSIZE1)        :: ZP_PERMSNOWFRAC
REAL, DIMENSION(KSIZE1)        :: ZP_PS
REAL, DIMENSION(KSIZE1)        :: ZP_SR
REAL, DIMENSION(KSIZE1)        :: ZP_TA
REAL, DIMENSION(KSIZE1)        :: ZP_VMOD

INTEGER         :: JWRK, JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_EBUD_RAD:CALL_SNOW_ROUTINES',0,ZHOOK_HANDLE)
!
! pack the variables
!
DO JWRK=1,KSIZE2
   DO JJ=1,KSIZE1
      JI = KMASK(JJ)
      ZP_SNOWSWE (JJ,JWRK) = PSNOWSWE (JI,JWRK)
      ZP_SNOWRHO (JJ,JWRK) = PSNOWRHO (JI,JWRK)
      ZP_SNOWHEAT(JJ,JWRK) = PSNOWHEAT(JI,JWRK)
      ZP_SNOWAGE (JJ,JWRK) = PSNOWAGE (JI,JWRK)
      ZP_SNOWDZ  (JJ,JWRK) = PSNOWDZ  (JI,JWRK)
   ENDDO
ENDDO
!
DO JJ=1,KSIZE1
   JI = KMASK(JJ)
   ZP_SNOW        (JJ) = ZSNOW(JI)
   ZP_PS          (JJ) = PPS  (JI)
   ZP_SR          (JJ) = PSR  (JI)
   ZP_TA          (JJ) = PTA  (JI)
   ZP_VMOD        (JJ) = PVMOD(JI)
   ZP_PERMSNOWFRAC(JJ) = PPERMSNOWFRAC(JI)
ENDDO
!
!---------------------------------------------------------------
!
! Local working:
!
ZP_SNOWHEAT(:,:)   = ZP_SNOWHEAT(:,:)*ZP_SNOWDZ(:,:) ! J/m3 to J/m2
!
!
CALL SNOW3LFALL(PTSTEP,ZP_SR,ZP_TA,ZP_VMOD,ZP_SNOW,ZP_SNOWRHO,ZP_SNOWDZ,          &
                 ZP_SNOWHEAT,ZP_SNOWHMASS,ZP_SNOWAGE,ZP_PERMSNOWFRAC)
!
CALL SNOW3LGRID(ZP_SNOWDZN,ZP_SNOW,PSNOWDZ_OLD=ZP_SNOWDZ)
!
CALL SNOW3LTRANSF(ZP_SNOW,ZP_SNOWDZ,ZP_SNOWDZN,ZP_SNOWRHO,ZP_SNOWHEAT,ZP_SNOWAGE)
!
! Snow heat capacity:
!
ZP_HEATCAPS(:,:)   = SNOW3LSCAP(ZP_SNOWRHO)                    ! J m-3 K-1
!
! Snow temperature (K)
!
ZP_SNOWTEMP(:,:)   = XTT + ( ((ZP_SNOWHEAT(:,:)/MAX(1.E-10,ZP_SNOWDZ(:,:)))  &
                   + XLMTT*ZP_SNOWRHO(:,:))/ZP_HEATCAPS(:,:) )
!
ZP_SNOWLIQ(:,:)    = MAX(0.0,ZP_SNOWTEMP(:,:)-XTT)*ZP_HEATCAPS(:,:)*         &
                     ZP_SNOWDZ(:,:)/(XLMTT*XRHOLW)

ZP_SNOWTEMP(:,:)   = MIN(XTT,ZP_SNOWTEMP(:,:))

! SWE:

ZP_SNOWSWE(:,:)  = ZP_SNOWDZ(:,:)*ZP_SNOWRHO(:,:)

CALL SNOW3LCOMPACTN(PTSTEP,XSNOWDZMIN,ZP_SNOWRHO,ZP_SNOWDZ,ZP_SNOWTEMP,ZP_SNOW,ZP_SNOWLIQ)

! Snow thermal conductivity:
!
CALL SNOW3LTHRM(ZP_SNOWRHO,ZP_SCOND,ZP_SNOWTEMP,ZP_PS)
!
!----------------------------------------------------------------
!
! Unpack:
!
DO JWRK=1,KSIZE2
   DO JJ=1,KSIZE1
      JI = KMASK(JJ)
      PSNOWSWE (JI,JWRK) = ZP_SNOWSWE (JJ,JWRK)
      PSNOWRHO (JI,JWRK) = ZP_SNOWRHO (JJ,JWRK)
      PSNOWAGE (JI,JWRK) = ZP_SNOWAGE (JJ,JWRK)
      PSNOWDZ  (JI,JWRK) = ZP_SNOWDZ  (JJ,JWRK)
      PSNOWTEMP(JI,JWRK) = ZP_SNOWTEMP(JJ,JWRK)
      PSNOWLIQ (JI,JWRK) = ZP_SNOWLIQ (JJ,JWRK)
      PSCOND   (JI,JWRK) = ZP_SCOND   (JJ,JWRK)
      PHEATCAPS(JI,JWRK) = ZP_HEATCAPS(JJ,JWRK)
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_EBUD_RAD:CALL_SNOW_ROUTINES',1,ZHOOK_HANDLE)
!
END SUBROUTINE CALL_SNOW_ROUTINES
!================================================================
!
END SUBROUTINE PREPS_FOR_MEB_EBUD_RAD
END MODULE MODI_PREPS_FOR_MEB_EBUD_RAD
