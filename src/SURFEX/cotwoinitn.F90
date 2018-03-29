!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_COTWOINIT_n
CONTAINS
      SUBROUTINE COTWOINIT_n (IO, S, PK, PEK, PCO2  )
!     #######################################################################
!
!!****  *COTWOINIT*
!!
!!    PURPOSE
!!    -------
!
!     Initialize model to calculate net assimilation of
!     CO2 and leaf conductance.
!
!!**  METHOD
!!    ------
!     Calvet at al (1998) [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CO2V_PAR
!!    USE MODI_COTWO
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. (1998)
!!
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97
!!      (V. Rivalland) 10/04/02  Add: PK%XAH and PK%XBH coefficients for
!!                               herbaceous water stress response
!!      (P. LeMoigne) 03/2004:   computation of zgmest in SI units
!!      (P. LeMoigne) 10/2004:   possibility of 2 different FZERO
!!      (L. Jarlan)   10/2004:   initialization of DMAX
!!      P Le Moigne   09/2005    AGS modifs of L. Jarlan
!!      S. Lafont     03/2009    change unit of AMAX
!!      A.L. Gibelin  04/2009    TAU_WOOD for NCB option
!!      A.L. Gibelin  04/2009    Suppress useless GPP and RDK arguments
!!      A.L. Gibelin  07/2009    Suppress PPST and PPSTF as outputs
!!      B. Decharme   05/2012    Optimization
!!      R. Alkama     05/2012    add 7 new vegtype (19  instead 12)
!!      C. Delire     01/2014    Define a dummy LAI from top and total lai for Dark respiration
!!
!-------------------------------------------------------------------------------
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_S_t, ISBA_PE_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE_ECOSG, NVEGTYPE, NVT_C3, NVT_C3W, NVT_C3S, NVT_C4, NVT_IRR, &
                                NVT_TROG, NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,    &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, NVT_GRAS
USE MODD_CSTS,           ONLY : XMD
USE MODD_CO2V_PAR,       ONLY : XTOPT, XFZERO1, XFZERO2, XFZEROTROP, XEPSO, XGAMM, XQDGAMM, &
                                  XQDGMES, XT1GMES, XT2GMES, XAMAX, ITRANSFERT_ESG,         &
                                  XQDAMAX, XT1AMAX, XT2AMAX, XAH, XBH,            &
                                  XDSPOPT, XIAOPT, XAW, XBW, XMCO2, XMC, XTAU_WOOD
!
USE MODI_COTWO
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(ISBA_S_t), INTENT(INOUT) :: S
!
REAL,DIMENSION(:),INTENT(IN)  :: PCO2
!                                     PK%XGMES     = mesophyll conductance (m s-1)
!                                     PCO2      = atmospheric CO2 concentration
!
!*      0.2    declaration of local variables
!
INTEGER                           :: JCLASS    ! indexes for loops
INTEGER                           :: ICLASS    ! indexes for loops
INTEGER                           :: ICO2TYPE  ! type of CO2 vegetation
INTEGER                           :: IRAD      ! with or without new radiative transfer
!
REAL, DIMENSION(SIZE(PK%XANMAX))     :: ZGS, ZGAMMT, ZTOPT, ZANMAX, ZGMEST, ZGPP, ZRDK, ZEPSO
!                                    ZTOPT     = optimum  temperature for compensation
!                                                point
!                                    ZANMAX    = maximum photosynthesis rate
!                                    ZGS       = leaf conductance
!                                    ZGAMMT    = temperature compensation point
!                                    ZGPP      = gross primary production
!                                    ZRDK      = dark respiration
!
!
REAL, DIMENSION(SIZE(PK%XANMAX))     :: ZCO2INIT3, ZCO2INIT4, ZCO2INIT5, ZCO2INIT2,ZCO2INIT1
!                                    working arrays for initializing surface
!                                    temperature, saturation deficit, global radiation,
!                                    optimum temperature for determining maximum
!                                    photosynthesis rate, and soil water stress (none)
REAL, DIMENSION(SIZE(PK%XDMAX))      :: ZDMAX
REAL, DIMENSION(SIZE(PK%XDMAX))      :: ZWORK
!                                    Local variable in order to initialise DMAX
!                                    following Calvet, 2000 (AST or LST cases)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COTWOINIT_N',0,ZHOOK_HANDLE)
!
ZTOPT  (:) = 0.
PK%XFZERO (:) = 0.
PK%XEPSO  (:) = 0.
PK%XGAMM  (:) = 0.
PK%XQDGAMM(:) = 0.
PK%XQDGMES(:) = 0.
PK%XT1GMES(:) = 0.
PK%XT2GMES(:) = 0.
PK%XAMAX  (:) = 0.
PK%XQDAMAX(:) = 0.
PK%XT1AMAX(:) = 0.
PK%XT2AMAX(:) = 0.
PK%XTAU_WOOD(:) = 0.
!
PK%XAH    (:) = 0.
PK%XBH    (:) = 0.
!
ZEPSO (:) = 0.
ZGPP (:) = 0.
ZRDK (:) = 0.
ZGAMMT (:) = 0.
ZANMAX (:) = 0.
ZGMEST (:) = 0.
ZCO2INIT3(:) = 0.
ZCO2INIT4(:) = 0.
ZCO2INIT5(:) = 0.
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!
! INITIALIZE VARIOUS PARAMETERS FOR CO2 MODEL:
! --------------------------------------------
! as a function of CO2 vegetation class, C3=>1, C4=>2
!
DO JCLASS=1,NVEGTYPE
  !
  IF (JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR .OR. JCLASS==NVT_TROG) THEN
    ICO2TYPE = 2   ! C4 type
  ELSE
    ICO2TYPE = 1   ! C3 type
  END IF
  IF(IO%LAGRI_TO_GRASS.AND.(JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR)) ICO2TYPE = 1
  IF (IO%LTR_ML) THEN
    IRAD = 1   ! running with new radiative transfer
  ELSE
    IRAD = 2
  ENDIF
  !
  ZTOPT  (:) = ZTOPT  (:) + XTOPT  (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  IF((JCLASS==NVT_TEBD) .OR. (JCLASS==NVT_BONE) .OR.                         &
    (JCLASS==NVT_TRBD) .OR. (JCLASS==NVT_TEBE) .OR. (JCLASS==NVT_TENE) .OR. &
    (JCLASS==NVT_BOBD) .OR. (JCLASS==NVT_BOND) .OR. (JCLASS==NVT_SHRB)) THEN
    PK%XFZERO (:) = PK%XFZERO (:) + ((XAW - LOG(PEK%XGMES(:)*1000.0))/XBW)*PK%XVEGTYPE_PATCH(:,JCLASS)
  ELSE IF (JCLASS==NVT_TRBE) THEN
    PK%XFZERO (:) = PK%XFZERO (:) + XFZEROTROP(IRAD) * PK%XVEGTYPE_PATCH(:,JCLASS)
  ELSE
    PK%XFZERO (:) = PK%XFZERO (:) + XFZERO2 (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  ENDIF
  !
  PK%XEPSO  (:) = PK%XEPSO  (:) + XEPSO  (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XGAMM  (:) = PK%XGAMM  (:) + XGAMM  (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XQDGAMM(:) = PK%XQDGAMM(:) + XQDGAMM(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XQDGMES(:) = PK%XQDGMES(:) + XQDGMES(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XT1GMES(:) = PK%XT1GMES(:) + XT1GMES(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XT2GMES(:) = PK%XT2GMES(:) + XT2GMES(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XQDAMAX(:) = PK%XQDAMAX(:) + XQDAMAX(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XT1AMAX(:) = PK%XT1AMAX(:) + XT1AMAX(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XT2AMAX(:) = PK%XT2AMAX(:) + XT2AMAX(ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XAH    (:) = PK%XAH    (:) + XAH    (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  PK%XBH    (:) = PK%XBH    (:) + XBH    (ICO2TYPE) * PK%XVEGTYPE_PATCH(:,JCLASS)
  !
  IF(IO%LAGRI_TO_GRASS.AND.(JCLASS==NVT_C3 .OR. JCLASS==NVT_C3W .OR. JCLASS==NVT_C3S .OR. &
                            JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR))THEN
    ICLASS=NVT_GRAS
  ELSE
    ICLASS=JCLASS
  ENDIF
  !
  IF (NVEGTYPE==NVEGTYPE_ECOSG) THEN
    PK%XTAU_WOOD(:) = PK%XTAU_WOOD(:) + XTAU_WOOD(ITRANSFERT_ESG(ICLASS)) * PK%XVEGTYPE_PATCH(:,JCLASS)
    PK%XAMAX    (:) = PK%XAMAX    (:) + XAMAX    (ITRANSFERT_ESG(ICLASS)) * PK%XVEGTYPE_PATCH(:,JCLASS)
  ELSE
    PK%XTAU_WOOD(:) = PK%XTAU_WOOD(:) + XTAU_WOOD(ICLASS) * PK%XVEGTYPE_PATCH(:,JCLASS)
    PK%XAMAX    (:) = PK%XAMAX    (:) + XAMAX    (ICLASS) * PK%XVEGTYPE_PATCH(:,JCLASS)
  ENDIF
  !
END DO
!
PK%XQDGAMM(:)=LOG(PK%XQDGAMM(:))
PK%XQDGMES(:)=LOG(PK%XQDGMES(:))
PK%XQDAMAX(:)=LOG(PK%XQDAMAX(:))
!
!
! INITIALIZE VARIOUS VARIABLES FOR CO2 MODEL:
! -------------------------------------------
!
!
! compute temperature responses:
!
!before optimization (with non log PK%XQDGAMM) :
!ZGAMMT(:) = PK%XGAMM(:)*(PK%XQDGAMM(:)**(0.1*(ZTOPT(:)-25.0)))
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PK%XQDGAMM(:)
ZGAMMT(:) = PK%XGAMM(:)*EXP(ZWORK(:))
!
!before optimization (with non log PK%XQDAMAX) :
!ZANMAX(:) = ( PK%XAMAX(:)*PK%XQDAMAX(:)**(0.1*(ZTOPT(:)-25.0)) ) / ...
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PK%XQDAMAX(:)
ZANMAX(:) = ( PK%XAMAX(:)*EXP(ZWORK(:)) )                   &
               /( (1.0+EXP(0.3*(PK%XT1AMAX(:)-ZTOPT(:))))*  &
                  (1.0+EXP(0.3*(ZTOPT(:)-PK%XT2AMAX(:)))) )
!
!before optimization (with non log PK%XQDGMES) :
!ZGMEST(:) = ( PEK%XGMES(:)*PK%XQDGMES(:)**(0.1*(ZTOPT(:)-25.0)) )    &
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PK%XQDGMES(:)
ZGMEST(:) = ( PEK%XGMES(:)*EXP(ZWORK(:)) )                   &
               /( (1.0+EXP(0.3*(PK%XT1GMES(:)-ZTOPT(:))))*  &
                  (1.0+EXP(0.3*(ZTOPT(:)-PK%XT2GMES(:)))) )
!
!
! initialize other variables: (using optimum values for some variables)
!
ZCO2INIT3(:) = XDSPOPT
ZCO2INIT4(:) = XIAOPT
ZCO2INIT5(:) = 1.0
!
! Define a dummy LAI from top (zco2init2=0.1) and total lai (zco2init=1) for Dark respiration extinction parameterization
!
ZCO2INIT2(:) = 0.1
ZCO2INIT1(:) = 1.0
!
! Add soil moisture stress effect to leaf conductance:
!
ZGMEST(:) = ZGMEST(:)*ZCO2INIT5(:)
!
! Initialise DMAX following Calvet (2000) in the case of 'AST' or 'LST' photosynthesis option
!
IF(IO%CPHOTO/='NON') THEN
   ZDMAX(:) = EXP((LOG(ZGMEST(:)*1000.)-PK%XAH(:))/PK%XBH(:))/1000.
ELSE
   ZDMAX(:) = PK%XDMAX(:)
ENDIF
!
! Compute maximum/initial/optimum net assimilation of CO2:
!
! Unit conversion with a constant value of 1.2 for PRHOA as it is not known here
! ZANMAX and ZEPSO from kgCO2/m2/s to kgCO2/kgair m/s by dividing by RHOA (kgair/m3)
! ZGAMMT from ppm to kgCO2/kgair
ZANMAX(:)=ZANMAX(:)/1.2
ZEPSO(:)=PK%XEPSO(:)/1.2
ZGAMMT(:)=ZGAMMT(:)*XMCO2/XMD*1e-6
!
CALL COTWO(PCO2, ZCO2INIT5, ZCO2INIT4, ZCO2INIT3, ZGAMMT, &
           PK%XFZERO(:), ZEPSO, ZANMAX, ZGMEST, PEK%XGC, ZDMAX,     &
           PK%XANMAX(:), ZGS, ZRDK, ZCO2INIT2, ZCO2INIT1        )
! change by sebastien PK%XEPSO change into ZEPSO for units consistency
!
!
!
IF (LHOOK) CALL DR_HOOK('COTWOINIT_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE COTWOINIT_n
END MODULE MODI_COTWOINIT_n
