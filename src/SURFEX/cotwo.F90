!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE COTWO(PCSP, PF2, PIA, PDS, PGAMMT,               &
                     PFZERO, PEPSO, PANMAX, PGMEST, PGC, PDMAX, &
                     PAN, PGS, PRD, PLAITOP, PLAI   )  
!   #########################################################################
!
!!****  *COTWO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates net assimilation of CO2 and leaf conductance.
!              
!!**  METHOD
!!    ------
!     Calvet et al. 1998 Forr. Agri. Met. [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_CO2V_PAR
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. 1998 Forr. Agri. Met. 
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
!!      L. Jarlan   27/10/04 : Add of photosynthesis (PPST variable) as output
!!                              of the routine in order to manage the calculation
!!                              of Soil respiration in cotwores and cotworestress
!!      P Le Moigne  09/2005 AGS modifs of L. Jarlan
!!      A.L. Gibelin 07/2009 : Suppress GPP and PPST as outputs
!!                             GPP is calculated in cotwores.f90 and cotworestress.f90
!!      B. Decharme   2012   : optimization
!!      C. Delire     2014   : Assuming a nitrogen profile with an exctinction coefficient 
!!      B. Decharme   07/15  : Bug = Add numerical adjustement for very dry soil 
!!      
!!
!-------------------------------------------------------------------------------
!
USE MODD_CSTS,     ONLY : XMV, XMD, XRHOLW
USE MODD_CO2V_PAR, ONLY : XRDCF, XAIRTOH2O, XCO2TOH2O, XCONDSTMIN
USE MODD_ISBA_PAR, ONLY : XDENOM_MIN
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
!
!                                      Atmospheric forcing:
REAL, DIMENSION(:),   INTENT(IN):: PCSP, PF2, PIA, PDS,PGAMMT
!                                      PCSP  = atmospheric concentration of CO2
!                                      PF2   = normalized soil water stress
!                                      PIA   = incident solar radiation
!                                      PDS   = saturation deficit of atmosphere
!                                              verses the leaf surface (with correction)
!                                      PGAMMT = compensation point
!
!                                      Time constants:
!
REAL, DIMENSION(:), INTENT(IN)  :: PFZERO, PEPSO, PANMAX, PGMEST, PGC, PDMAX, PLAITOP, PLAI
!                                      PFZERO    = ideal value of F, no photorespiration 
!                                                  or saturation deficit
!                                      PEPSO     = maximum initial quantum use efficiency 
!                                                  (kgCO2 J-1 PAR)
!                                      PGAMM     = CO2 conpensation concentration (kgCO2 kgAir-1)
!                                      PANMAX    = maximum net assimilation
!                                      PGMEST    = temperature response 
!                                                  of mesophyll conductance
!                                      PGC       = cuticular conductance (m s-1)
!                                      PDMAX     = maximum saturation deficit of 
!                                                  atmosphere tolerate by vegetation       
!                                      PLAITOP   = LAI (thickness of canopy) above considered layer 
!                                      PLAI      = canopy LAI 
!
!                                      CO2 model outputs:
REAL, DIMENSION(:),  INTENT(OUT) :: PAN, PGS, PRD
!                                      PAN   = Net assimilation of CO2
!                                      PGS   = Leaf conductance
!                                      PRD   = Dark Respiration
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PAN)) :: ZFMIN, ZDRAP, ZF, ZWORK
!                                       ZFMIN = minimum f factor
!                                       ZDRAP = ratio Ds/Dmax
!                                       ZF    = factor related to diffusion
!                                       ZWORK = work array
!
REAL, DIMENSION(SIZE(PAN)) :: ZCSP, ZCI, ZCMIN, ZAMIN  
!                                       ZCSP    = atmospheric concentration 
!                                                       of CO2
!                                       ZCI     = Leaf Internal concentration 
!                                                       of CO2
!                                       ZCMIN = minimim internal leaf
!                                               CO2 concentration
!                                       ZAMIN   = minimum net 
!                                                       assimilation
!
REAL, DIMENSION(SIZE(PAN)) :: ZAM, ZEPS, ZLEF, ZAGR, ZAG
!                                       ZAM     = net assimilation as a 
!                                                 function of CO2 deficit
!                                       ZEPS   = initial quantum 
!                                                use efficiency
!                                       ZLEF    = leaf transpiration 
!                                        ZAGR = assimilation rate ratio
!                                        ZAG  = modified gross assimilation 
!                                               rate
!
REAL, DIMENSION(SIZE(PAN)) :: ZGSC, ZGS
!                                       ZGSC    = stomatal conductance to CO2
!                                       ZGS     = cuticular conductance (m s-1)
!
INTEGER :: JJ, ITER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COTWO',0,ZHOOK_HANDLE)
!
!*       X.     COMPUTE PRELIMINARY QUANITIES NEEDED FOR CO2 MODEL
!               --------------------------------------------------
!
DO JJ = 1, SIZE(PAN) 
  !
  !*       X.     BEGIN CO2 MODEL:
  !               ----------------
  !
  !                                                          Equation #s in 
  !                                                          Jacob's Thesis:
  !
  ZWORK(JJ) = MAX(PGC(JJ)+PGMEST(JJ),XDENOM_MIN)
  !  
  ! Eq. 3.21
  ZFMIN(JJ) = PGC(JJ)/ZWORK(JJ) ! fmin
  ! fmin <= f0, and so f <= f0
  ZFMIN(JJ) = MIN(ZFMIN(JJ),PFZERO(JJ))
  ! fmin > 0, and so PCI > PGAMMT
  ZFMIN(JJ) = MAX(ZFMIN(JJ),XDENOM_MIN)
  !
  ! f from specific humidity deficit ds (g kg-1)
  !
  IF (PDMAX(JJ).NE.0.) THEN
    ZDRAP(JJ)=MIN(1.0,PDS(JJ)/PDMAX(JJ))
  ELSE
    ZDRAP(JJ)=1.
  ENDIF
  ZF(JJ)  = PFZERO(JJ)*(1.0-ZDRAP(JJ)) + ZFMIN(JJ)*ZDRAP(JJ)
  !
  ZCSP(JJ)  = MAX(PCSP(JJ),PGAMMT(JJ)+1.e-6)
  !
  ! ci/cs ratio = f+(1.-f)*gammt/cs ; internal leaf CO2 concentration:
  !
  ZCI(JJ) = ZCSP(JJ)*(ZF(JJ)+(1.0-ZF(JJ))*PGAMMT(JJ)/ZCSP(JJ))
  !
  !
  ! Eq. 3.23
  ZCMIN(JJ) = (PGC(JJ)*ZCSP(JJ) + PGMEST(JJ)*PGAMMT(JJ))/ZWORK(JJ) 
  !
  ! residual photosynthesis rate (kgCO2 kgAir-1 m s-1)
  !
  ZAMIN(JJ) = PGMEST(JJ)*(ZCMIN(JJ)-PGAMMT(JJ))
  !
  !
  ! Eq. 3.12
  !
  ! light response curve (kgCO2 kgAir-1 m s-1)
  !
  ZAM(JJ) = PGMEST(JJ)*(ZCI(JJ)-PGAMMT(JJ))
  !
  ZAM(JJ) = -ZAM(JJ)/PANMAX(JJ)
  ZAM(JJ) = PANMAX(JJ)*(1.0 - EXP(ZAM(JJ)))
  ZAM(JJ) = MAX(ZAM(JJ),ZAMIN(JJ))
  !
  ! Assuming a nitrogen profile within the canopy with a Kn exctinction coefficient (Bonan et al, 2011) 
  ! that applies to dark respiration. Here Kn=0.2 (Mercado et al, 2009).
  ! Rd is divised by LAI to be consistent with the logic of assimilation (calculated per unit LAI)
  ! if not tropical forest, PLAITOP=0, PRD=ZAM*XRDCF
  !
  PRD(JJ) = ZAM(JJ)*XRDCF/PLAI(JJ)*EXP(-0.2*PLAITOP(JJ))
  !
  ! Initial quantum use efficiency (kgCO2 J-1 PAR m3 kgAir-1):
  !
  ZEPS(JJ)   = PEPSO(JJ)*(ZCI(JJ) - PGAMMT(JJ))/(ZCI(JJ) + 2.0*PGAMMT(JJ))
  !
  IF (ZAM(JJ)/=0.) THEN
    PAN(JJ) = (ZAM(JJ) + PRD(JJ))*( 1.0 - EXP(-ZEPS(JJ)*PIA(JJ) &
               /(ZAM(JJ) + PRD(JJ))) ) - PRD(JJ)  
  ELSE
    PAN(JJ) = 0.
  ENDIF
  PAN(JJ) = MAX(-PRD(JJ),PAN(JJ))
  !
  ! Eq. 3.28
  IF (ZAM(JJ)/=0.) THEN
    ZAGR(JJ) = (PAN(JJ) + PRD(JJ))/(ZAM(JJ) + PRD(JJ))   
  ELSE
    ZAGR(JJ)=0.
  ENDIF
  !
  ZAG(JJ)  = PAN(JJ) - ZAMIN(JJ)*ZDRAP(JJ)*ZAGR(JJ) +     &
                   PRD(JJ)*(1.0-ZAGR(JJ))  
  !
  ZLEF(JJ)    = 0.0                                  ! initialize leaf transpiration
  !
ENDDO
!
!   Iterate bewteen GSC and LEF:
!
! Iterations are for stomatal conductance and stomatal evaporation only
!
DO ITER = 1, 3
  !
  DO JJ = 1, SIZE(PAN)
    !
    ! stomatal conductance for CO2 (m s-1):
    !
    ZGSC(JJ) = ZAG(JJ)/(ZCSP(JJ) - ZCI(JJ))
    ZGSC(JJ) = MAX( XCONDSTMIN, ZGSC(JJ))
    ZGSC(JJ) = ZGSC(JJ) + XAIRTOH2O*ZLEF(JJ)*( (ZCSP(JJ) +      &
                ZCI(JJ))/(2.0*(ZCSP(JJ) - ZCI(JJ))) )
    !
    IF (ITER<3) THEN
    !
      ZGS(JJ) = 1.6*ZGSC(JJ)
    !
    ! compute transpiration (kgH2O kgAir-1 m s-1) from specific
    ! humidity deficit
    !
    ! Eq. 3.5
      ZLEF(JJ) = ZGS(JJ)*PDS(JJ)
    !
    ENDIF
    !
  ENDDO
  !
ENDDO
!   
!   End of iterations
!
! Final calculation of leaf conductance (stomatal AND cuticular)
!
! Eq. 3.16
PGS(:) = XCO2TOH2O*ZGSC(:) + PGC(:)
!
! Prevent numerical artefact in plant transpiration when
! soilstress is maximum (F2 = 0.0)
!
PGS(:) = PGS(:) * MIN(1.0,PF2(:)/XDENOM_MIN)
!
IF (LHOOK) CALL DR_HOOK('COTWO',1,ZHOOK_HANDLE)
!	
END SUBROUTINE COTWO
