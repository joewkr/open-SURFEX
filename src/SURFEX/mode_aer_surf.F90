!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!   ########################
MODULE MODE_AER_SURF
!!   ########################
!!
!! MODULE DUST PSD (Particle Size Distribution)
!! Purpose: Contains subroutines to convert from transported variables (ppp)
!! to understandable aerosol variables, e.g. #/m3, kg/m3, sigma, R_{n}
!-------------------------------------------------------------------------------
!!    MODIFICATIONS
!!    -------------
!!
!!      J.Escobar     06/2013  for REAL4/8 add EPSILON management
!!    M.Leriche 2015 : masse molaire Black carbon à 12 g/mol
!!
!------------------------------------------------------------------------------- 
  USE MODD_CHS_AEROSOL
  USE MODD_DST_SURF, ONLY : XDENSITY_DST
!
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  USE MODD_SURF_PAR , ONLY : XSURF_TINY
!
  IMPLICIT NONE
!!
CONTAINS
!!
SUBROUTINE INIT_VAR(PSV,PFAC,PCTOTA)
!
IMPLICIT NONE
!
REAL,DIMENSION(:,:), INTENT(IN)    :: PSV      ! [aerosol concentration]
REAL,DIMENSION(:), INTENT(OUT)     :: PFAC     ! M3 / mass conversion factor
REAL,DIMENSION(:,:,:), INTENT(OUT) :: PCTOTA
!
REAL,DIMENSION(NSP+NCARB+NSOA)                     :: ZMI      ! [kg/mol] molar weight of aerosol
REAL,DIMENSION(NSP+NCARB+NSOA)                     :: ZRHOI    ! aerosol density
REAL                                               :: ZPI
INTEGER                                            :: JJ
REAL, PARAMETER                                    :: ZMOL = 6.0221367E+11
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:INIT_VAR',0,ZHOOK_HANDLE)
!
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DST
!
! Moments index
NM0(1) = 1 
NM3(1) = 2
NM6(1) = 3 
NM0(2) = 4 
NM3(2) = 5 
NM6(2) = 6 
!
!Set density of aerosol, here  (kg/m3)
! Aerosol Density
! Cf Ackermann (all to black carbon except water)
!
!Set molecular weightn g/mol 
ZMI(:) = 250.
ZMI(JP_AER_SO4)  = 98.
ZMI(JP_AER_NO3)  = 63.
ZMI(JP_AER_NH3)  = 17.
ZMI(JP_AER_H2O)  = 18.
ZMI(JP_AER_BC)   = 12.
ZMI(JP_AER_DST)  = 100.
IF (NSOA .EQ. 10) THEN
  ZMI(JP_AER_SOA1) = 88. 
  ZMI(JP_AER_SOA2) = 180.
  ZMI(JP_AER_SOA3) = 1.5374857E3
  ZMI(JP_AER_SOA4) = 1.9586780E3
  ZMI(JP_AER_SOA5) = 195.
  ZMI(JP_AER_SOA6) = 195.
  ZMI(JP_AER_SOA7) = 165.
  ZMI(JP_AER_SOA8) = 195.
  ZMI(JP_AER_SOA9) = 270.
  ZMI(JP_AER_SOA10) = 210.
END IF
!
ZPI = 2.*ASIN(1.)
DO JJ=1,NSP+NCARB+NSOA
  PFAC(JJ)=(4./3.)*ZPI*ZRHOI(JJ)*1.e-9
ENDDO
!
!*       2    transfer aerosol mass from gas to aerosol variables
!               (and conversion of mol.cm-3 --> microgram/m3)
!
PCTOTA(:,:,:) = 0.
! aerosol phase
PCTOTA(:,JP_AER_SO4,1) = PSV(:,JP_CH_SO4i)*ZMI(JP_AER_SO4)/ZMOL
PCTOTA(:,JP_AER_SO4,2) = PSV(:,JP_CH_SO4j)*ZMI(JP_AER_SO4)/ZMOL

PCTOTA(:,JP_AER_NO3,1) = PSV(:,JP_CH_NO3i)*ZMI(JP_AER_NO3)/ZMOL
PCTOTA(:,JP_AER_NO3,2) = PSV(:,JP_CH_NO3j)*ZMI(JP_AER_NO3)/ZMOL

PCTOTA(:,JP_AER_NH3,1) = PSV(:,JP_CH_NH3i)*ZMI(JP_AER_NH3)/ZMOL
PCTOTA(:,JP_AER_NH3,2) = PSV(:,JP_CH_NH3j)*ZMI(JP_AER_NH3)/ZMOL
!
! water
PCTOTA(:,JP_AER_H2O,1) = PSV(:,JP_CH_H2Oi)*ZMI(JP_AER_H2O)/ZMOL
PCTOTA(:,JP_AER_H2O,2) = PSV(:,JP_CH_H2Oj)*ZMI(JP_AER_H2O)/ZMOL
!
! primary organic carbon
PCTOTA(:,JP_AER_OC,1) = PSV(:,JP_CH_OCi)*ZMI(JP_AER_OC)/ZMOL
PCTOTA(:,JP_AER_OC,2) = PSV(:,JP_CH_OCj)*ZMI(JP_AER_OC)/ZMOL
!
! primary black carbon
PCTOTA(:,JP_AER_BC,1) = PSV(:,JP_CH_BCi)*ZMI(JP_AER_BC)/ZMOL
PCTOTA(:,JP_AER_BC,2) = PSV(:,JP_CH_BCj)*ZMI(JP_AER_BC)/ZMOL
!
!dust
PCTOTA(:,JP_AER_DST,1) = PSV(:,JP_CH_DSTi)*ZMI(JP_AER_DST)/6.0221367E+11
PCTOTA(:,JP_AER_DST,2) = PSV(:,JP_CH_DSTj)*ZMI(JP_AER_DST)/6.0221367E+11
!
IF (NSOA .EQ. 10) THEN
  PCTOTA(:,JP_AER_SOA1,1) = PSV(:,JP_CH_SOA1i)*ZMI(JP_AER_SOA1)/ZMOL
  PCTOTA(:,JP_AER_SOA1,2) = PSV(:,JP_CH_SOA1j)*ZMI(JP_AER_SOA1)/ZMOL
  PCTOTA(:,JP_AER_SOA2,1) = PSV(:,JP_CH_SOA2i)*ZMI(JP_AER_SOA2)/ZMOL
  PCTOTA(:,JP_AER_SOA2,2) = PSV(:,JP_CH_SOA2j)*ZMI(JP_AER_SOA2)/ZMOL
  PCTOTA(:,JP_AER_SOA3,1) = PSV(:,JP_CH_SOA3i)*ZMI(JP_AER_SOA3)/ZMOL
  PCTOTA(:,JP_AER_SOA3,2) = PSV(:,JP_CH_SOA3j)*ZMI(JP_AER_SOA3)/ZMOL
  PCTOTA(:,JP_AER_SOA4,1) = PSV(:,JP_CH_SOA4i)*ZMI(JP_AER_SOA4)/ZMOL
  PCTOTA(:,JP_AER_SOA4,2) = PSV(:,JP_CH_SOA4j)*ZMI(JP_AER_SOA4)/ZMOL
  PCTOTA(:,JP_AER_SOA5,1) = PSV(:,JP_CH_SOA5i)*ZMI(JP_AER_SOA5)/ZMOL
  PCTOTA(:,JP_AER_SOA5,2) = PSV(:,JP_CH_SOA5j)*ZMI(JP_AER_SOA5)/ZMOL

  PCTOTA(:,JP_AER_SOA6,1) = PSV(:,JP_CH_SOA6i)*ZMI(JP_AER_SOA6)/ZMOL
  PCTOTA(:,JP_AER_SOA6,2) = PSV(:,JP_CH_SOA6j)*ZMI(JP_AER_SOA6)/ZMOL
  PCTOTA(:,JP_AER_SOA7,1) = PSV(:,JP_CH_SOA7i)*ZMI(JP_AER_SOA7)/ZMOL
  PCTOTA(:,JP_AER_SOA7,2) = PSV(:,JP_CH_SOA7j)*ZMI(JP_AER_SOA7)/ZMOL
  PCTOTA(:,JP_AER_SOA8,1) = PSV(:,JP_CH_SOA8i)*ZMI(JP_AER_SOA8)/ZMOL
  PCTOTA(:,JP_AER_SOA8,2) = PSV(:,JP_CH_SOA8j)*ZMI(JP_AER_SOA8)/ZMOL
  PCTOTA(:,JP_AER_SOA9,1) = PSV(:,JP_CH_SOA9i)*ZMI(JP_AER_SOA9)/ZMOL
  PCTOTA(:,JP_AER_SOA9,2) = PSV(:,JP_CH_SOA9j)*ZMI(JP_AER_SOA9)/ZMOL
  PCTOTA(:,JP_AER_SOA10,1) = PSV(:,JP_CH_SOA10i)*ZMI(JP_AER_SOA10)/ZMOL
  PCTOTA(:,JP_AER_SOA10,2) = PSV(:,JP_CH_SOA10j)*ZMI(JP_AER_SOA10)/ZMOL
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:INIT_VAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VAR
!
!!   ############################################################
SUBROUTINE PPP2AERO_SURF(             &
          PSVT,                       &!I [ppp] input scalar variables (moment of distribution)
          PRHODREF,                   &!I [kg/m3] density of air       
          PSIG1D,                     &!O [-] standard deviation of aerosol distribution
          PRG1D,                      &!O [um] number median diameter of aerosol distribution
          PN1D,                       &!O [#/m3] number concentration of aerosols
          PCTOTA,                     &!O [ug/m3] mass of each aerosol compounds
          PM1D                        &!moments 0, 3 and 6
         )  
!!   ############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Translate the three moments M0, M3 and M6 given in ppp into
!!    Values which can be understood more easily (R, sigma, N, M)
!! 
!!    CALLING STRUCTURE NOTE: OPTIONAL VARIABLES
!!    -------
!!    CALL PPP2AERO_SURFS(PSVT, PRHODREF, PSIG1D=SIGVAR,  &
!!       PRG1D=RVAR, PN1D=NVAR, PM1D=ZM)
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Alf Grini (CNRM)
!!
!!    EXTERNAL
!!    --------
!!
    IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
!INPUT
REAL,       DIMENSION(:,:),  INTENT(IN)     :: PSVT      !I [#/molec_{air}] first moment
                                                         !I [molec_{aer}/molec_{air} 3rd moment
                                                         !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,       DIMENSION(:),    INTENT(IN)      :: PRHODREF !I [kg/m3] density of air

!OUTPUT
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PSIG1D   !O [-] standard deviation
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PRG1D    !O [um] number median diameter
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PN1D     !O [#/m3] number concentration
REAL,       DIMENSION(:,:,:),OPTIONAL, INTENT(OUT)     :: PCTOTA   !O [ug/m3] mass of each component
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PM1D     !O moments 0,3 and 6 
!
!*      0.2    declarations local variables
!    
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2))         :: ZSV      ! [aerosol concentration]
REAL,DIMENSION(SIZE(PSVT,1))                       :: ZSIGMA   ! [-] standard deviation
REAL,DIMENSION(SIZE(PSVT,1),NSP+NCARB+NSOA,JPMODE) :: ZCTOTA
REAL,DIMENSION(SIZE(PSVT,1),JPMODE*3)              :: ZM
REAL,DIMENSION(JPMODE*3)                           :: ZMMIN
!
REAL,DIMENSION(NSP+NCARB+NSOA)                     :: ZFAC     ! M3 / mass conversion factor
REAL, PARAMETER                                    :: ZDEN2MOL = 1E-6 * 6.0221367E+23  / 28.9644E-3
INTEGER                                            :: JJ, JN   ! [idx] loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:PPP2AERO_SURF',0,ZHOOK_HANDLE)
!
!        1.    initialisation
!
DO JJ=1, SIZE(PSVT,2)
  ZSV(:,JJ) =  PSVT(:,JJ) * ZDEN2MOL * PRHODREF(:)
  ZSV(:,JJ) = MAX(ZSV(:,JJ),1E-40 * ZDEN2MOL * PRHODREF(:))
ENDDO
!
 CALL INIT_VAR(ZSV,ZFAC,ZCTOTA)
!
!*       2    calculate moment 3 from total aerosol mass
!
ZM(:,2) = 0.
ZM(:,5) = 0.
DO JJ = 1,NSP+NCARB+NSOA
  ZM(:,2) = ZM(:,2)+ZCTOTA(:,JJ,1)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
  ZM(:,5) = ZM(:,5)+ZCTOTA(:,JJ,2)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
ENDDO
!
!
!*       3    set  moment 0 
!
ZM(:,1)=   MAX(ZSV(:,JP_CH_M0i) * 1E+6, XSURF_TINY) ! molec_{aer}/m3_{air}
ZM(:,4)=   MAX(ZSV(:,JP_CH_M0j) * 1E+6, XSURF_TINY) ! molec_{aer}/m3_{air}
WHERE ((ZM(:,1) .LT. ZMMIN(1))) !.OR.(ZM(:,2) .LT. ZMMIN(2)))
  ZM(:,1)= ZMMIN(1)
!  ZM(:,2)= ZMMIN(2)
!
!  ZCTOTA(:,JP_AER_H2O,1) = 0.
!  ZCTOTA(:,JP_AER_NH3,1) = 0.
!  ZCTOTA(:,JP_AER_SO4,1) = 0.
!  ZCTOTA(:,JP_AER_NO3,1) = 0.
!  ZCTOTA(:,JP_AER_BC,1) = 0.5 * ZM(:,2) * ZFAC(JP_AER_BC)
!  ZCTOTA(:,JP_AER_OC,1) = 0.5 * ZM(:,2) * ZFAC(JP_AER_OC)
END WHERE
!!
WHERE ((ZM(:,4) .LT. ZMMIN(4))) !.OR.(ZM(:,5) .LT. ZMMIN(5)))  
  ZM(:,4)= ZMMIN(4)
!  ZM(:,5)= ZMMIN(5)
!
!  ZCTOTA(:,JP_AER_H2O,2) = 0.
!  ZCTOTA(:,JP_AER_NH3,2) = 0.
!  ZCTOTA(:,JP_AER_SO4,2) = 0.
!  ZCTOTA(:,JP_AER_NO3,2) = 0.
!  ZCTOTA(:,JP_AER_BC,2) = 0.5 * ZM(:,5) * ZFAC(JP_AER_BC)
!  ZCTOTA(:,JP_AER_OC,2) = 0.5 * ZM(:,5) * ZFAC(JP_AER_OC)
END WHERE
!
!*       4    set moment 6  ==> um6_{aer}/m3_{air}
!
IF (LVARSIGI) THEN ! set M6 variable standard deviation
  ZM(:,3) = MAX(ZSV(:,JP_CH_M6i), XSURF_TINY)

  ZSIGMA(:)=ZM(:,2)**2/(ZM(:,1)*ZM(:,3))
  ZSIGMA(:)=MIN(1-1E-10,ZSIGMA(:))
  ZSIGMA(:)=MAX(1E-10,ZSIGMA(:))
  ZSIGMA(:)= LOG(ZSIGMA(:))
  ZSIGMA(:)= EXP(1./3.*SQRT(-ZSIGMA(:)))
  ZM(:,3) = ZM(:,1) &
            * ( (ZM(:,2)/ZM(:,1))**(1./3.)  &
            * exp(-(3./2.)*log(ZSIGMA(:))**2))**6 &
            * exp(18.*log(ZSIGMA(:))**2)  

  IF(PRESENT(PSIG1D)) PSIG1D(:,1) = ZSIGMA(:)

ELSE ! fixed standard deviation
  ZM(:,3) = ZM(:,1) &
            * ( (ZM(:,2)/ZM(:,1))**(1./3.)  &
            * exp(-(3./2.)*log(XEMISSIGI)**2))**6 &
            * exp(18.*log(XEMISSIGI)**2) 

  IF(PRESENT(PSIG1D)) PSIG1D(:,1) = XEMISSIGI
END IF

IF (LVARSIGJ) THEN ! set M6 variable standard deviation
  ZM(:,6) = MAX(ZSV(:,JP_CH_M6j), XSURF_TINY)

  ZSIGMA(:)=ZM(:,5)**2/(ZM(:,4)*ZM(:,6))
  ZSIGMA(:)=MIN(1-1E-10,ZSIGMA(:))
  ZSIGMA(:)=MAX(1E-10,ZSIGMA(:))
  ZSIGMA(:)= LOG(ZSIGMA(:))
  ZSIGMA(:)= EXP(1./3.*SQRT(-ZSIGMA(:)))

  ZM(:,6) = ZM(:,4) &
            * ( (ZM(:,5)/ZM(:,4))**(1./3.)  &
            * exp(-(3./2.)*log(ZSIGMA(:))**2))**6 &
            * exp(18.*log(ZSIGMA(:))**2)  

  IF(PRESENT(PSIG1D)) PSIG1D(:,2) = ZSIGMA(:)

ELSE ! fixed standard deviation
  ZM(:,6) = ZM(:,4) &
            * ( (ZM(:,5)/ZM(:,4))**(1./3.)  &
            * exp(-(3./2.)*log(XEMISSIGJ)**2))**6 &
            * exp(18.*log(XEMISSIGJ)**2)  

  IF(PRESENT(PSIG1D)) PSIG1D(:,2) = XEMISSIGJ
END IF


!*       6    calculate modal parameters from moments
DO JN=1,JPMODE
  IF(PRESENT(PN1D)) PN1D(:,JN) = ZM(:,NM0(JN))

  IF(PRESENT(PRG1D)) PRG1D(:,JN)=(ZM(:,NM3(JN))**4. &
                                 / (ZM(:,NM6(JN))*ZM(:,NM0(JN))**3.))**(1./6.)  
ENDDO

IF(PRESENT(PCTOTA)) PCTOTA(:,:,:) = ZCTOTA(:,:,:)
IF(PRESENT(PM1D)) PM1D(:,:) = ZM(:,:)
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:PPP2AERO_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE PPP2AERO_SURF
!!   ############################################################
SUBROUTINE AERO2PPP_SURF(    &
            PSVT,            &!IO [ppp] input scalar variables (moment of distribution)
            PRHODREF,        &!I [kg/m3] density of air       
            PSIG1D,          &!I [-] standard deviation of aerosol distribution
            PRG1D            &!I [um] number median diameter of aerosol distribution
            )  
!!   ############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Translate the aerosol Mass, RG and SIGMA in the  three moments M0, M3 and M6 given in ppp 
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Alf Grini (CNRM)
!!
!!    EXTERNAL
!!    --------
!!
IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
!INPUT
REAL,       DIMENSION(:,:),  INTENT(INOUT)  :: PSVT      !I [#/molec_{air}] first moment
                                                         !I [molec_{aer}/molec_{air} 3rd moment
                                                         !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,       DIMENSION(:),  INTENT(IN)       :: PRHODREF  !I [kg/m3] density of air

!OUTPUT
REAL,       DIMENSION(:,:),  INTENT(IN)     :: PSIG1D   !O [-] standard deviation
REAL,       DIMENSION(:,:),  INTENT(IN)     :: PRG1D    !O [um] number median diameter
!
!*      0.2    declarations local variables
!
REAL,DIMENSION(SIZE(PSVT,1),NSP+NCARB+NSOA,JPMODE):: ZCTOTA
REAL,DIMENSION(SIZE(PSVT,1), JPMODE*3) :: ZM                  ! [aerosol units] local array which goes to output later
!
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZFAC                ! M3 / mass conversion factor
REAL, PARAMETER                      :: ZDEN2MOL = 1E-6 * 6.0221367E+23  / 28.9644E-3
INTEGER                              :: JJ                  ! [idx] loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:AERO2PPP_SURF',0,ZHOOK_HANDLE)
!
!        1.    initialisation 
!
DO JJ=1, SIZE(PSVT, 2)
  PSVT(:,JJ) =  PSVT(:,JJ) * ZDEN2MOL * PRHODREF(:)
ENDDO
!
 CALL INIT_VAR(PSVT,ZFAC,ZCTOTA)
!
!*       3    calculate moment 3 from total aerosol mass
!
ZM(:,2) = 0.
ZM(:,5) = 0.
DO JJ = 1,NSP+NCARB+NSOA
  ZM(:,2) = ZM(:,2)+ZCTOTA(:,JJ,1)/ZFAC(JJ)
  ZM(:,5) = ZM(:,5)+ZCTOTA(:,JJ,2)/ZFAC(JJ)
ENDDO
!
!
!*       4    calculate moment 0 from dispersion and mean radius
!
ZM(:,1)= ZM(:,2)/ ( (PRG1D(:,1)**3)*EXP(4.5 * LOG(PSIG1D(:,1))**2) )  
ZM(:,4)= ZM(:,5)/ ( (PRG1D(:,2)**3)*EXP(4.5 * LOG(PSIG1D(:,2))**2) )  
!
!*       5    calculate moment 6 from dispersion and mean radius
!
ZM(:,3) = ZM(:,1)*(PRG1D(:,1)**6) * EXP(18 *(LOG(PSIG1D(:,1)))**2)  
ZM(:,6) = ZM(:,4)*(PRG1D(:,2)**6) * EXP(18 *(LOG(PSIG1D(:,2)))**2)  
!
!*       6    return to ppp
!
PSVT(:,JP_CH_M0i) = ZM(:,1) * 1E-6 
PSVT(:,JP_CH_M0j) = ZM(:,4) * 1E-6
!
IF (LVARSIGI) PSVT(:,JP_CH_M6i) = ZM(:,3) 
IF (LVARSIGJ) PSVT(:,JP_CH_M6j) = ZM(:,6)
!
DO JJ=1,SIZE(PSVT,2)
  PSVT(:,JJ) =  PSVT(:,JJ) / (ZDEN2MOL * PRHODREF(:))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_AER_SURF:AERO2PPP_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE AERO2PPP_SURF

END MODULE MODE_AER_SURF
