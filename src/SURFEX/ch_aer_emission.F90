!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!   ############################################################
     SUBROUTINE CH_AER_EMISSION(PFLUX, PRHODREF, HSV, KSV_CHSBEG,  PFCO)
!!   ############################################################
!!
!!    PURPOSE
!!    -------
!!    Transforme les emissions  d'aérosol en masse kg.kg-1.m.s-1 en molecules.m-2.s-1 : flux du moment m3
!!    Calcule les flux des moments m0 et m6 à partir de sigma  et Rg (um)
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (CNRM/GMEI)
!!
!!    MODIFICATIONS
!!    -------------
!!    none
!!
!!    EXTERNAL
!!    --------
!!    None
!!
USE MODD_CHS_AEROSOL
USE MODD_DST_SURF, ONLY : XDENSITY_DST
USE MODI_ABOR1_SFX
!!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),      INTENT(INOUT) :: PFLUX
REAL,   DIMENSION(:),        INTENT(IN) :: PRHODREF
INTEGER,                     INTENT(IN) :: KSV_CHSBEG
 CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN)  :: HSV      ! name of chemical species
REAL,   DIMENSION(:),OPTIONAL, INTENT(IN)  :: PFCO   ! CO flux 

!
!
!*      0.2    declarations local variables
!
REAL :: ZDEN2MOL
        !  ZDEN2MOL = 6.0221367E+23 * 1E-6 / 28.9644E-3
        !  conversion factor density to mol/cm3
        !  n_molec (moelc./cm3):  M = 1E-6*RHO(kg/m3) * XAVOGADRO / XMD
REAL,DIMENSION(NSP+NCARB+NSOA) :: ZFAC, ZRHOI, ZMI
REAL,DIMENSION(SIZE(PFLUX,1),NSP+NCARB+NSOA,JPMODE) :: ZFCTOTA
REAL,DIMENSION(SIZE(PFLUX,1),JPIN) :: ZFM
REAL,DIMENSION(SIZE(PFLUX,1)) :: ZFCO
REAL,DIMENSION(SIZE(PFLUX,1)) :: ZCONVERSION
!
INTEGER :: JJ, JSV  ! loop counter
REAL   :: ZEMISRADIUSI, ZEMISRADIUSJ
REAL   :: ZVALBC, ZVALOC
INTEGER :: I_CH_M0i, I_CH_M0j, I_CH_M6i, I_CH_M6j, I_CH_H2Oi, I_CH_H2Oj,&
                  I_CH_SO4i,I_CH_SO4j, I_CH_NO3i, I_CH_NO3j, I_CH_NH3i, I_CH_NH3j,&
                  I_CH_OCi, I_CH_OCj, I_CH_BCi, I_CH_BCj  , I_CH_DSTi, I_CH_DSTj   
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------
!
!*       1.     TRANSFER FROM GAS TO AEROSOL MODULE
!               ------------------------------------
!        1.1    initialisation 
!
IF (LHOOK) CALL DR_HOOK('CH_AER_EMISSION',0,ZHOOK_HANDLE)

I_CH_M0i=-999
I_CH_M0j=-999
I_CH_M6i=-999
I_CH_M6j=-999
I_CH_H2Oi=-999
I_CH_H2Oj=-999
I_CH_SO4i=-999
I_CH_SO4j=-999
I_CH_NO3i=-999
I_CH_NO3j=-999
I_CH_NH3i=-999
I_CH_NH3j=-999
I_CH_OCi=-999
I_CH_OCj=-999
I_CH_BCi=-999
I_CH_BCj=-999
I_CH_DSTi=-999
I_CH_DSTj=-999

DO JSV=1, size(HSV)
  IF (TRIM(HSV(JSV)) == "M0I") I_CH_M0i=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "M0J") I_CH_M0j=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "M6I") I_CH_M6i=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "M6J") I_CH_M6j=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "H2OI") I_CH_H2Oi=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "H2OJ") I_CH_H2Oj=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "SO4I") I_CH_SO4i=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "SO4J") I_CH_SO4j=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "NO3I") I_CH_NO3i=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "NO3J") I_CH_NO3j=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "NH3I") I_CH_NH3i=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "NH3J") I_CH_NH3j=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "OCI") I_CH_OCi=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "OCJ") I_CH_OCj=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "BCI") I_CH_BCi=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "BCJ") I_CH_BCj=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "DSTI") I_CH_DSTi=JSV-KSV_CHSBEG+1
  IF (TRIM(HSV(JSV)) == "DSTJ") I_CH_DSTj=JSV-KSV_CHSBEG+1
END DO

IF (I_CH_M0i ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_M0i ')
IF (I_CH_M0j ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_M0j ')
IF (I_CH_M6i ==-999 .AND. LVARSIGI) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_M6i ')
IF (I_CH_M6j ==-999 .AND. LVARSIGJ) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_M6j ')
IF (I_CH_H2Oi==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_H2Oi')
IF (I_CH_H2Oj==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_H2Oj')
IF (I_CH_SO4i==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_SO4i')
IF (I_CH_SO4j==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_SO4j')
IF (I_CH_NO3i==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_NO3i')
IF (I_CH_NO3j==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_NO3j')
IF (I_CH_NH3i==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_NH3i')
IF (I_CH_NH3j==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_NH3j')
IF (I_CH_OCi ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_OCi ')
IF (I_CH_OCj ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_OCj ')
IF (I_CH_BCi ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_BCi ')
IF (I_CH_BCj ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_BCj ')
IF (I_CH_DSTi ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_DSTi ')
IF (I_CH_DSTj ==-999) CALL ABOR1_SFX ('WRONG VALUE FOR I_CH_DSTj ')

ZMI(:) = 250.
ZMI(JP_AER_SO4) = 98.
ZMI(JP_AER_NO3) = 63.
ZMI(JP_AER_NH3) = 17.
ZMI(JP_AER_H2O) = 18.
ZMI(JP_AER_DST) = 100.

! Aerosol Density
! Cf Ackermann (all to black carbon except water)
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DST

ZDEN2MOL = 1E-6 * XAVOGADRO / XMD

IF (CRGUNIT=="MASS") THEN
ZEMISRADIUSI = XEMISRADIUSI * EXP(-3.*(LOG(XEMISSIGI))**2)
ZEMISRADIUSJ = XEMISRADIUSJ * EXP(-3.*(LOG(XEMISSIGJ))**2)
ELSE
ZEMISRADIUSI = XEMISRADIUSI
ZEMISRADIUSJ = XEMISRADIUSJ
END IF
!
DO JJ=1,NSP+NCARB+NSOA
  ZFAC(JJ) = (4./3.)*3.14292654*ZRHOI(JJ)*1.e-9
ENDDO

ZFM(:,:)=0.
ZFCTOTA(:,:,:)=0.
PFLUX(:,:) = MAX(PFLUX(:,:),0.)
!
!*       0      conversion into  kg.kg-1.m.s-1 (due to PCONVERSION)

ZVALBC  = 0.
ZVALOC  = 0.
ZFCO(:) = 0.
IF ((LCO2PM).AND.(PRESENT(PFCO))) THEN
  !ZVALBC=2.748549E-02  ! CO / BC conversion factor
  !ZVALOC=4.947248E-02  ! CO / POM conversion factor
  ZVALBC= 5.* 0.6E-10 / 0.4E-8  ! CO / BC conversion factor
  ZVALOC= 5.* 0.3E-10 / 0.4E-8  ! CO / POM conversion factor
  ZFCO(:) = PFCO(:)
END IF

! Initial aerosols fluxes have been transformed into molecu.m-2.s-1, 
! conversion into are in kg.kg-1.m.s-1 
!  conversion in kg.kg-1.m.s-1
!
ZCONVERSION(:) =  XAVOGADRO * PRHODREF(:)
!
PFLUX(:,I_CH_SO4i) = PFLUX(:,I_CH_SO4i) / ZCONVERSION(:) * ZMI(JP_AER_SO4)*1E-3
PFLUX(:,I_CH_SO4j) = PFLUX(:,I_CH_SO4j) / ZCONVERSION(:) * ZMI(JP_AER_SO4)*1E-3
PFLUX(:,I_CH_NO3i) = PFLUX(:,I_CH_NO3i) / ZCONVERSION(:) * ZMI(JP_AER_NO3)*1E-3
PFLUX(:,I_CH_NO3j) = PFLUX(:,I_CH_NO3j) / ZCONVERSION(:) * ZMI(JP_AER_NO3)*1E-3
PFLUX(:,I_CH_NH3i) = PFLUX(:,I_CH_NH3i) / ZCONVERSION(:) * ZMI(JP_AER_NH3)*1E-3
PFLUX(:,I_CH_NH3j) = PFLUX(:,I_CH_NH3j) / ZCONVERSION(:) * ZMI(JP_AER_NH3)*1E-3
PFLUX(:,I_CH_H2Oi) = PFLUX(:,I_CH_H2Oi) / ZCONVERSION(:) * ZMI(JP_AER_H2O)*1E-3
PFLUX(:,I_CH_H2Oj) = PFLUX(:,I_CH_H2Oj) / ZCONVERSION(:) * ZMI(JP_AER_H2O)*1E-3
PFLUX(:,I_CH_OCi)  = (PFLUX(:,I_CH_OCi) + ZFCO(:) * ZVALOC / 2.) / ZCONVERSION(:) * ZMI(JP_AER_OC)*1E-3
PFLUX(:,I_CH_OCj)  = (PFLUX(:,I_CH_OCj) + ZFCO(:) * ZVALOC ) / ZCONVERSION(:) * ZMI(JP_AER_OC)*1E-3
PFLUX(:,I_CH_BCi)  = (PFLUX(:,I_CH_BCi) + ZFCO(:) * ZVALBC / 2.) / ZCONVERSION(:) * ZMI(JP_AER_BC)*1E-3
PFLUX(:,I_CH_BCj)  = (PFLUX(:,I_CH_BCj) + ZFCO(:) * ZVALBC ) / ZCONVERSION(:) * ZMI(JP_AER_BC)*1E-3
PFLUX(:,I_CH_DSTi) = PFLUX(:,I_CH_DSTi) / ZCONVERSION(:) * ZMI(JP_AER_DST)*1E-3 
PFLUX(:,I_CH_DSTj) = PFLUX(:,I_CH_DSTj) / ZCONVERSION(:) * ZMI(JP_AER_DST)*1E-3 
!
!*       1.0    transfer aerosol mass from gas to aerosol variables
!               (and conversion of kg.kg-1.m.s-1 --> microgram.m-2.s-1)
!
ZFCTOTA(:,JP_AER_SO4,1) = PFLUX(:,I_CH_SO4i) *1E+9 * PRHODREF(:) 
ZFCTOTA(:,JP_AER_SO4,2) = PFLUX(:,I_CH_SO4j) *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_NH3,1) = PFLUX(:,I_CH_NH3i) *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_NH3,2) = PFLUX(:,I_CH_NH3j) *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_NO3,1) = PFLUX(:,I_CH_NO3i) *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_NO3,2) = PFLUX(:,I_CH_NO3j) *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_H2O,1) = PFLUX(:,I_CH_H2Oi) *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_H2O,2) = PFLUX(:,I_CH_H2Oj) *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_OC,1)  = PFLUX(:,I_CH_OCi)  *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_OC,2)  = PFLUX(:,I_CH_OCj)  *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_BC,1)  = PFLUX(:,I_CH_BCi)  *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_BC,2)  = PFLUX(:,I_CH_BCj)  *1E+9 * PRHODREF(:)

ZFCTOTA(:,JP_AER_DST,1)  = PFLUX(:,I_CH_DSTi)  *1E+9 * PRHODREF(:)
ZFCTOTA(:,JP_AER_DST,2)  = PFLUX(:,I_CH_DSTj)  *1E+9 * PRHODREF(:)
!
!*       1.1    calculate moment 3 flux from total aerosol mass
!
ZFM(:,2) = 0.
ZFM(:,5) = 0.
DO JJ = 1,NSP+NCARB+NSOA
  ZFM(:,2) = ZFM(:,2)+ZFCTOTA(:,JJ,1)/ZFAC(JJ)
  ZFM(:,5) = ZFM(:,5)+ZFCTOTA(:,JJ,2)/ZFAC(JJ)
ENDDO
!
!*       1.2    calculate moment 0 flux from dispersion and mean radius Rg
!
ZFM(:,1)= ZFM(:,2) / ((ZEMISRADIUSI**3)*EXP(4.5 * (LOG(XEMISSIGI))**2)) 
!
ZFM(:,4)= ZFM(:,5) / ((ZEMISRADIUSJ**3)*EXP(4.5 * (LOG(XEMISSIGJ))**2)) 
!
!*       1.3    calculate moment 6 flux from dispersion and mean diameter
!
ZFM(:,3) = ZFM(:,1) * (ZEMISRADIUSI**6) *EXP(18 *(LOG(XEMISSIGI))**2)
!
ZFM(:,6) = ZFM(:,4) * (ZEMISRADIUSJ**6) *EXP(18 *(LOG(XEMISSIGJ))**2)
!
!*       1.4    conversion en ppp.m.s-1
!
! conversion in atmospheric unit only for moments 0 and 6 
PFLUX(:,I_CH_M0i) = ZFM(:,1) * 1E-6 / (ZDEN2MOL * PRHODREF(:))
PFLUX(:,I_CH_M0j) = ZFM(:,4) * 1E-6 / (ZDEN2MOL * PRHODREF(:))
!
IF (LVARSIGI) PFLUX(:,I_CH_M6i) = ZFM(:,3) / (ZDEN2MOL * PRHODREF(:))
IF (LVARSIGJ) PFLUX(:,I_CH_M6j) = ZFM(:,6) / (ZDEN2MOL * PRHODREF(:))
!
! aerosol phase conversion kg/kg.m.s-1 into ppp.m.s-1
PFLUX(:,I_CH_SO4i) = PFLUX(:,I_CH_SO4i) * XMD / (ZMI(JP_AER_SO4)*1E-3)
PFLUX(:,I_CH_SO4j) = PFLUX(:,I_CH_SO4j) * XMD / (ZMI(JP_AER_SO4)*1E-3)
PFLUX(:,I_CH_NO3i) = PFLUX(:,I_CH_NO3i) * XMD / (ZMI(JP_AER_NO3)*1E-3)
PFLUX(:,I_CH_NO3j) = PFLUX(:,I_CH_NO3j) * XMD / (ZMI(JP_AER_NO3)*1E-3)
PFLUX(:,I_CH_NH3i) = PFLUX(:,I_CH_NH3i) * XMD / (ZMI(JP_AER_NH3)*1E-3)
PFLUX(:,I_CH_NH3j) = PFLUX(:,I_CH_NH3j) * XMD / (ZMI(JP_AER_NH3)*1E-3)
PFLUX(:,I_CH_H2Oi) = PFLUX(:,I_CH_H2Oi) * XMD / (ZMI(JP_AER_H2O)*1E-3)
PFLUX(:,I_CH_H2Oj) = PFLUX(:,I_CH_H2Oj) * XMD / (ZMI(JP_AER_H2O)*1E-3)
!
PFLUX(:,I_CH_OCi) = PFLUX(:,I_CH_OCi) * XMD / (ZMI(JP_AER_OC)*1E-3)
PFLUX(:,I_CH_OCj) = PFLUX(:,I_CH_OCj) * XMD / (ZMI(JP_AER_OC)*1E-3)
PFLUX(:,I_CH_BCi) = PFLUX(:,I_CH_BCi) * XMD / (ZMI(JP_AER_BC)*1E-3)
PFLUX(:,I_CH_BCj) = PFLUX(:,I_CH_BCj) * XMD / (ZMI(JP_AER_BC)*1E-3)
PFLUX(:,I_CH_DSTi) = PFLUX(:,I_CH_DSTi) * XMD / (ZMI(JP_AER_DST)*1E-3)
PFLUX(:,I_CH_DSTj) = PFLUX(:,I_CH_DSTj) * XMD / (ZMI(JP_AER_DST)*1E-3)
!
! then conversion in  molecules.m-2.s-1
PFLUX(:,I_CH_M0i) = PFLUX(:,I_CH_M0i) * ZCONVERSION(:)
PFLUX(:,I_CH_M0j) = PFLUX(:,I_CH_M0j) * ZCONVERSION(:)
!
IF (LVARSIGI) PFLUX(:,I_CH_M6i) = PFLUX(:,I_CH_M6i) * ZCONVERSION(:) 
IF (LVARSIGJ) PFLUX(:,I_CH_M6j) = PFLUX(:,I_CH_M6j) * ZCONVERSION(:)
!
PFLUX(:,I_CH_SO4i) = PFLUX(:,I_CH_SO4i) * ZCONVERSION(:) / (ZMI(JP_AER_SO4)*1E-3)
PFLUX(:,I_CH_SO4j) = PFLUX(:,I_CH_SO4j) * ZCONVERSION(:) / (ZMI(JP_AER_SO4)*1E-3)
PFLUX(:,I_CH_NO3i) = PFLUX(:,I_CH_NO3i) * ZCONVERSION(:) / (ZMI(JP_AER_NO3)*1E-3)
PFLUX(:,I_CH_NO3j) = PFLUX(:,I_CH_NO3j) * ZCONVERSION(:) / (ZMI(JP_AER_NO3)*1E-3)
PFLUX(:,I_CH_NH3i) = PFLUX(:,I_CH_NH3i) * ZCONVERSION(:) / (ZMI(JP_AER_NH3)*1E-3)
PFLUX(:,I_CH_NH3j) = PFLUX(:,I_CH_NH3j) * ZCONVERSION(:) / (ZMI(JP_AER_NH3)*1E-3)
PFLUX(:,I_CH_H2Oi) = PFLUX(:,I_CH_H2Oi) * ZCONVERSION(:) / (ZMI(JP_AER_H2O)*1E-3)
PFLUX(:,I_CH_H2Oj) = PFLUX(:,I_CH_H2Oj) * ZCONVERSION(:) / (ZMI(JP_AER_H2O)*1E-3)
PFLUX(:,I_CH_OCi) = PFLUX(:,I_CH_OCi)   * ZCONVERSION(:) / (ZMI(JP_AER_OC)*1E-3)
PFLUX(:,I_CH_OCj) = PFLUX(:,I_CH_OCj)   * ZCONVERSION(:) / (ZMI(JP_AER_OC)*1E-3)
PFLUX(:,I_CH_BCi) = PFLUX(:,I_CH_BCi)   * ZCONVERSION(:) / (ZMI(JP_AER_BC)*1E-3)
PFLUX(:,I_CH_BCj) = PFLUX(:,I_CH_BCj)   * ZCONVERSION(:) / (ZMI(JP_AER_BC)*1E-3)
PFLUX(:,I_CH_DSTi) = PFLUX(:,I_CH_DSTi) * ZCONVERSION(:) / (ZMI(JP_AER_DST)*1E-3)
PFLUX(:,I_CH_DSTj) = PFLUX(:,I_CH_DSTj) * ZCONVERSION(:) / (ZMI(JP_AER_DST)*1E-3)
!
IF (LHOOK) CALL DR_HOOK('CH_AER_EMISSION',1,ZHOOK_HANDLE)
!
END SUBROUTINE CH_AER_EMISSION
