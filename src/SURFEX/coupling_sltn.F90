!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE COUPLING_SLT_n (SLT, &
      KI,                   &!I [nbr] number of sea points 
      KSLT,                 &!I [nbr] number of sea points 
      PWIND,                &!I Wind velocity
      PSFSLT                &!O [kg/m2/sec] flux of sea salt
      ) 
  
!PURPOSE
!-------
!  Compute sea salt emission  upon Vignatti et al, 2001
!
!AUTHOR
!-------
! P. Tulet
!
!
USE MODD_SLT_n, ONLY : SLT_t
!
USE MODD_CSTS, ONLY : XAVOGADRO, XPI
USE MODD_SLT_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!INPUT
!
TYPE(SLT_t), INTENT(INOUT) :: SLT
!
INTEGER, INTENT(IN)                :: KI             !I Number of sea points
INTEGER, INTENT(IN)                :: KSLT           !I Number of sea salt emission variables
REAL, DIMENSION(KI),      INTENT(IN)  :: PWIND       !I wind velocity
REAL, DIMENSION(KI,KSLT), INTENT(OUT) :: PSFSLT      !Out: mole particles per mole air m/s *(MWdst/MWair*rhoair)(index #1)
                                                     !Out: kg/m2/s (index #2)
                                                     !Out: moles m6/moles air m/s *(MWdst/MWair*rhoair)(index #3)
!LOCAL VARIABLES
REAL,DIMENSION(KI,3)  :: ZSFSLT_MDE       ! sea salt flux from modes
INTEGER               :: JN, JI           !Counter for sea salt modes
REAL, DIMENSION(KI)   :: DZSPEED 
INTEGER, DIMENSION(KI):: WCL
REAL                  :: ZCONVERTFACM0_SLT
REAL                  :: ZCONVERTFACM3_SLT
REAL                  :: ZCONVERTFACM6_SLT
!
!REAL, PARAMETER :: MASS1FLUX(0:40) = (/  &
!          0.000E+00, 2.483E-15, 2.591E-14, 1.022E-13, 2.707E-13, 5.761E-13,  &
!          1.068E-12, 1.800E-12, 2.829E-12, 4.215E-12, 6.023E-12, 8.317E-12, &
!          1.117E-11, 1.464E-11, 1.882E-11, 2.378E-11, 2.959E-11, 3.633E-11, &
!          4.409E-11, 5.296E-11, 6.301E-11, 7.433E-11, 8.693E-11, 1.012E-10, &
!          1.168E-10, 1.342E-10, 1.532E-10, 1.741E-10, 1.970E-10, 2.219E-10, &
!          2.489E-10, 2.781E-10, 3.097E-10, 3.437E-10, 3.803E-10, 4.195E-10, &
!          4.616E-10, 5.065E-10, 5.544E-10, 6.054E-10, 6.711E-10             /) 
!
!REAL, PARAMETER :: MASS2FLUX(0:40) = (/  &
!          0.000E+00, 2.319E-13, 2.411E-12, 9.481E-12, 2.505E-11, 5.321E-11,  &
!          9.850E-11, 1.658E-10, 2.602E-10, 3.874E-10, 5.529E-10, 7.628E-10,  &
!          1.023E-09, 1.341E-09, 1.722E-09, 2.175E-09, 2.704E-09, 3.319E-09,  &
!          4.026E-09, 4.832E-09, 5.746E-09, 6.776E-09, 7.925E-09, 9.214E-09,  &
!          1.064E-08, 1.221E-08, 1.394E-08, 1.584E-08, 1.791E-08, 2.016E-08,  &
!          2.261E-08, 2.526E-08, 2.812E-08, 3.120E-08, 3.451E-08, 3.806E-08,  &
!          4.186E-08, 4.592E-08, 5.025E-08, 5.486E-08, 6.014E-08             /) 
!
!REAL, PARAMETER :: MASS3FLUX(0:40) = (/ 0.0, &
!        1.783E-12, 1.579E-11, 5.852E-11, 1.501E-10, 3.134E-10, 5.740E-10, &
!        9.597E-10, 1.500E-09, 2.227E-09, 3.175E-09, 4.378E-09, 5.872E-09, &
!        7.698E-09, 9.897E-09, 1.250E-08, 1.556E-08, 1.912E-08, 2.323E-08, &
!        2.792E-08, 3.325E-08, 3.927E-08, 4.608E-08, 5.356E-08, 6.194E-08, &
!        7.121E-08, 8.143E-08, 9.266E-08, 1.049E-07, 1.183E-07, 1.329E-07, &
!        1.487E-07, 1.658E-07, 1.843E-07, 2.041E-07, 2.255E-07, 2.484E-07, &
!        2.729E-07, 2.991E-07, 3.270E-07, 3.517E-07 /) 


REAL, PARAMETER :: NUMB1FLUX(0:40) = (/ &
         0.000E+00, 3.004E+01, 3.245E+02, 1.306E+03, 3.505E+03, 7.542E+03,  &
          1.410E+04, 2.394E+04, 3.787E+04, 5.674E+04, 8.147E+04, 1.130E+05,  &
          1.523E+05, 2.005E+05, 2.586E+05, 3.278E+05, 4.091E+05, 5.037E+05,  &
          6.129E+05, 7.379E+05, 8.800E+05, 1.041E+06, 1.220E+06, 1.422E+06,  &
          1.646E+06, 1.893E+06, 2.166E+06, 2.466E+06, 2.794E+06, 3.152E+06,  &
          3.541E+06, 3.962E+06, 4.419E+06, 4.911E+06, 5.441E+06, 6.011E+06,  &
          6.621E+06, 7.274E+06, 7.972E+06, 8.716E+06, 8.801E+06             /) 

REAL, PARAMETER :: NUMB2FLUX(0:40) = (/  &
          0.000E+00, 1.934E+01, 2.068E+02, 8.271E+02, 2.211E+03, 4.741E+03,  &
          8.841E+03, 1.497E+04, 2.363E+04, 3.534E+04, 5.066E+04, 7.017E+04,  &
          9.447E+04, 1.242E+05, 1.600E+05, 2.025E+05, 2.525E+05, 3.106E+05,  &
          3.776E+05, 4.542E+05, 5.413E+05, 6.395E+05, 7.501E+05, 8.726E+05,  &
          1.009E+06, 1.160E+06, 1.327E+06, 1.509E+06, 1.709E+06, 1.927E+06,  &
          2.163E+06, 2.420E+06, 2.697E+06, 2.996E+06, 3.318E+06, 3.664E+06,  &
          4.034E+06, 4.430E+06, 4.852E+06, 5.303E+06, 5.740E+06             /) 

REAL, PARAMETER :: NUMB3FLUX(0:40) = (/ 0.0, &
        4.340E-01, 5.217E+00, 2.241E+01, 6.301E+01, 1.404E+02, 2.703E+02, &
        4.699E+02, 7.584E+02, 1.157E+03, 1.687E+03, 2.373E+03, 3.240E+03, &
        4.314E+03, 5.625E+03, 7.197E+03, 9.063E+03, 1.126E+04, 1.380E+04, &
        1.674E+04, 2.011E+04, 2.393E+04, 2.827E+04, 3.311E+04, 3.853E+04, &
        4.457E+04, 5.126E+04, 5.864E+04, 6.675E+04, 7.564E+04, 8.535E+04, &
        9.592E+04, 1.074E+05, 1.198E+05, 1.333E+05, 1.478E+05, 1.633E+05, &
        1.801E+05, 1.980E+05, 2.172E+05, 2.353E+05 /) 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!!
!!    MESONH carries the following units during transport:
!!    M0=#/molec_{air}
!!    M6=um6/molec_{air}*1.d6
!!    The surface model should have (for sea salt)
!!    M0=#/m3*[kg_{dst}/mole_{dst}/XAVOGADRO]
!!    M3=kg/m3
!!    M6=um6/m3
!!    REFERENCE
!!    ---------
!!    Tulet et al, ORILAM manuscript for transformation of modal parameters
!!    J. Geophys. Res., 110, D18201, doi:10.1029/2004JD005716
!
!Initialize output which is total flux of sea salt (kg/m2/sec). 
IF (LHOOK) CALL DR_HOOK('COUPLING_SLT_N',0,ZHOOK_HANDLE)
!
!Factor which is needed so that all gains normal units when leaving ground paramn
ZCONVERTFACM0_SLT = XMOLARWEIGHT_SLT / XAVOGADRO !(kg_dst/mol_dst)/(molec/mol)
!Factor which is needed for moment 6, there is a factor 1.d6 transported around in M6 in MESONH
ZCONVERTFACM6_SLT = XMOLARWEIGHT_SLT / XAVOGADRO*1.d6
ZCONVERTFACM3_SLT = 4./3.*XPI*XDENSITY_SLT / 1.d18
!
PSFSLT(:,:)=0.d0
!
IF (CEMISPARAM_SLT.eq."Vig01")THEN
  !
  ! Vignatti et al. 2001 (in particles.cm-2.s-1)
  ZSFSLT_MDE(:,1) =  10**(0.09  *PWIND(:) + 0.283)   ! fine mode
  ZSFSLT_MDE(:,2) =  10**(0.0422*PWIND(:) + 0.288)   ! median mode
  ZSFSLT_MDE(:,3) =  10**(0.069 *PWIND(:) - 3.5)     ! coarse mode
  ! convert into  particles.m-2.s-1)
  ZSFSLT_MDE(:,1) = MAX(ZSFSLT_MDE(:,1) * 1E4, 1E-10)  
  ZSFSLT_MDE(:,2) = MAX(ZSFSLT_MDE(:,2) * 1E4, 1E-10)  
  ZSFSLT_MDE(:,3) = MAX(ZSFSLT_MDE(:,3) * 1E4, 1E-10)  
  !
ELSE ! Use Schultz et al., 2004
  !
  WCL(:) = INT(PWIND(:))
  WCL(:) = MAX (0, MIN(WCL(:), 39))
  !
  DZSPEED(:) = MAX(0., MIN(PWIND(:) - FLOAT(WCL(:)), 1.))
  !
  ! Flux given  in  particles.m-2 s-1
  !
  DO JI=1,KI
    !plm-gfortran
    ZSFSLT_MDE(JI,1) = NUMB1FLUX(WCL(JI)) + (NUMB1FLUX(WCL(JI)+1)-NUMB1FLUX(WCL(JI)))*DZSPEED(JI)
    ZSFSLT_MDE(JI,2) = NUMB2FLUX(WCL(JI)) + (NUMB2FLUX(WCL(JI)+1)-NUMB2FLUX(WCL(JI)))*DZSPEED(JI)
    ZSFSLT_MDE(JI,3) = NUMB3FLUX(WCL(JI)) + (NUMB3FLUX(WCL(JI)+1)-NUMB3FLUX(WCL(JI)))*DZSPEED(JI)
    !plm-gfortran
  ENDDO
  !
END IF
!
DO JN=1,JPMODE_SLT
  !
  IF (LVARSIG_SLT) THEN
    !
    PSFSLT(:,1+(JN-1)*3) = ZSFSLT_MDE(:,JORDER_SLT(JN))
    PSFSLT(:,2+(JN-1)*3) = PSFSLT(:,1+(JN-1)*3) * (SLT%XEMISRADIUS_SLT(JN)**3)*EXP(4.5 * LOG(SLT%XEMISSIG_SLT(JN))**2)  
    PSFSLT(:,3+(JN-1)*3) = PSFSLT(:,1+(JN-1)*3) * (SLT%XEMISRADIUS_SLT(JN)**6)*EXP(18. * LOG(SLT%XEMISSIG_SLT(JN))**2)  
    !
    ! Conversion into fluxes
    PSFSLT(:,1+(JN-1)*3) = PSFSLT(:,1+(JN-1)*3) * ZCONVERTFACM0_SLT
    PSFSLT(:,2+(JN-1)*3) = PSFSLT(:,1+(JN-1)*3) * ZCONVERTFACM3_SLT
    PSFSLT(:,3+(JN-1)*3) = PSFSLT(:,3+(JN-1)*3) * ZCONVERTFACM6_SLT

  ELSE IF (LRGFIX_SLT) THEN
    PSFSLT(:,JN) =  ZSFSLT_MDE(:,JORDER_SLT(JN)) * (SLT%XEMISRADIUS_SLT(JN)**3)*EXP(4.5 * LOG(SLT%XEMISSIG_SLT(JN))**2) 
    ! Conversion into fluxes
    PSFSLT(:,JN) = PSFSLT(:,JN) * ZCONVERTFACM3_SLT

  ELSE
    PSFSLT(:,1+(JN-1)*2) = ZSFSLT_MDE(:,JORDER_SLT(JN)) 
    PSFSLT(:,2+(JN-1)*2) = PSFSLT(:,1+(JN-1)*2) * (SLT%XEMISRADIUS_SLT(JN)**3)*EXP(4.5 * LOG(SLT%XEMISSIG_SLT(JN))**2) 

    ! Conversion into fluxes
    PSFSLT(:,1+(JN-1)*2) = PSFSLT(:,1+(JN-1)*2) * ZCONVERTFACM0_SLT
    PSFSLT(:,2+(JN-1)*2) = PSFSLT(:,1+(JN-1)*2) * ZCONVERTFACM3_SLT

  ENDIF
END DO

IF (LHOOK) CALL DR_HOOK('COUPLING_SLT_N',1,ZHOOK_HANDLE)
END SUBROUTINE COUPLING_SLT_n
