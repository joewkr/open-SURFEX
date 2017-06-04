!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!   ########################
MODULE MODE_DSLT_SURF
!!   ########################
!!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
CONTAINS
!!
!!   ############################################################
SUBROUTINE MASSFLUX2MOMENTFLUX(     &
          PFLUX,                    & ! [kg/m2/s] for M3, zero for other]
          PRHODREF,                 & ! [kg/m3] air density
          PEMISRADIUS,              & ! [um] emitted radius for the different modes
          PEMISSIG,                 & ! [-] emitted sigma for the different modes
          KMDE,                     &
          PCONVERTFACM0,            &
          PCONVERTFACM6,            &
          PCONVERTFACM3,            &
          OVARSIG,                  &
          ORGFIX                    &
          )  
!!   ############################################################
!!
!!    PURPOSE
!!    -------
!!    Transform emissions in mass (kg/m2/sec) to emissions of moments which have
!!    a bit strange units
!!    MESONH carries the following units during transport:
!!    M0=#/molec_{air}
!!    M3=molec_{dst}/molec_{air}
!!    M6=um6/molec_{air}*1.d6
!!    The surface model should have (for dust)
!!    M0=#/m3*[kg_{dst}/mole_{dst}/XAVOGADRO]
!!    M3=kg/m3
!!    M6=um6/m3
!!
!!    REFERENCE
!!    ---------
!!    Tulet et al, ORILAM manuscript for transformation of modal parameters
!!    J. Geophys. Res., 110, D18201, doi:10.1029/2004JD005716
!!
!!    AUTHOR
!!    ------
!!    Alf Grini and Pierre TULET (CNRM/GMEI)
!!
!!    MODIFICATIONS
!!    -------------
!!      J.Escobar     06/2013  for REAL4/8 add EPSILON management
!!    
!!
!!    EXTERNAL
!!    --------
!!    None
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
REAL,   DIMENSION(:,:),      INTENT(INOUT) :: PFLUX     !In; kg/m2/s (index #2, #5, #8 etc)
                                                        !Out: mole particles per mole air m/s *(MWdst/MWair*rhoair)(index #1)
                                                        !Out: kg/m2/s (index #2)
                                                        !Out: moles m6/moles air m/s *(MWdst/MWair*rhoair)(index #3)
REAL,   DIMENSION(:),        INTENT(IN)    :: PRHODREF  !I [kg/m3] density of air
REAL,   DIMENSION(:),        INTENT(IN)    :: PEMISRADIUS !I [um] emitted radius
REAL,   DIMENSION(:),        INTENT(IN)    :: PEMISSIG    !I [-] emitted sigma for the modes
INTEGER,        INTENT(IN)    :: KMDE
REAL,           INTENT(IN)    :: PCONVERTFACM0
REAL,           INTENT(IN)    :: PCONVERTFACM6
REAL,           INTENT(IN)    :: PCONVERTFACM3
LOGICAL,        INTENT(IN)    :: OVARSIG
LOGICAL,        INTENT(IN)    :: ORGFIX
!
!*      0.2    declarations local variables
!
REAL,DIMENSION(SIZE(PFLUX,1),3) :: ZFM               !Intermediate variable to get moments
!
INTEGER   :: JMODE  ! Counter for dust modes
INTEGER   :: JSV_IDX ! Counter for dust scalar variables
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!MESONH carries the following units during transport:
!M0=#/molec_{air}
!M3=molec_{dst}/molec_{air}
!M6=um6/molec_{air}*1.d6

!The surface should get the following units
!M0=#/m3*MW_DST/XAVOGADRO
!M3=kg/m3
!M6=um6/m3*1.d6 MW_DST/XAVOGADRO

!Emissions of dust are in kg/m2/sec for mode 3 at this point

!Factor which is needed so that all gains normal units when leaving ground paramn
IF (LHOOK) CALL DR_HOOK('MODE_DSLT_SURF:MASSFLUX2MOMENTFLUX',0,ZHOOK_HANDLE)

!Initialize initermediate moments
ZFM(:,:)=0.

DO JMODE=1,KMDE

  !Make index which is 0 for first mode, 3 for second, 6 for third etc
  IF (OVARSIG) THEN
    JSV_IDX = (JMODE-1)*3
  ELSE IF (ORGFIX) THEN
    JSV_IDX = JMODE-2
  ELSE
    JSV_IDX = (JMODE-1)*2
  END IF

  !IN THIS VERSION, MASS FLUX (kg/m2/sec) IS SENT IN INDEX #2, #5, #8 if 3 moments per mode
  !IF TWO MOMENTS PER MODE, MASS FLUX IS SENT AS INDEX #2, #4, #6

  !Get flux of number in #/m2/sec from flux of mass in kg/m2/sec
  ZFM(:,1) = PFLUX(:,JSV_IDX+2)                     & ! kg_{dst}/m2/sec
             / PEMISRADIUS(JMODE)**3                & ! *um^{-3} ==> #/m2/sec*(m3/um3)
             * EXP(-4.5*(LOG(PEMISSIG(JMODE)))**2)  & ! Take into account size distribution  
             / PCONVERTFACM3           ! /(kg_{dst}/m^{3}_{dst)} ==> m^3_{dst}/m2/sec  
 
   
  ! Get flux of moment 6 consistent with the other moments
  ZFM(:,3) = ZFM(:,1)                              & ! [#/m3]
             * (PEMISRADIUS(JMODE)**6)             & ! *um6 ==> um6/m2/sec 
             * EXP(18. *(LOG(PEMISSIG(JMODE)))**2)   ! Take into account size distribution  

  !Get flux of Moment 0 in transport units
  IF (.NOT.ORGFIX) THEN
    PFLUX(:,JSV_IDX+1) = ZFM(:,1)            & ! particles/m^2/sec
                         * PCONVERTFACM0       ! ==> particles/m2/sec * kg_dst/m3_{air}  
  END IF
   
  ! Flux moment 6
  IF (OVARSIG) THEN
    PFLUX(:,JSV_IDX+3) = ZFM(:,3)          & ! um^6/m^2/sec
                         * PCONVERTFACM6     ! ==>   
  ENDIF

  !Multiply with molecular weights so that you get back the units described above when
  !when multiply with the opposite variable in ground_paramn.f90
  !PFLUX(:,JSV_IDX+1) = PFLUX(:,JSV_IDX+1) * 100.E-3 * PRHODREF(:) / ZMD   !#_{aer}/molec_{air} m/s * kg_{aer}/m^3_{air}
  !IF (LVARSIG) PFLUX(:,JSV_IDX+3) = PFLUX(:,JSV_IDX+3) * 100.E-3 * PRHODREF(:) / ZMD   !um^6_{aer}/molec_{air}*cm^3/m^3 m/s kg_{aer}/m^3_{air}
  !
  !
ENDDO !Loop on modes
!
IF (LHOOK) CALL DR_HOOK('MODE_DSLT_SURF:MASSFLUX2MOMENTFLUX',1,ZHOOK_HANDLE)
!
END SUBROUTINE MASSFLUX2MOMENTFLUX

!**********************************************************************
!**********************************************************************
!**********************************************************************

SUBROUTINE DSLTMOMENT2SIZE(       &
          PSVT,                   & !I [XX/m3] input scalar variables (moment of distribution)
          PRHODREF,               & !I [kg/m3] density of air       
          PEMISRADIUS,            & ![um] emitted radius for the different modes
          PEMISSIG,               & ![-] emitted sigma for the different modes   
          KM0,                    &
          KM3,                    &
          KM6,                    &
          PCONVERTFACM0,          &
          PCONVERTFACM6,          &
          PCONVERTFACM3,          &
          OVARSIG,                &
          ORGFIX,                 &   
          PSIG1D,                 & !O [-] standard deviation of aerosol distribution
          PRG1D,                  & !O [um] number median diameter of aerosol distribution
          PN1D,                   & !O [#/m3] number concentration of aerosols
          PMASS1D,                & !O [kg/m3] mass concentration of aerosol
          PM1D                    & !O aerosols moments 0, 3 and 6
          )  
!!   ############################################################
!!
!!    PURPOSE
!!    -------
!!    Translate the three moments M0, M3 and M6 given in ppp into
!!    Values which can be understood more easily (R, sigma, N, M)
!!    At this point, M3 is in kg/m3, M0 in #/m3*(kg_{dst}/mole), M6 in um6/m3*1.d6*(kg_{dst}/mole)
!!
!!    All the moments have been transformed in MESONH (atmospheric model) so that the surface gets
!!    M0 [#/m3] *XMOLARWEIGHT_DST/XAVOGADRO
!!    M3 [kg/m3]
!!    M6 [um6/m3*1.d6] *XMOLARWEIGHT_DST/XAVOGADRO
!!   
!!    REFERENCE
!!    ---------
!!    Tulet et al, ORILAM manuscript for transformation of modal parameters
!!    J. Geophys. Res., 110, D18201, doi:10.1029/2004JD005716
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
USE MODD_SURF_PAR , ONLY : XSURF_TINY
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
REAL,       DIMENSION(:,:),  INTENT(IN)     :: PSVT      !I [ppp] moments in surface units
REAL,       DIMENSION(:),    INTENT(IN)     :: PRHODREF  !I [kg/m3] density of air
REAL,       DIMENSION(:),    INTENT(IN)     :: PEMISSIG  
REAL,       DIMENSION(:),    INTENT(IN)     :: PEMISRADIUS
INTEGER,DIMENSION(:),    INTENT(IN) :: KM0             ! [idx] index for Mode 0 in passed variables
INTEGER,DIMENSION(:),    INTENT(IN) :: KM3             ! [idx] indexes for Mode 3 in passed variables
INTEGER,DIMENSION(:),    INTENT(IN) :: KM6             ! [idx] indexes for Mode 6 in passed variables
REAL,           INTENT(IN)    :: PCONVERTFACM0
REAL,           INTENT(IN)    :: PCONVERTFACM6
REAL,           INTENT(IN)    :: PCONVERTFACM3
LOGICAL,        INTENT(IN)    :: OVARSIG
LOGICAL,        INTENT(IN)    :: ORGFIX
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PSIG1D   !O [-] standard deviation
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PRG1D    !O [um] number median diameter
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PN1D     !O [#/m3] number concentration
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PMASS1D  !O [kg_{aer}/m3] mass concentration
REAL,       DIMENSION(:,:),  OPTIONAL, INTENT(OUT)     :: PM1D     !O aerosols moments (MESONH units)
!
!*      0.2    declarations local variables
!
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2)) :: ZSV    ! [dusts moment concentration]
REAL,DIMENSION(SIZE(PSVT,1), SIZE(KM0)*3)  :: ZM     ! [moments] local array for moments  
REAL,DIMENSION(SIZE(PSVT,1))               :: ZSIGMA ! [-] standard deviation
REAL,DIMENSION(SIZE(PSVT,1))               :: ZRG    ! [um] number median diameter
INTEGER                   :: JN, J0, J3, J6          ! [idx] loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!        1.1    initialisation 
! 
!Get the conversion factors
IF (LHOOK) CALL DR_HOOK('MODE_DSLT_SURF:DSLTMOMENT2SIZE',0,ZHOOK_HANDLE)
!  
!Get scalar variable indexes
!
!Save the moments in a local array
ZSV(:,:) = MAX(PSVT(:,:),  XSURF_TINY)
!
DO JN=1,SIZE(KM0)

  J0 = 1 + (JN-1)*3 
  J3 = 2 + (JN-1)*3
  J6 = 3 + (JN-1)*3

  !calculate moment 3 from total aerosol mass in kg/m3 ==> um3/m3
  ZM(:,J3) = ZSV(:,KM3(JN))          & ! kg_{aer}/m3_{air}
             / PCONVERTFACM3           ! ==> m3_{dst}/m3_{air}

  IF (OVARSIG) THEN ! give M6 (case of variable standard deviation)
              
    !Get number concentration (#/molec_{air}==>#/m3)
    ZM(:,J0) = ZSV(:,KM0(JN))          & ! #/m3air*M_{dst}/avogadro 
               / PCONVERTFACM0           ! ==> #/m3  
                 
    !Calculate moment 6 from the sent value
    ZM(:,J6) = ZSV(:,KM6(JN))          & ! um6/m3_{air}*(cm3/m3)*M_{dst}/Avogadro
               / PCONVERTFACM6           ! ==> um6/m3  

    !Get sigma (only if sigma is allowed to vary)
    !Get intermediate values for sigma M3^2/(M0*M6) (ORILAM paper, eqn 8)
    ZSIGMA(:) = ZM(:,J3)**2 / (ZM(:,J0)*ZM(:,J6))
    !Limit the intermediate value, can not be larger than 1
    ZSIGMA(:) = MIN(1-1E-10,ZSIGMA(:))
    !Limit the value for intermediate, can not be smaller than 0
    ZSIGMA(:) = MAX(1E-10,ZSIGMA(:))
    !Calculate log(sigma)
    ZSIGMA(:) = LOG(ZSIGMA(:))
    !Finally get the real sigma the negative sign is because of 
    !The way the equation is written (M3^2/(M0*M6)) instead of (M0*M6)/M3^3
    ZSIGMA(:) = EXP(1./3.*SQRT(-ZSIGMA(:)))
        
  ELSE IF (ORGFIX) THEN ! compute M6 from M3, Rg and SIGMA    

    !Get the emitted sigma for this mode
    ZSIGMA(:) = PEMISSIG(JN)

    ZM(:,J0) = ZM(:,J3) /               &
              ((PEMISRADIUS(KM3(JN))**3)*EXP(4.5 * LOG(ZSIGMA(:))**2))  

  ELSE ! compute M6 from M0, M3 and SIGMA
          
    !Get the emitted sigma for this mode
    ZSIGMA(:) = PEMISSIG(JN)

    !Get number concentration (#/molec_{air}==>#/m3)
    ZM(:,J0) = ZSV(:,KM0(JN))        & ! #/m3air*M_{dst}/avogadro 
               / PCONVERTFACM0         ! ==> #/m3  

  END IF

  !Calculate moment 6 from this emitted sigma
  ZM(:,J6) = ZM(:,J0) * ((ZM(:,J3)/ZM(:,J0))**(1./3.) &
            * EXP(-(3./2.)*LOG(ZSIGMA(:))**2))**6     &
            * EXP(18.*LOG(ZSIGMA(:))**2)  

  !Get number median radius (eqn. 7 in Orilam manuscript)
  ZRG(:) = ((ZM(:,J3)**4) / (ZM(:,J6)*ZM(:,J0)**3)) ** (1./6.)     
  
  !Give the sigma-values to the passed array
  IF(PRESENT(PSIG1D)) PSIG1D(:,JN) = ZSIGMA(:)

  !Set the number concentrations in the passed array
  IF(PRESENT(PN1D)) PN1D(:,JN) = ZM(:,J0)

  !Get the number median radius
  IF(PRESENT(PRG1D)) PRG1D(:,JN)= ZRG(:)
    
  IF(PRESENT(PMASS1D))THEN
    PMASS1D(:,JN)=  ZM(:,J0)          &!#/m^3_{air}
                    * PCONVERTFACM3   &
                    * ZRG(:)**3 * EXP(4.5*(LOG(ZSIGMA(:)))**2)
  ENDIF

END DO  !Loop on modes

IF(PRESENT(PM1D)) PM1D(:,:) = ZM(:,:)

IF (LHOOK) CALL DR_HOOK('MODE_DST_SURF:DUSTMOMENT2SIZE',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE DSLTMOMENT2SIZE


END MODULE MODE_DSLT_SURF
