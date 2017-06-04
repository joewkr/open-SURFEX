!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE NITRO_DECLINE(IO, PK, PEK, OWOOD, PBSLAI_NITRO, PLAT, PBIOMASS_LEAF )  
!
!   ###############################################################
!!**  NITRO_DECLINE 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!     Calvet and Soussana (2001) and Gibelin et al. (2006) for nitrogen dilution.
!!     Gibelin et al. (2008) : New biomass reservoirs, and new method for allocation, 
!!     mortality and respiration.
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
!! Calvet and Soussana (2001), "Modelling CO2-enrichment effects using an
!! interactive vegetation SVAT scheme", Agricultural and Forest Meteorology, Vol. 108
!! pp. 129-152
!! Gibelin et al. (2008), "Modelling energy and CO2 fluxes with an interactive vegetation 
!! land surface model - Evaluation at high and middle latitudes", 
!! Agricultural and Forest Meteorology, Vol. 148 , pp. 1611-1628
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/01/03 
!!
!!      P Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays
!!      A.L. Gibelin 04/2009 : Suppress unused arguments
!!      A.L. Gibelin 04/2009 : Suppress unused modules and add ONLY
!!      A.L. Gibelin 04/2009 : adaptation to SURFEX environment
!!      A.   Barbu   01/2011 : modification of active biomass,leaf reservoir (see nitro_decline.f90)
!!      C.   Delire  04/2012 : spinup wood carbon
!!      R.   Alkama  04/2012 : 19 vegtype rather than 12
!!      B.   Decharme 05/2012: Optimization
!!                              ZCC_NITRO and ZBIOMASST_LIM in modd_co2v_par.F90
!!      C.   Delire   01/2014 : sapwood respiration from IBIS

!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_CSTS,           ONLY : XPI, XDAY
USE MODD_CO2V_PAR,       ONLY : XPCCO2, XCC_NIT, XCA_NIT, XMC, &
                                XMCO2, XCC_NITRO, XBIOMASST_LIM 
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, &
                                NVT_SHRB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
!*      0.1    declarations of arguments
!
LOGICAL, DIMENSION(:), INTENT(IN) :: OWOOD
!
REAL,   DIMENSION(:), INTENT(IN) :: PBSLAI_NITRO     ! ratio of biomass to LAI
REAL,   DIMENSION(:), INTENT(IN) :: PLAT             ! latitude of each grid point
!
REAL,   DIMENSION(:), INTENT(OUT)   :: PBIOMASS_LEAF ! temporary leaf biomass
!
!*      0.2    declarations of local variables
!
REAL                            :: ZBMCOEF
REAL,    DIMENSION(SIZE(PEK%XLAI,1))  :: ZXSEFOLD        ! e-folding time for senescence corrected (days)
REAL,    DIMENSION(SIZE(PEK%XLAI,1))  :: ZLAIB_NITRO     ! LAI correction parameter used in sefold calculation
REAL,    DIMENSION(SIZE(PEK%XLAI,1))  :: ZASSIM          ! assimilation
REAL,    DIMENSION(SIZE(PEK%XLAI,1))  :: ZBIOMASST       ! leaf + active structural biomass
!
REAL, DIMENSION(SIZE(PEK%XLAI,1),SIZE(PEK%XBIOMASS,2))  :: ZINCREASE
REAL, DIMENSION(SIZE(PEK%XLAI,1),SIZE(PEK%XBIOMASS,2))  :: ZBIOMASS      ! temporary biomass reservoirs
REAL, DIMENSION(SIZE(PEK%XLAI,1),SIZE(PEK%XBIOMASS,2))  :: ZDECLINE      ! biomass decline (storage+mortality) (kgDM m-2 day-1)
REAL, DIMENSION(SIZE(PEK%XLAI,1),SIZE(PEK%XBIOMASS,2))  :: ZSTORAGE      ! storage (part of decline kgDM m-2 day-1)
REAL, DIMENSION(SIZE(PEK%XLAI,1))                   :: ZMORT_LEAF    ! leaf mortality
!
REAL, DIMENSION(SIZE(PEK%XLAI,1))                   :: ZWORK,ZRESP
LOGICAL, DIMENSION(SIZE(PEK%XLAI,1))                :: GMASK_ASSIM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JSPIN, JI, INI
!
! correspondence between array indices and biomass compartments
! LEAF = 1
! STRUCT_ACT = 2
! STRUCT_PAS = 3
! STRUCT_BELOW = 4
! WOOD_ABOVE = 5
! WOOD_BELOW = 6
!
!-------------------------------------------------------------------------------
!
! 1 - Initialisations
! -------------------
!
IF (LHOOK) CALL DR_HOOK('NITRO_DECLINE',0,ZHOOK_HANDLE)
!
INI = SIZE(PEK%XLAI,1)
!
ZXSEFOLD(:)         = 0.0
ZLAIB_NITRO(:)      = 0.0
ZBIOMASST(:)        = 0.0
ZASSIM(:)           = 0.0
ZBIOMASS(:,:)       = 0.0
ZDECLINE(:,:)       = 0.0
ZINCREASE(:,:)      = 0.0
ZSTORAGE(:,:)       = 0.0
ZMORT_LEAF(:)       = 0.0
!---------------------------------------------------
!
ZBMCOEF     = XMC/(XMCO2*XPCCO2)
!
!-----------------------------------------------------------------
!avoid possible but unlikely negative values for biomass:        
!
PEK%XBIOMASS(:,1) = MAX(PEK%XBIOMASS(:,1),0.0)
!
! current leaf biomass value:
!
PBIOMASS_LEAF(:) = PEK%XBIOMASS(:,1)
!
!-------------------------------------------------------------------------------
!
! Once a day (at midnight),repartition of net assimilation and mortality 
! into different biomass compartments.
!
! 2 - Evolution of leaf biomass and senescence calculations
! ---------------------------------------------------------
!
! coef c for biomass in kg/m2 now in modd_co2v_par.F90 (XCC_NITRO)
!
! LAI correction for shadow effect
IF (IO%LTR_ML) THEN
  ZLAIB_NITRO(:) = 5.30
ELSE
  ZLAIB_NITRO(:) = MAX( 5.76-0.64*ATAN(ABS(PLAT(:))*XPI/180.),3.8 )
ENDIF
!
!
! leaf life expectancy
!
ZWORK(:) = 0.0
WHERE(PEK%XGMES(:)>0.0)
      ZWORK(:) = 0.321*LOG(PEK%XGMES(:)*1000.)
      ZWORK(:) = EXP(ZWORK(:))*PEK%XLAI(:)/ZLAIB_NITRO(:)
ENDWHERE
! before optimization
!ZXSEFOLD(:)= PEK%XSEFOLD(:) * MAX(((PEK%XGMES(:)*1000.)**0.321)*PEK%XLAI(:)/ZLAIB_NITRO(:), 1.) * ...
ZXSEFOLD(:) = PEK%XSEFOLD(:) * MAX(1.0,ZWORK(:)) * MIN(1.0,PEK%XANFM(:)/PK%XANMAX(:)) / XDAY
!
! avoid possible but unlikely division by zero
!
ZXSEFOLD(:) = MAX(1.0E-8,ZXSEFOLD(:))
!
! limitation of leaf life expectancy
!
! OLD   ZXSEFOLD(:) = MAX(5.,ZXSEFOLD(:))
! Following Marita's work limitation of the senesence
ZXSEFOLD(:) = MAX(PEK%XSEFOLD(:)/XDAY/10.0,ZXSEFOLD(:))
!
! senesence of active biomass
!
ZDECLINE(:,1) = MIN(PBIOMASS_LEAF(:)-PEK%XLAIMIN(:)*PBSLAI_NITRO(:), &
                    PBIOMASS_LEAF(:)*(1.0-EXP(-1.0/ZXSEFOLD(:))))
!
! avoid negative values due to computation precision
!
ZDECLINE(:,1) = MAX(ZDECLINE(:,1),0.0)
!
! current leaf biomass with assimilation and senescence
!
PBIOMASS_LEAF(:) = PBIOMASS_LEAF(:) - ZDECLINE(:,1)
!
! daily active biomass assimilation
!
ZASSIM(:) = PEK%XANDAY(:)*ZBMCOEF
!
!-------------------------------------------------------------------------------
!
! 3 - Evolution of active structural biomass
! ------------------------------------------
!
ZWORK(:) = 0.0
WHERE(PBIOMASS_LEAF(:)>0.0)
      ZWORK(:) = (1.0/(1.0-XCA_NIT))*LOG(PBIOMASS_LEAF(:)/XCC_NITRO)
      ZWORK(:) = EXP(ZWORK(:))
ENDWHERE
!
WHERE (ZASSIM(:) >= ZDECLINE(:,1))
  !
  ! 3.1 - Growing phase : plant nitrogen decline theory
  !
  ! the growth allometric law is applied
  ! repartition of total biomass    
  !
  !before optimization
  !ZBIOMASST(:)= MAX(PBIOMASS_LEAF(:), (PBIOMASS_LEAF(:)/XCC_NITRO)**(1.0/(1.0-XCA_NIT)))  
  ZBIOMASST(:) = MAX(PBIOMASS_LEAF(:), ZWORK(:))
  !
  ! active structural biomass increment and storage
  !
  ZBIOMASS(:,2)  = ZBIOMASST(:)  - PBIOMASS_LEAF(:)
  ZDECLINE(:,2)  = ZBIOMASS(:,2) * (1.0-EXP(-1.0*XDAY/PEK%XSEFOLD(:)))
  ZSTORAGE(:,1)  = ZBIOMASS(:,2) - PEK%XBIOMASS(:,2) + ZDECLINE(:,2) + PEK%XRESP_BIOMASS(:,2)
  !
ELSE WHERE
  !
  ! 3.2 - Senescence phase
  !
  ! the active structural biomass dies exponentially at the lowest rate
  !
  ZSTORAGE(:,1) = 0.0
  ZDECLINE(:,2) = PEK%XBIOMASS(:,2) * (1.0-EXP(-1.0*XDAY/PEK%XSEFOLD(:)))
  ZBIOMASS(:,2) = PEK%XBIOMASS(:,2) - ZDECLINE(:,2) - PEK%XRESP_BIOMASS(:,2)
  !
  !  Avoid negative values of biomass
  !  No test on ZDECLINE(:,2) as it is not used after, or recalculated
  !  No test on PEK%XRESP_BIOMASS(:,2,1) as it should be smaller than PEK%XBIOMASS(:,2,1)
  !  otherwise there are irrealistic values of temperature     
  !
  ZBIOMASS(:,2) = MAX(ZBIOMASS(:,2),0.0)
  !
  ZBIOMASST(:) = PBIOMASS_LEAF(:) + ZBIOMASS(:,2)
  !
END WHERE
!
! 3.3 - Flow to the passive structural biomass: cut or growth after senescence
! Biomass is taken from active structural biomass, not from senescence of leaves
! 
ZINCREASE(:,1) = ZASSIM(:)
ZINCREASE(:,2) = ZSTORAGE(:,1)
ZINCREASE(:,3) = -MIN(ZSTORAGE(:,1),0.0)
!
ZSTORAGE (:,1) = MAX(0.0,ZSTORAGE(:,1))
!
! 3.4 - Mass conservation : leaf biomass sensecence must be >= structural storage
!
WHERE( ZSTORAGE(:,1) > ZDECLINE(:,1))
  ZDECLINE(:,2)    = PEK%XBIOMASS(:,2) * (1.0 - EXP(-1.0*XDAY/PEK%XSEFOLD(:)))
  ZBIOMASST(:)     = PEK%XBIOMASS(:,1) + PEK%XBIOMASS(:,2) - ZDECLINE(:,2) - PEK%XRESP_BIOMASS(:,2)  
END WHERE
!
ZWORK(:) = 0.0
WHERE( ZBIOMASST(:) > 0.0)
      ZWORK(:) = (1.0-XCA_NIT)*LOG(ZBIOMASST(:))
      ZWORK(:) = EXP(ZWORK(:))
ENDWHERE
!
WHERE( ZSTORAGE(:,1) > ZDECLINE(:,1))
  !   
  !before optimization
  !PBIOMASS_LEAF(:)= ZCC_NITRO * (ZBIOMASST(:)**(1.0-XCA_NIT))
  PBIOMASS_LEAF(:) = XCC_NITRO * ZWORK(:)
  ZBIOMASS(:,2)    = ZBIOMASST(:)  - PBIOMASS_LEAF(:)
  ZDECLINE(:,1)    = PEK%XBIOMASS(:,1) - PBIOMASS_LEAF(:)
  ZSTORAGE(:,1)    = ZBIOMASS(:,2) - PEK%XBIOMASS(:,2) + ZDECLINE(:,2) + PEK%XRESP_BIOMASS(:,2)  
  !
  ZINCREASE(:,2) = ZSTORAGE(:,1)
  !
END WHERE
!
!-------------------------------------------------------------------------------
!
! 4 - Evolution of other biomass pools and final calculations
! -----------------------------------------------------------
!
! 4.1 - Mortality of leaf biomass
!
ZMORT_LEAF(:) = MAX(0.0, ZDECLINE(:,1) - ZSTORAGE(:,1))
!
ZBIOMASS(:,3) = PEK%XBIOMASS(:,3)
!
IF (IO%CPHOTO=='NIT') THEN
  !
  ! senesence of deep-structural biomass
  !
  ZDECLINE(:,3) = ZBIOMASS(:,3)*(1.0-EXP(-1.0*XDAY/PEK%XSEFOLD(:)))          
  !
  ! threshold value for leaf biomass and total above ground biomass in nitrogen
  ! dilution theory now in modd_co2v_par.F90 (XBIOMASST_LIM)
  !
  ! emergency deep structural biomass
  WHERE((ZBIOMASST(:) <= XBIOMASST_LIM) .AND. (ZXSEFOLD(:) > 1.0))
    ZBIOMASS(:,3) = ZBIOMASS(:,3) + ZMORT_LEAF(:)
  END WHERE
  !
ELSEIF (IO%CPHOTO=='NCB') THEN
  !
  ! 4.2 - Evolution of the other reservoirs
  ! 4.2.1 - senesence, avoiding negative values of biomass
  !
  ZDECLINE(:,3) = MIN(PEK%XBIOMASS(:,3)*(1.0-EXP(-1.0*XDAY/(PEK%XSEFOLD(:)/4.))), &
                      PEK%XBIOMASS(:,3)-PEK%XRESP_BIOMASS(:,3))            
  ZDECLINE(:,4) = MIN(PEK%XBIOMASS(:,4)*(1.0-EXP(-1.0*XDAY/PEK%XSEFOLD(:))), &
                      PEK%XBIOMASS(:,4)-PEK%XRESP_BIOMASS(:,4))
  !
  WHERE (OWOOD(:))
    ! Woody
    ZDECLINE(:,5) = MIN(PEK%XBIOMASS(:,5)*(1.0-EXP(-1.0*XDAY/PK%XTAU_WOOD(:))), &
                        PEK%XBIOMASS(:,5)-PEK%XRESP_BIOMASS(:,5))
    ZDECLINE(:,6) = PEK%XBIOMASS(:,6)*(1.0-EXP(-1.0*XDAY/PK%XTAU_WOOD(:)))
  ELSEWHERE
    ! Herbaceous
    ZDECLINE(:,5) = 0.
    ZDECLINE(:,6) = 0.
  END WHERE
  !
  ! 4.2.2 - storage (part of decline used as input for other reservoirs)
  !
  GMASK_ASSIM (:)=(ZASSIM(:) >= ZDECLINE(:,1))
  !
  WHERE (GMASK_ASSIM(:))
    !
    ! Remaining mortality is stored in roots.
    ZINCREASE(:,4)   = ZMORT_LEAF(:)
    !      
    ! Growing phase, all leaf decline is used as storage.
    ZSTORAGE(:,1)    = ZSTORAGE(:,1) + ZINCREASE(:,4)
    ZMORT_LEAF(:)    = ZMORT_LEAF(:) - ZINCREASE(:,4)
    !      
    ZSTORAGE(:,2)    = ZDECLINE(:,2)
    ZSTORAGE(:,3)    = ZDECLINE(:,3)
    !   
  ELSEWHERE
    !
    ! Senescence, a part of mortality is stored in roots, limited by assimilation rate.
    ZINCREASE(:,4)   = MIN(MAX(0.5*ZASSIM(:),0.) , 0.5*ZMORT_LEAF(:))
    !
    ZSTORAGE(:,1)    = ZSTORAGE(:,1) + ZINCREASE(:,4)
    ZMORT_LEAF(:)    = ZMORT_LEAF(:) - ZINCREASE(:,4)
    !   
  END WHERE
  !
  WHERE(GMASK_ASSIM(:).AND.OWOOD(:))
      ! Woody
      ZSTORAGE(:,4)  = ZDECLINE(:,4)
      !
      ZINCREASE(:,4) = ZINCREASE(:,4) + 0.3* (ZSTORAGE(:,2) + ZSTORAGE(:,3))
      ZINCREASE(:,5) =                  0.7* (ZSTORAGE(:,2) + ZSTORAGE(:,3))
      ZINCREASE(:,6) = ZSTORAGE(:,4)
      !
  ELSEWHERE(GMASK_ASSIM(:).AND..NOT.OWOOD(:))
      ! Herbaceous
      ZSTORAGE(:,4)  = 0.
      !
      ZINCREASE(:,4) = ZINCREASE(:,4) + ZSTORAGE(:,2) + ZSTORAGE(:,3)
      !
  END WHERE
  !
  WHERE (.NOT.GMASK_ASSIM(:).AND.OWOOD(:))
      ! Woody
      ! Senescence, only a part of decline is used as storage
      ZSTORAGE(:,2)  = 0.5*ZDECLINE(:,2)
      ZSTORAGE(:,3)  = 0.5*ZDECLINE(:,3)
      ZSTORAGE(:,4)  = 0.5*ZDECLINE(:,4)
      !
      ZINCREASE(:,5) = ZSTORAGE(:,2) + ZSTORAGE(:,3)
      ZINCREASE(:,6) = ZSTORAGE(:,4)
      !
  ELSEWHERE(.NOT.GMASK_ASSIM(:).AND..NOT.OWOOD(:))
      !  Herbaceous
      ! Senescence, no storage
      ZSTORAGE(:,2)  = 0.
      ZSTORAGE(:,3)  = 0.
      ZSTORAGE(:,4)  = 0.
      !
  END WHERE
  !
  ZSTORAGE(:,5) = 0.
  ZSTORAGE(:,6) = 0.
  !
  ! 4.2.3 - mortality (senescence - storage) and turnover
  !
  IF (IO%CRESPSL=='CNT') THEN
    PK%XTURNOVER(:,1) = ZMORT_LEAF(:)*1000.*XPCCO2/XDAY
    PK%XTURNOVER(:,2) = (ZDECLINE(:,2) - ZSTORAGE(:,2))*1000.*XPCCO2/XDAY
    PK%XTURNOVER(:,3) = (ZDECLINE(:,3) - ZSTORAGE(:,3))*1000.*XPCCO2/XDAY
    PK%XTURNOVER(:,4) = (ZDECLINE(:,4) - ZSTORAGE(:,4))*1000.*XPCCO2/XDAY
    PK%XTURNOVER(:,5) = (ZDECLINE(:,5) - ZSTORAGE(:,5))*1000.*XPCCO2/XDAY
    PK%XTURNOVER(:,6) = (ZDECLINE(:,6) - ZSTORAGE(:,6))*1000.*XPCCO2/XDAY
  ENDIF
  !
ENDIF
!
!
! 4.3 - Re-initialisations for next time step
!
ZBIOMASS(:,3) = ZBIOMASS(:,3) + ZINCREASE(:,3) - ZDECLINE(:,3) - PEK%XRESP_BIOMASS(:,3)
!
! Add net accumulated CO2 assimilation 
PBIOMASS_LEAF(:) = PBIOMASS_LEAF(:) + ZASSIM(:)
!
! re-initialisation of biomass compartments values: X(day) <-- X(day-1)
PEK%XBIOMASS(:,1) = PBIOMASS_LEAF(:)
PEK%XBIOMASS(:,2) = ZBIOMASS(:,2)
PEK%XBIOMASS(:,3) = ZBIOMASS(:,3)
!
! re-initialisation of respiration and assimilation terms
PEK%XRESP_BIOMASS(:,2) = 0.0
PEK%XRESP_BIOMASS(:,3) = 0.0
PEK%XANFM(:) = 0.0
!
!
! 4.2.4 - evolution of reservoirs
!
IF (IO%CPHOTO=='NIT') THEN
  !
  PEK%XBIOMASS(:,3) = MAX(PEK%XBIOMASS(:,3),0.0)
  !
ELSEIF (IO%CPHOTO=='NCB') THEN
  !
  ZBIOMASS(:,4) = PEK%XBIOMASS(:,4) + ZINCREASE(:,4) - ZDECLINE(:,4) - PEK%XRESP_BIOMASS(:,4)
  !
!
  ZBIOMASS(:,5) = PEK%XBIOMASS(:,5)
  ZBIOMASS(:,6) = PEK%XBIOMASS(:,6)
  ZRESP(:) = PEK%XRESP_BIOMASS(:,5)
!
  DO JSPIN = 1, IO%NSPINW
    DO JI = 1,INI
       IF(OWOOD(JI))THEN
         !Woody
         ZBIOMASS(JI,5) = ZBIOMASS(JI,5) + ZINCREASE(JI,5) - ZDECLINE(JI,5) - ZRESP(JI)
         ZBIOMASS(JI,6) = ZBIOMASS(JI,6) + ZINCREASE(JI,6) - ZDECLINE(JI,6)
         ZDECLINE(JI,5) = ZBIOMASS(JI,5)*(1.0-EXP((-1.0*XDAY)/PK%XTAU_WOOD(JI)))
         ZDECLINE(JI,6) = ZBIOMASS(JI,6)*(1.0-EXP((-1.0*XDAY)/PK%XTAU_WOOD(JI)))
         IF (PEK%XBIOMASS(JI,5).GT.0.0) &
                 ZRESP(JI) = PEK%XRESP_BIOMASS(JI,5)/PEK%XBIOMASS(JI,5) * ZBIOMASS(JI,5)  
       ELSE   
         !Herbaceous
         ZBIOMASS(JI,5) = 0.
         ZBIOMASS(JI,6) = 0.
       ENDIF
    ENDDO
  ENDDO
!
  PEK%XBIOMASS(:,4) = ZBIOMASS(:,4)
  PEK%XBIOMASS(:,5) = ZBIOMASS(:,5)
  PEK%XBIOMASS(:,6) = ZBIOMASS(:,6)
  !
  PEK%XRESP_BIOMASS(:,4) = 0.0
  PEK%XRESP_BIOMASS(:,5) = 0.0
  !
  PK%XINCREASE(:,:) = ZINCREASE(:,:)
 
ENDIF
!
IF (LHOOK) CALL DR_HOOK('NITRO_DECLINE',1,ZHOOK_HANDLE)
!
END SUBROUTINE NITRO_DECLINE
