!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE TRIDIAG_GROUND_RM_COEFS(PTSTEP,PDEPTH,PTEMP,PHEATCAP,PCONDTRM,              &
     PSOURCE,PTDEEP_A,PTDEEP_B,PCONDA_DELZ,PA_COEF,PB_COEF)
!
!
!!****  *TRIDIAG_GROUND_RM_COEF*  
!!
!!    PURPOSE
!!    -------
!
!     This routine computes the coefficients of a Tridiagnoal matrix using
!     the method of Richtmeyer and Morton (1967): this routine corresponds to the **upward sweep**
!     The lower boundary condition is based on the linear eq ==> TN = B + A FN
!     Where B and A are imposed and correspond to either a Dirichlet (prescribed lower boundary T)
!     or a lower boundary flux (Neumann type). If B is FLAGGED, then
!     lower boundary flux is imposed as zero.
!     This routine is used to
!     eliminate T2(t+dt) (sub-surface T at time t+dt) from an energy balance Eq (where T1=Tsfc)
!     NOTE the solution is computed from TRIDIAG_GROUND_RM_SOLN.F90
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!
!     Richtmeyer, R. and K. Morton, 1967: Difference method for initial values problems,
!     Interscience Publishers, 2.
!
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/03/11 
!!      Modif       23/02/12 A. Boone: Optimization
!!      Modif       03/2013  A. Boone: MEB
!!      Modif       06/2013  A. Boone: use TN=B+A FN form for lower BC
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
REAL,                 INTENT(IN)  :: PTSTEP        ! time-step                         (s)
REAL, DIMENSION(:,:), INTENT(IN)  :: PDEPTH        ! soil layer depth                  (m)
REAL, DIMENSION(:,:), INTENT(IN)  :: PTEMP         ! surface and sub-surface soil 
!                                                  ! temperature profile               (K)
REAL, DIMENSION(:,:), INTENT(IN)  :: PHEATCAP      ! soil heat capacity                (J/K/m3)
REAL, DIMENSION(:,:), INTENT(IN)  :: PCONDTRM      ! soil thermal conductivity         (W/m/K)
REAL, DIMENSION(:,:), INTENT(IN)  :: PSOURCE       ! soil heat source function         (J/m2/s)
REAL, DIMENSION(:),   INTENT(IN)  :: PTDEEP_A, PTDEEP_B
!                                                  ! PTDEEP_A = Deep soil temperature
!                                                  ! coefficient depending on flux     (m2/K/W)
!                                                  ! PTDEEP_B = Deep soil temperature  (K)
!                                                  ! (prescribed)
!                                                  ! which models heating/cooling from
!                                                  ! below the diurnal wave penetration
!                                                  ! (surface temperature) depth. If it
!                                                  ! is FLAGGED as undefined, then the zero
!                                                  ! flux lower BC is applied.
!                                                  ! Tdeep = PTDEEP_B + PTDEEP_A * PDEEP_FLUX
!                                                  ! (with PDEEP_FLUX in W/m2)
!
REAL, DIMENSION(:),   INTENT(OUT) :: PCONDA_DELZ   ! ratio: ground flux thermal 
                                                   ! conductivity/ 
                                                   ! ground flux thickness             (W/m2/K)
REAL, DIMENSION(:,:), INTENT(OUT) :: PA_COEF       ! RM67 A-soil coefficient           (-)
REAL, DIMENSION(:,:), INTENT(OUT) :: PB_COEF       ! RM67 B-soil coefficient           (K)
!
!
!*      0.2    Declarations of local variables
!
INTEGER                                        :: JJ, JI, INL, INI
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZD_G             ! soil layer thicknesses             (m)
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZDLZ             ! thickness between layer mid_points (m)
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZLAMBDA          ! ratio of thermal cond to dz        (W/m2/K)
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZGHEATCAP        ! effective heat capacity         
!                                                                  ! coefficient                        (J/K/m2)
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZTHRM            ! interfacial therm. cond.           (W/m/K) 
REAL, DIMENSION(SIZE(PDEPTH,1))                :: ZC_COEF          ! working denominator for coefs
REAL, DIMENSION(SIZE(PDEPTH,1))                :: ZA_COEFD         !
REAL, DIMENSION(SIZE(PDEPTH,1))                :: ZB_COEFD         ! 
REAL, DIMENSION(SIZE(PDEPTH,1),SIZE(PDEPTH,2)) :: ZSINK            ! sink term (can be input)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    Declarations of local parameters
!
REAL, PARAMETER                                :: ZDZ_MIN = 1.E-6  ! m
!
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_RM_COEFS',0,ZHOOK_HANDLE)
!
!*       0.     Initialization
!               --------------
!
INI              = SIZE(PDEPTH,1)
INL              = SIZE(PDEPTH,2)
!
!*       1.     Compute needed parameters
!               -------------------------
! Needed parameters: interfacial thermal conductivity,
! layer thicknesses, and layer average heat capacities:

! Soil layer thicknesses, dz, from depths (base of each layer):

ZD_G(:,1)         = PDEPTH(:,1)
DO JJ=2,INL
   DO JI=1,INI
      ZD_G(JI,JJ) = PDEPTH(JI,JJ) - PDEPTH(JI,JJ-1)  
   ENDDO
ENDDO
ZD_G(:,:)         = MAX(ZDZ_MIN, ZD_G(:,:)) ! just for numerical reasons

! distance between layer mid_points (m):

DO JJ=1,INL-1
   DO JI=1,INI
      ZDLZ(JI,JJ)  = (ZD_G(JI,JJ)+ZD_G(JI,JJ+1))*0.5
   ENDDO
ENDDO
ZDLZ(:,INL)       = ZD_G(:,INL)*0.5

! effective heat capacity coefficient (J/K/m2):

ZGHEATCAP(:,:)    = ZD_G(:,:)*PHEATCAP(:,:)

! interfacial thermal conductivity (W/m/K):

DO JJ=1,INL-1
   DO JI=1,INI
      ZTHRM(JI,JJ)  = (ZD_G(JI,JJ)+ZD_G(JI,JJ+1))/              &
                     ((ZD_G(JI,JJ+1)/PCONDTRM(JI,JJ+1))+        &
                     (ZD_G(JI,JJ)  /PCONDTRM(JI,JJ)  ))
   ENDDO
ENDDO
ZTHRM(:,INL)        = PCONDTRM(:,INL) 
!
!
! Energy sink: 
! NOTE for now, set this part to zero as accounted for elsewhere.
!!!ZSINK(:,:)       = -PPHASE(:,:)*PTSTEP/ZGHEATCAP(:,:)
!
ZSINK(:,:)          = -PSOURCE(:,:)  ! J/m2/s
!
! Thermal cond/dz ratio (W m-2 K-1):
!
ZLAMBDA(:,:)        = ZTHRM(:,:)/ZDLZ(:,:)
!
! Save sfc interfacial thermal cond/dz ratio (W m-2 K-1):
!
PCONDA_DELZ(:)      = ZLAMBDA(:,1)
!
!
!*       2.     COEFFICIENTS
!               ------------
!
! Start at base of soil or snow etc...assuming an imposed lower boundary flux,
! and move up towards surface:

! initialize:

PA_COEF(:,:)        = 0.0
PB_COEF(:,:)        = 0.0

! lowest layer: flux is prescribed (can be 0)
! where GBFLUX = tcond/dz ( T_N - T_N+1 )  (W m-2)

WHERE(PTDEEP_B(:) == XUNDEF) ! no flux at model base

   ZC_COEF(:)     = (ZGHEATCAP(:,INL)/PTSTEP)                           &
                   + ZLAMBDA(:,INL-1)
   PA_COEF(:,INL) = ZLAMBDA(:,INL-1)/ZC_COEF(:)
   PB_COEF(:,INL) = ( PTEMP(:,INL)*(ZGHEATCAP(:,INL)/PTSTEP)            &
                   - ZSINK(:,INL) )/ZC_COEF(:)

   ZA_COEFD(:)    = XUNDEF
   ZB_COEFD(:)    = XUNDEF
   
ELSEWHERE 

! corresponds to Dirichlet or Neumann type lower BC (non-zero flux)

   ZC_COEF(:)     = 1. - PTDEEP_A(:)*ZLAMBDA(:,INL)
   ZA_COEFD(:)    = -ZLAMBDA(:,INL)/ZC_COEF(:)
   ZB_COEFD(:)    =  ZLAMBDA(:,INL)*PTDEEP_B(:)/ZC_COEF(:)

   ZC_COEF(:)     = (ZGHEATCAP(:,INL)/PTSTEP)                          &
                    + ZLAMBDA(:,INL-1) - ZA_COEFD(:)
   PA_COEF(:,INL) = ZLAMBDA(:,INL-1)/ZC_COEF(:)
   PB_COEF(:,INL) = ( PTEMP(:,INL)*(ZGHEATCAP(:,INL)/PTSTEP)           &
                    + ZB_COEFD(:) - ZSINK(:,INL) )/ZC_COEF(:)

END WHERE

! interior layers:

DO JJ=INL-1,2,-1
   DO JI=1,INI
      ZC_COEF(JI)    = (ZGHEATCAP(JI,JJ)/PTSTEP)                       &
                      + ZLAMBDA(JI,JJ)*(1.0-PA_COEF(JI,JJ+1))          &
                      + ZLAMBDA(JI,JJ-1)
      PA_COEF(JI,JJ) = ZLAMBDA(JI,JJ-1)/ZC_COEF(JI)
      PB_COEF(JI,JJ) = ( PTEMP(JI,JJ)*(ZGHEATCAP(JI,JJ)/PTSTEP)        &
                      + ZLAMBDA(JI,JJ)*PB_COEF(JI,JJ+1)                &
                      - ZSINK(JI,JJ) )/ZC_COEF(JI)
   ENDDO
ENDDO
!
! NOTE: uppermost coefficients have been kept at
!       zero (initial value)/not computed as they're 
!       not used in computations (implicit in surface E budget
!       computed elsewhere)
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_RM_COEFS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
END SUBROUTINE TRIDIAG_GROUND_RM_COEFS
