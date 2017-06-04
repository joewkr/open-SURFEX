!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CANOPY_EVOL_FIELD(KI, KLVL, PTSTEP, KIMPL, PK, PDKDDVDZ,       &
                                  PSFLUX_F, PFORC_F, PDFORC_FDF, PDZ, PDZF,     &
                                  PEXT, PDEXTDV, PF, PWF, PALFA, PBETA          ) 
!     #########################################
!
!!****  *CANOPY_EVOL_FIELD* - evolution of wind in canopy 
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_TRIDIAG_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL,                     INTENT(IN)    :: PTSTEP    ! time-step                             (s)
INTEGER,                  INTENT(IN)    :: KIMPL     ! implicitation code: 
!                                                    ! 1 : computes only alfa and beta coupling
!                                                    !     coefficients for all variables
!                                                    ! 2 : computes temporal evolution of the
!                                                    !     variables
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PK        ! mixing exchange coefficient           (m2/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDKDDVDZ  ! derivative of mixing coefficient as a
!                                                    ! function of vertical gradient of wind
!                                                    ! (at mid levels)                       (m2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_F  ! surface flux w'Th'                    (mK/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PFORC_F   ! tendency of wind due to canopy drag   (K/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDFORC_FDF! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZ       ! deltaZ between canopy half levels,
!                                                    ! located at full levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZF      ! deltaZ between canopy (full) levels,
!                                                    ! located at half levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PEXT      ! external forcing at full levels
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDEXTDV   ! derivative of external forcing as a
!                                                    ! function of vertical variable
!                                                    ! (at full levels)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PF        ! pot. temp. at canopy levels           (K)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PWF       ! turbulent flux (at half levels)       (mK/s)
REAL, DIMENSION(KI), OPTIONAL, INTENT(OUT)   :: PALFA     !  V+(1) = alfa F(1) + beta
REAL, DIMENSION(KI), OPTIONAL, INTENT(OUT)   :: PBETA     !  V+(1) = alfa F(1) + beta
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                     :: JLAYER   ! loop counter on layers
!
REAL, DIMENSION(KI,KLVL)   :: ZDFDZ    ! dTh/dz at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZWORK    ! work variable : wind at futur instant 
!                                      ! (or past at the end of the routine)
REAL, DIMENSION(KI,KLVL)   :: ZF       ! turbulent flux at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZDFDDVDZ ! derivative of turbulent flux as a
!                                      ! function of vertical gradient of wind variable
!                                      ! (at mid levels)
REAL, DIMENSION(KI)         :: ZDFADT  ! dTHa/dt   at forcing level
REAL, DIMENSION(KI)         :: ZDWFDZ  ! dw'Th'/dz at forcing level
REAL, DIMENSION(KI)         :: ZALFA, ZBETA
LOGICAL                     :: LIMPL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*    1. initializations
!        ---------------
!
!* external forces
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_FIELD',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    2. pot. temp. vertical derivative (at half levels below full levels)
!        ------------------------------
!
ZDFDZ(:,:) = -999.
DO JLAYER=2,KLVL
  ZDFDZ(:,JLAYER) = (PF(:,JLAYER) - PF(:,JLAYER-1)) / PDZF(:,JLAYER)
END DO
!
!-------------------------------------------------------------------------------
!
!*    3. turbulent flux (at half levels below full levels)
!        --------------
!
ZWORK    = -999.
!
!* surface flux
ZWORK(:,1) = PSFLUX_F(:)
!
!* flux at other levels
DO JLAYER=2,KLVL
  ZWORK(:,JLAYER) = -PK (:,JLAYER) * ZDFDZ(:,JLAYER)
END DO
!
!-------------------------------------------------------------------------------
!
!*    4. formal derivative of turbulent flux for variable X=(dU/dz)
!        ----------------------------------------------------------
!
!* no implicitation of surface flux
!
ZDFDDVDZ(:,:) = 0.
!
!* other levels (at half levels below full levels)
!
ZDFDDVDZ(:,2:KLVL) = - PK(:,2:KLVL) - PDKDDVDZ(:,2:KLVL) * ZDFDZ(:,2:KLVL)
!
!-------------------------------------------------------------------------------
!
!*    7. adds Forces & divergence of turbulent flux to dU/dt (at full levels)
!        ---------------------------------------------------
!
LIMPL=(KIMPL==1)
 CALL TRIDIAG_SURF(PF, ZWORK, ZDFDDVDZ, PEXT, PDEXTDV, PTSTEP, &
                  PDZF, PDZ, ZF, LIMPL, ZALFA, ZBETA    ) 
!
IF (PRESENT(PALFA)) PALFA = ZALFA
IF (PRESENT(PBETA)) PBETA = ZBETA
!
!-------------------------------------------------------------------------------
!
!*    8. updated turbulent flux (at half levels below full levels)
!        ----------------------
!
PWF(:,:) = -999.
PWF(:,1) = PSFLUX_F(:) 
!
DO JLAYER=2,KLVL
  PWF(:,JLAYER) = PWF(:,JLAYER-1)                                                         &
    + ( PFORC_F(:,JLAYER-1) + PDFORC_FDF(:,JLAYER-1) * (ZF(:,JLAYER-1)-PF(:,JLAYER-1)) )  &
      * PDZ(:,JLAYER-1) 
END DO
!
!-------------------------------------------------------------------------------
!
!*    8. New value of potential temperature (at full levels)
!        ----------------------------------
!
PF(:,:) = ZF(:,:)
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_FIELD',1,ZHOOK_HANDLE)
!----------------------------------------------------------------
!
END SUBROUTINE CANOPY_EVOL_FIELD
