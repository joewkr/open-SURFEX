!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################
      SUBROUTINE CANOPY_EVOL(SB, KI, PTSTEP, KIMPL, PZZ, PWIND, PTA, PQA, PPA, PRHOA, &
                             PSFLUX_U, PSFLUX_T, PSFLUX_Q, PFORC_U, PDFORC_UDU,  &
                             PFORC_E, PDFORC_EDE, PFORC_T, PDFORC_TDT, &
                             PFORC_Q, PDFORC_QDQ, PLM, PLEPS, PUSTAR,  &
                             PALFAU, PBETAU, PALFATH, PBETATH, PALFAQ, PBETAQ, &
                             ONEUTRAL     ) 
!     #########################################
!
!!****  *CANOPY_EVOL* - evolution of canopy
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
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_CSTS,        ONLY : XG, XRD, XCPD, XP00
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_CANOPY_TURB, ONLY : XCMFS, XCSHF
!
USE MODI_RMC01_SURF
USE MODI_CANOPY_EVOL_WIND
USE MODI_CANOPY_EVOL_TKE
USE MODI_CANOPY_EVOL_TEMP
!
USE MODE_SBLS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
REAL,                     INTENT(IN)    :: PTSTEP    ! atmospheric time-step                 (s)
INTEGER,                  INTENT(IN)    :: KIMPL     ! implicitation code: 
!                                                    ! 1 : computes only alfa and beta coupling
!                                                    !     coefficients for all variables
!                                                    ! 2 : computes temporal evolution of the
!                                                    !     variables
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PZZ       ! Mixing length generic profile at mid levels (-)
REAL, DIMENSION(KI),      INTENT(IN)    :: PWIND     ! wind speed                            (m/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PTA       ! Air temperature                       (K)
REAL, DIMENSION(KI),      INTENT(IN)    :: PQA       ! Air humidity                          (kg/m3)
REAL, DIMENSION(KI),      INTENT(IN)    :: PPA       ! Pressure at forcing level             (Pa)
REAL, DIMENSION(KI),      INTENT(IN)    :: PRHOA     ! Air density at forcing level          (kg/m3)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_U  ! surface flux u'w'                     (m2/s2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_T  ! surface flux w'T'                     (Km/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_Q  ! surface flux w'q'                     (kg/m2/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_E   ! tendency of TKE  due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! TKE  due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_T   ! tendency of Temp due to canopy drag   (T/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_TDT! formal derivative of the tendency of
!                                                    ! Temp due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PFORC_Q   ! tendency of Hum. due to canopy drag   (kg/m3/s)
REAL, DIMENSION(KI,SB%NLVL), INTENT(IN)    :: PDFORC_QDQ! formal derivative of the tendency of
!                                                    ! Hum. due to canopy drag               (1/s)
!
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PLM       ! mixing length                         (m)
REAL, DIMENSION(KI,SB%NLVL), INTENT(OUT)   :: PLEPS     ! dissipative length                    (m)
REAL, DIMENSION(KI),      INTENT(OUT)   :: PUSTAR    ! friction velocity at forcing level    (m/s)
!
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFATH  ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETATH  ! Th+(1) = alfa w'th'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFAQ   ! Q+(1) = alfa w'q'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETAQ   ! Q+(1) = alfa w'q'(1) + beta
!
LOGICAL, OPTIONAL, INTENT(IN) :: ONEUTRAL
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL :: GNEUTRAL
!
INTEGER :: JLAYER                              ! loop counter on layers
INTEGER :: JI                                  ! loop counter
!
REAL, DIMENSION(KI,SB%NLVL) :: ZK                 ! mixing coefficient
REAL, DIMENSION(KI,SB%NLVL) :: ZDKDDVDZ           ! formal derivative of mixing coefficient according to variable vertical gradient
REAL, DIMENSION(KI,SB%NLVL) :: ZTH                ! potential temperature at full levels
REAL, DIMENSION(KI)         :: ZTHA               ! potential temperature at forcing level
REAL, DIMENSION(KI,SB%NLVL) :: ZEXN               ! Exner function        at full levels
REAL, DIMENSION(KI,SB%NLVL) :: ZUW                ! friction at mid levels
REAL, DIMENSION(KI)         :: ZSFLUX_TH          ! Surface flux w'Th'                    (mK/s)
REAL, DIMENSION(KI,SB%NLVL) :: ZFORC_TH           ! tendency of Temp due to canopy drag   (T/s)
REAL, DIMENSION(KI,SB%NLVL) :: ZDFORC_THDTH       ! formal derivative of the tendency of
!                                              ! Temp due to canopy drag               (1/s)
REAL, DIMENSION(KI,SB%NLVL) :: ZWTH               ! w'Th' at mid levels
REAL, DIMENSION(KI,SB%NLVL) :: ZWQ                ! w'q'  at mid levels
REAL, DIMENSION(KI,SB%NLVL) :: ZSFTH              ! heat flux at atmospheric forcing level
REAL, DIMENSION(KI,SB%NLVL) :: ZSFRV              ! vapor flux at atmospheric forcing level
REAL                        :: ZZ0                ! a value of z0 just for first time-step init.
REAL, DIMENSION(KI,SB%NLVL) :: ZRHOA              ! air density profile
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL',0,ZHOOK_HANDLE)
!
GNEUTRAL = .FALSE. 
!
IF (PRESENT(ONEUTRAL)) GNEUTRAL = ONEUTRAL
!
!*    1. First time step initialization
!        ------------------------------
!
!* first time step (i.e. wind profile is initially zero) : 
!  set a neutral wind profile in relation with forcing wind
DO JI=1,KI
  IF(PWIND(JI)>0. .AND. SB%XU(JI,SB%NLVL-1)==0.) THEN
    ZZ0 = SB%XZ(JI,1)/2.
    SB%XU(JI,:) = PWIND(JI) * LOG (SB%XZ(JI,:)/ZZ0) / LOG (SB%XZ(JI,SB%NLVL)/ZZ0)
  END IF
END DO
!-------------------------------------------------------------------------------
!
!*    5. mixing and dissipative lengths (at full levels)
!        ------------------------------
!
 CALL RMC01_SURF(PZZ,SB%XLMO,PLM,PLEPS,GNEUTRAL)
!
!-------------------------------------------------------------------------------
!
!*    6. time evolution of wind in canopy
!        --------------------------------
!
!*    6.1 mixing coefficient for wind at mid layers (at half levels)
!         ---------------------------
!
ZK = -999.
DO JLAYER=2,SB%NLVL
  ZK(:,JLAYER) = 0.5 * XCMFS * PLM(:,JLAYER)   * SQRT(SB%XTKE(:,JLAYER)  ) &
               + 0.5 * XCMFS * PLM(:,JLAYER-1) * SQRT(SB%XTKE(:,JLAYER-1))  
END DO
!
!
!*    6.2 mixing coefficient vertical derivative at mid layers (at half levels)
!         --------------------------------------
!
!* there is no formal dependency on wind
ZDKDDVDZ = 0.
!
!
!*    6.3  time evolution of wind in canopy
!          --------------------------------
!
 CALL CANOPY_EVOL_WIND(SB, KI, PTSTEP, KIMPL, PWIND, ZK, ZDKDDVDZ,&
                       PSFLUX_U, PFORC_U, PDFORC_UDU, ZUW, PALFAU, PBETAU)
!
!*    6.4  Friction velocity at top of SBL layers
!          --------------------------------------
!
PUSTAR = SQRT(ABS(ZUW(:,SB%NLVL)))
!
!-------------------------------------------------------------------------------
!
IF (GNEUTRAL) THEN
  !
  ZTH  = 300.  
  ZWTH = 0.
  ZWQ  = 0.
  !
ELSE
  !
  !*    7. time evolution of temperature in canopy
  !        ---------------------------------------
  !
  !*    7.3 convertion into potential temperature (at half levels)
  !         -------------------------------------
  !
  DO JLAYER=1,SB%NLVL
    SB%XP(:,JLAYER) = PPA(:) + XG * PRHOA(:) * (SB%XZ(:,SB%NLVL) - SB%XZ(:,JLAYER))
  END DO
  ZEXN = (SB%XP/XP00)**(XRD/XCPD)
  !
  ZTH  = XUNDEF
  WHERE(SB%XT/=XUNDEF) ZTH(:,:) = SB%XT(:,:) / ZEXN(:,:)
  !
  ZTHA(:) = PTA(:) / ZEXN(:,SB%NLVL)
  !
  !*    7.1 mixing coefficient for wind at mid layers (at half levels)
  !         ---------------------------
  !
  ZK = -999.
  DO JLAYER=2,SB%NLVL
    ZK(:,JLAYER) = 0.5 * XCSHF * PLM(:,JLAYER)   * SQRT(SB%XTKE(:,JLAYER)  ) &
                 + 0.5 * XCSHF * PLM(:,JLAYER-1) * SQRT(SB%XTKE(:,JLAYER-1))  
  END DO
  !
  !*    7.2 mixing coefficient vertical derivative at mid layers (at half levels)
  !         --------------------------------------
  !
  !* there is no formal dependency on temperature
  ZDKDDVDZ = 0.
  !
  !*    7.4  conversion of canopy tendency into potential temperature tendency
  !          -----------------------------------------------------------------
  !
  ZSFLUX_TH    = PSFLUX_T / ZEXN(:,1)
  ZFORC_TH     = PFORC_T  / ZEXN
  ZDFORC_THDTH = PDFORC_TDT
  !
  !
  !*    7.5  time evolution of temperature in canopy
  !          ---------------------------------------
  !
  CALL CANOPY_EVOL_TEMP(SB, KI, PTSTEP, KIMPL, ZTHA, ZK, ZDKDDVDZ, &
                        ZSFLUX_TH, ZFORC_TH, ZDFORC_THDTH, ZTH, ZWTH, PALFATH, PBETATH)
  !
  !*    7.6  convertion into absolute temperature
  !          ------------------------------------
  !
  WHERE(SB%XT/=XUNDEF) SB%XT(:,:) = ZTH(:,:) * ZEXN(:,:)
  !
  !-------------------------------------------------------------------------------
  !
  !*    8. time evolution of Humidity in canopy
  !        ------------------------------------
  !
  CALL CANOPY_EVOL_TEMP(SB, KI, PTSTEP, KIMPL, PQA, ZK, ZDKDDVDZ, &
                        PSFLUX_Q, PFORC_Q, PDFORC_QDQ, SB%XQ, ZWQ, PALFAQ, PBETAQ)
  !
  !-------------------------------------------------------------------------------
  IF (KIMPL==1 .AND. LHOOK) CALL DR_HOOK('CANOPY_EVOL',1,ZHOOK_HANDLE)
  IF (KIMPL==1) RETURN
  !-------------------------------------------------------------------------------
  !
ENDIF
!
!*    9. time evolution of TKE in canopy
!        -------------------------------
!
 CALL CANOPY_EVOL_TKE(SB, KI, PTSTEP, PRHOA, PFORC_E, PDFORC_EDE, ZTH, ZUW, ZWTH, ZWQ, PLEPS) 
!
!-------------------------------------------------------------------------------
!
IF (.NOT.GNEUTRAL) THEN
  !
  !*   10. Monin-Obuhkov length
  !        --------------------
  !
  !* MO length is estimated using the heat and vapor turbulent fluxes at atmospheric level
  !  (it avoids the problems of vertical variation of the fluxes in the canopy)
  !  However, friction flux MUST be taken as the maximum flux on the
  !  profile, in order to avoid unrealistically small MO length when using
  !  small time-steps
  !
  ZRHOA(:,:) = SPREAD(PRHOA(:),2,SB%NLVL)
  !
  ZSFTH(:,:)  = ZWTH(:,:)
  ZSFRV(:,:)  = ZWQ (:,:) / ZRHOA(:,:)
  !
  SB%XLMO(:,:) = LMO(SQRT(ABS(ZUW)),SB%XT,SB%XQ,ZSFTH,ZSFRV)
  !
  DO JLAYER=1,SB%NLVL
    WHERE (SB%XLMO(:,JLAYER)>0.) SB%XLMO(:,JLAYER) = MAX(SB%XLMO(:,JLAYER),SB%XZ(:,SB%NLVL))
    WHERE (SB%XLMO(:,JLAYER)<0.) SB%XLMO(:,JLAYER) = MIN(SB%XLMO(:,JLAYER),-SB%XZ(:,SB%NLVL))     
  ENDDO
  !
  !-------------------------------------------------------------------------------
  !
  !*   11. Security at atmospheric forcing level
  !        -------------------------------------
  !
  SB%XT(:,SB%NLVL) = PTA(:)
  !
  SB%XQ(:,SB%NLVL) = PQA(:)
  !
ENDIF
!
SB%XU(:,SB%NLVL) = PWIND(:)
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE CANOPY_EVOL

