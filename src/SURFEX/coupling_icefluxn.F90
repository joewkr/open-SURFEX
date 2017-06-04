!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################
SUBROUTINE COUPLING_ICEFLUX_n(KI, PTA, PEXNA, PRHOA, PTICE, PEXNS,       &
                                PQA, PRAIN, PSNOW, PWIND, PZREF, PUREF,  &
                                PPS, PTWAT, PTTS, PSFTH, PSFTQ,          &
                                OHANDLE_SIC, PMASK, PQSAT, PZ0,          &
                                PUSTAR, PCD, PCDN, PCH,                  &
                                PRI, PRESA, PZ0H )
!     #######################################################################
!
!!****  *COUPLING_ICEFLUX_n * - Driver of the ICE_FLUX scheme   
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. DECHARME 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2010
!!      S.Senesi    01/2014 Add numerous optional output fields (transfer 
!!                           coeff, qsat...). Optionnaly use seaice cover
!!---------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_ICE_SEA_FLUX
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,             INTENT(IN)  :: KI        ! number of points
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PEXNA     ! Exner function at atm. level
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PTICE     ! Ice Surface Temperature
REAL, DIMENSION(KI), INTENT(IN)  :: PEXNS     ! Exner function at sea surface
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PWIND     ! module of wind at atm. wind level
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! atm. level for temp. and humidity
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! atm. level for wind
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PTWAT     ! Sea surface temperature
REAL,                INTENT(IN)  :: PTTS      ! Freezing point for sea water
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
!
LOGICAL, INTENT(IN) , OPTIONAL:: OHANDLE_SIC  ! Should we output extended set of fields
REAL, DIMENSION(KI), INTENT(IN) , OPTIONAL :: PMASK     ! Where to compute sea-ice fluxes (0./1.)  
!
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PQSAT     ! humidity at saturation
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PZ0       ! roughness length over the sea ice
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PUSTAR    ! friction velocity (m/s)
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCD       ! Drag coefficient
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCDN      ! Neutral Drag coefficient
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCH       ! Heat transfer coefficient
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PRI       ! Richardson number
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PRESA     ! aerodynamical resistance
REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PZ0H      ! heat roughness length over ice
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(KI)           :: IMASK
LOGICAL                          :: GHANDLE_SIC
INTEGER                          :: JJ, ISIZE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Preliminaries:
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ICEFLUX_N',0,ZHOOK_HANDLE)
PSFTH(:)=0.0
PSFTQ(:)=0.0
IF (PRESENT(OHANDLE_SIC)) THEN 
   GHANDLE_SIC=OHANDLE_SIC
ELSE
   GHANDLE_SIC=.FALSE.
ENDIF
!
IMASK(:)=0
ISIZE   =0
DO JJ=1,KI
   IF (GHANDLE_SIC) THEN 
      IF(PMASK(JJ)>0.)THEN
         ISIZE=ISIZE+1
         IMASK(ISIZE)=JJ
      ENDIF
   ELSE
      IF(PTWAT(JJ)<PTTS+10.)THEN
         ISIZE=ISIZE+1
         IMASK(ISIZE)=JJ
      ENDIF
   ENDIF
ENDDO
!
IF(ISIZE==0)THEN
  IF (LHOOK) CALL DR_HOOK('COUPLING_ICEFLUX_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
 CALL TREAT_ICE(ISIZE,IMASK)
!
!=======================================================================================
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ICEFLUX_N',1,ZHOOK_HANDLE)
CONTAINS
!
!=======================================================================================
SUBROUTINE TREAT_ICE(KSIZE,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
!
REAL, DIMENSION(KSIZE)  :: ZTA       ! air temperature forcing               (K)
REAL, DIMENSION(KSIZE)  :: ZEXNA     ! Exner function at atm. level
REAL, DIMENSION(KSIZE)  :: ZRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KSIZE)  :: ZTICE     ! Ice Surface Temperature
REAL, DIMENSION(KSIZE)  :: ZEXNS     ! Exner function at sea surface
REAL, DIMENSION(KSIZE)  :: ZQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KSIZE)  :: ZRR       ! liquid precipitation                  (kg/m2/s)
REAL, DIMENSION(KSIZE)  :: ZRS       ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KSIZE)  :: ZWIND     ! module of wind at atm. wind level
REAL, DIMENSION(KSIZE)  :: ZZREF     ! atm. level for temp. and humidity
REAL, DIMENSION(KSIZE)  :: ZUREF     ! atm. level for wind
REAL, DIMENSION(KSIZE)  :: ZPS       ! pressure at atmospheric model surface (Pa)
!
REAL, DIMENSION(KSIZE)  :: ZSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KSIZE)  :: ZSFTQ     ! flux of water vapor                   (kg/m2/s)

!        
REAL, DIMENSION(KSIZE)  :: ZZ0        ! roughness length over the sea ice
REAL, DIMENSION(KSIZE)  :: ZQSAT      ! humidity at saturation
REAL, DIMENSION(KSIZE)  :: ZUSTAR     ! friction velocity (m/s)
REAL, DIMENSION(KSIZE)  :: ZCD        ! Drag coefficient
REAL, DIMENSION(KSIZE)  :: ZCDN       ! Neutral Drag coefficient
REAL, DIMENSION(KSIZE)  :: ZCH        ! Heat transfer coefficient
REAL, DIMENSION(KSIZE)  :: ZRI        ! Richardson number
REAL, DIMENSION(KSIZE)  :: ZRESA      ! aerodynamical resistance
REAL, DIMENSION(KSIZE)  :: ZZ0H       ! heat roughness length over ice
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ICEFLUX_N:TREAT_ICE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
DO JJ=1, SIZE(ZTA)
   ZTA  (JJ) = PTA  (KMASK(JJ))
   ZEXNA(JJ) = PEXNA(KMASK(JJ))
   ZRHOA(JJ) = PRHOA(KMASK(JJ))
   ZTICE(JJ) = PTICE(KMASK(JJ))
   ZEXNS(JJ) = PEXNS(KMASK(JJ))
   ZQA  (JJ) = PQA  (KMASK(JJ))
   ZRR  (JJ) = PRAIN(KMASK(JJ))
   ZRS  (JJ) = PSNOW(KMASK(JJ))
   ZWIND(JJ) = PWIND(KMASK(JJ))
   ZZREF(JJ) = PZREF(KMASK(JJ))
   ZUREF(JJ) = PUREF(KMASK(JJ))
   ZPS  (JJ) = PPS  (KMASK(JJ))
END DO
!
! Local variables :
!
ZZ0   (:) = XUNDEF
ZQSAT (:) = XUNDEF
ZUSTAR(:) = XUNDEF
ZCD   (:) = XUNDEF    
ZCDN  (:) = XUNDEF
ZCH   (:) = XUNDEF
ZRI   (:) = XUNDEF
ZRESA (:) = XUNDEF
ZZ0H  (:) = XUNDEF
ZSFTH (:) = XUNDEF
ZSFTQ (:) = XUNDEF
!
!-------------------------------------------------------------------------------------
! Fluxes over ice according to Charnock formulae (or constant Cd)
!--------------------------------------------------------------------------------------
!
 CALL ICE_SEA_FLUX(ZZ0, ZTA, ZEXNA, ZRHOA, ZTICE, ZEXNS,      &
                    ZQA, ZRR, ZRS, ZWIND, ZZREF, ZUREF, ZPS,  &
                    ZQSAT, ZSFTH, ZSFTQ, ZUSTAR, ZCD, ZCDN,   &
                    ZCH, ZRI, ZRESA, ZZ0H                     )  
!                
!-------------------------------------------------------------------------------------
! Outputs:
!-------------------------------------------------------------------------------------
!
DO JJ=1, SIZE(ZSFTH)
   PSFTH (KMASK(JJ)) = ZSFTH (JJ)
   PSFTQ (KMASK(JJ)) = ZSFTQ (JJ)
   IF (GHANDLE_SIC) THEN 
      PQSAT (KMASK(JJ)) = ZQSAT (JJ)
      PZ0   (KMASK(JJ)) = ZZ0   (JJ)
      PUSTAR(KMASK(JJ)) = ZUSTAR(JJ)
      PCD   (KMASK(JJ)) = ZCD  (JJ)
      PCDN  (KMASK(JJ)) = ZCDN (JJ)
      PCH   (KMASK(JJ)) = ZCH  (JJ)
      PRI   (KMASK(JJ)) = ZRI  (JJ)
      PRESA (KMASK(JJ)) = ZRESA(JJ)
      PZ0H  (KMASK(JJ)) = ZZ0H (JJ)
   ENDIF
END DO
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ICEFLUX_N:TREAT_ICE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_ICE
!
!==========================================================================================
!
END SUBROUTINE COUPLING_ICEFLUX_n
