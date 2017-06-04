!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!####################################################################
    SUBROUTINE PREPS_FOR_MEB_DRAG(LCVEL, LFORC_MEASURE,                   &
                             PZ0, PZ0H, PZ0EFF, PH_VEG, PZREF,           & 
                             PTC, PTA, PQC, PQA, PUREF, PVMOD,            &
                             PEXNA, PEXNS, PDIRCOSZW, PDISPH,             &
                             PVELC, PZVMOD, PRI, PRA,                     &
                             PCH,PCDN,PCD                                 )
!
! typical values for nordic forest:
!     PZ0 = 0.8 m
!     PH_VEG =   15 m
!
!
!!****  *PREPS_FOR_MEB*
!!
!!    PURPOSE
!!    -------
!
!     Calculates the winds near top of vegetation (PVELC)
!     vegetation (PVELC). Used for input to surface_air_meb.
!     Only used for double energy balance
!     Also calculates PRA,PCH,PCDN and PCD
!         
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
!!
!!      P. Samuelsson/S.Gollvik           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2010
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
! LFORC_MEASURE= .TRUE. => heigh is given over ground
!                        .FALSE. => heigh is given over displacement height
!                         example from a numerical model PZREF=10 means that
!                         the level is 10 m above the displacement height
!
!
USE MODD_CSTS,              ONLY : XPI, XKARMAN, XG, XCPD, XRD
USE MODD_SURF_ATM,          ONLY : LDRAG_COEF_ARP, XRIMAX
USE MODD_ISBA_PAR,          ONLY : XLIMH
!
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_RI
USE MODI_WIND_THRESHOLD
!RJ: missing modi
USE MODI_SURFACE_CDCH_1DARP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   ::  PZ0, PZ0H, PZ0EFF,PH_VEG, PZREF, PUREF, PVMOD
!                                     PZ0  = roughness length for momentum
!                                     PZ0H  = roughness length for heat
!                                     PZ0EFF  = roughness length for momentum (orographic)
!                                     PH_VEG = height of the vegetation
!                                     PZREF = height of the lowest model layer
!                                     PUREF = reference height of the wind 
!                                             NOTE this is different from PZREF
!                                             ONLY in stand-alone/forced mode,
!                                             NOT when coupled to a model (MesoNH)
!                                     PVMOD = module of the horizontal wind
!
REAL, DIMENSION(:), INTENT(IN)   ::  PTC, PTA, PQC, PQA 
!                                     PTC = temperature of the canopy air
!                                     PTA = temperature of the lowest model layer
!                                     PQC = specific humidity of the canopy air
!                                     PQA = specific humidity of the lowest model level
!
REAL, DIMENSION(:), INTENT(IN)   ::  PEXNA, PEXNS, PDIRCOSZW, PDISPH
!                                     PEXNA = exner function at the lowest model level
!                                     PEXNS = exner function at the surface
!                                     PDIRCOSZW = Cosine of the angle between the normal
!                                                 to the surface and the vertical
!                                     PDISPH = displacement height
!
REAL, DIMENSION(:), INTENT(OUT)  ::  PVELC, PZVMOD, PRI, PRA, PCH, PCDN, PCD
!                                     PVELC  =  wind speed atr top of vegetation
!                                     PZVMOD = PMOD after wind threshold
!                                     PRI = Richardson number between canopy air and atm
!                                     PRA = aerodynamic resistance between canopy air
!                                           and lowest model layer
!                                     PCH = exchange coefficient for heat
!                                     PCDN = neutral exchange coefficient for momentum
!                                     PCD = exchange coefficient for momentum
!
LOGICAL, INTENT(IN)              ::   LCVEL, LFORC_MEASURE
!                                       LCVEL = switch for calculating PVELC
!                                       LFORC_MEASURE = switch for using measured data
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PZ0)) :: ZREVEG, ZCD, ZCUR, ZUCUR, ZRATVV, ZAC
REAL, DIMENSION(SIZE(PZ0)) :: ZBN, ZBD, ZCBNVV, ZCBSVV, ZCBUVV, ZREDVV
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZUL      = 1.        ! typical windspeed within the foliage
REAL, PARAMETER             :: ZNY      = 0.15e-04  ! kinematic viscosity for air
REAL, PARAMETER             :: ZVELCLIM = 0.1       ! safe minimum value ov PVELC
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_DRAG',0,ZHOOK_HANDLE)
!
PVELC(:) = ZVELCLIM
PRA(:)   = 0.
ZCUR(:)  = MAX(PZREF(:),XLIMH)
ZUCUR(:) = MAX(PUREF(:),XLIMH)
!
!Calculate height compared to dispacement height
!
IF(LFORC_MEASURE) THEN
!
   ZCUR(:)  = PZREF(:)-PDISPH(:)
   ZUCUR(:) = PUREF(:)-PDISPH(:)
!
   IF (ANY(ZCUR<0.0 .OR. ZUCUR<0.0))THEN
     PRINT *,'MAXVAL(PH_VEG)=',MAXVAL(PH_VEG)
     PRINT *,'MAXVAL(PDISPH)=',MAXVAL(PDISPH)
     PRINT *,'MINVAL(PZREF)=',MINVAL(PZREF)
     PRINT *,'MINVAL(PUREF)=',MINVAL(PUREF)
     STOP "Forcing height for wind or temperature too low!!"
   ENDIF
!
ELSE
!
! When running a numerical model it is assumed that PZREF is the distance between the 
! lowest model layer and displacement height, thus the "trees are below surface"
!
   ZCUR(:)  = MAX(PZREF(:),PH_VEG(:)-PDISPH(:)+XLIMH)
   ZUCUR(:) = MAX(PUREF(:),PH_VEG(:)-PDISPH(:)+XLIMH)
!
ENDIF
!
! Exner function at displacement height
! For consistancy displacement height has the same pressure as the surface
!
CALL SURFACE_RI(PTC, PQC, PEXNS, PEXNA, PTA, PQA,          &
                   ZCUR, ZUCUR, PDIRCOSZW, PVMOD, PRI )
!
PRI(:) = MIN(PRI(:),XRIMAX)
!
PZVMOD = WIND_THRESHOLD(PVMOD,ZUCUR)
!
IF (LDRAG_COEF_ARP) THEN

   CALL SURFACE_CDCH_1DARP(ZCUR, PZ0EFF, PZ0H, PZVMOD, PTA, PTC, &
                           PQA, PQC, PCD, PCDN, PCH              )
   PRA(:) = 1. / ( PCH(:) * PZVMOD(:) )

ELSE
!
!               -------------------------------------------------
!
   CALL SURFACE_AERO_COND(PRI, ZCUR, ZUCUR, PZVMOD, PZ0, PZ0H, ZAC, PRA, PCH)
!
!-------------------------------------------------------------------------------
!
!*       7.2    DRAG COEFFICIENT FOR MOMENTUM TRANSFERS
!               ---------------------------------------
!
!
   CALL SURFACE_CD(PRI, ZCUR, ZUCUR, PZ0EFF, PZ0H, PCD, PCDN)
!
ENDIF
!
IF(LCVEL) THEN
!
! In cases with very low model layer, we estimate the wind at treetop as that of
! the lowest layer
!
   ZRATVV(:) = MIN((PH_VEG(:)-PDISPH(:))/ZCUR(:),1.)
!
   ZBN(:)    = XKARMAN/SQRT(PCDN(:))
   ZBD(:)    = XKARMAN/SQRT(PCD(:))
   ZCBNVV(:) = ALOG(1.+(EXP(ZBN(:))-1.)*ZRATVV(:))
   ZCBSVV(:) = -(ZBN(:)-ZBD(:))*ZRATVV(:)
   ZCBUVV(:) = -ALOG(1.+(EXP(ZBN(:)-ZBD(:))-1.)*ZRATVV(:))
!
   WHERE(PRI>=0)
      ZREDVV(:) = (ZCBNVV(:) + ZCBSVV(:))/ZBD(:)
   ELSEWHERE
      ZREDVV(:) = (ZCBNVV(:) + ZCBUVV(:))/ZBD(:)
   END WHERE
!
   PVELC(:)     = MAX(ZREDVV(:)*PZVMOD(:),ZVELCLIM)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREPS_FOR_MEB_DRAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREPS_FOR_MEB_DRAG


