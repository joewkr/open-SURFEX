!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!      ###################################################
       SUBROUTINE OI_LATLON_CONF_PROJ(NDIM,PLAT0,PLON0,PRPK,PBETA,PLATOR,PLONOR, &
                                       PX,PY,PLAT,PLON                       )  
!      ###################################################
!
!!****  *LATLON_CONF_PROJ * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!        This routine computes the latitude and longitude of
!      an array given in cartesian conformal coordinates
!        Five map projections are available: 
!      - polar-stereographic from south pole  (PRPK=1),
!      - lambert conformal from south pole  (0<PRPK<1),
!      - mercator                             (PRPK=0),
!      - lambert conformal from north pole (-1<PRPK<0),
!      - polar-stereographic from north pole  (PRPK=-1).
!
!
!!**   METHOD
!!     ------
!!       Spherical earth approximation is used. Longitude origin is 
!!     set in Greenwich, and is positive eastwards. An anticlockwise 
!!     rotation of PBETA degrees is applied to the conformal frame 
!!     with respect to the geographical directions.
!!
!!       WARNING: ALL INPUT AND OUTPUT ANGLES ARE IN DEGREES...
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!      Asencio N. et al., 1994, "Le projet de modele non-hydrostatique
!!            commun CNRM-LA, specifications techniques", 
!!            Note CNRM/GMME, 26, 139p, (Chapter 2).
!!      Ducrocq V., 1994, "Generation de la grille dans le modele",
!!            Note interne MNH, 5 mai, 3p.
!!      Joly A., 1992, "Geographic parameters for ARPEGE/ALADIN",
!!            Internal note ARPEGE/ALADIN, february 27,28p.
!!      Levallois J., 1970, "Geodesie generale", Tome 2, Collection
!!             de l'IGN, Eyrolles, Paris, 408p.
!!       
!!     AUTHOR
!!     ------
!!      P.M.       *LA*
!!
!!     MODIFICATION
!!     ------------
!!       Original  PM  24/05/94
!!       Updated   PM  27/07/94
!!       Updated   VD  23/08/94
!!       Updated   VM  24/10/95 projection from north pole (PRPK<0) and 
!!                              longitudes set between PLON0-180. and PLON0+180.
!!       Update    VM  11/2004  externalized version
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_CSTS,ONLY : XPI, XRADIUS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,                 INTENT(IN) :: PLAT0  ! Reference latitude
REAL,                 INTENT(IN) :: PLON0  ! Reference longitude
REAL,                 INTENT(IN) :: PRPK   ! projection parameter 
!                                          !   K=1 : stereographic north pole
!                                          ! 0<K<1 : Lambert, north hemisphere
!                                          !   K=0 : Mercator
!                                          !-1<K<0 : Lambert, south hemisphere
!                                          !   K=-1: stereographic south pole
REAL,                 INTENT(IN) :: PBETA  ! angle between grid and reference longitude
REAL,                 INTENT(IN) :: PLATOR ! Latitude of the origine point
REAL,                 INTENT(IN) :: PLONOR ! Longitude of the origine point
REAL, DIMENSION(NDIM),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
REAL, DIMENSION(NDIM),   INTENT(OUT):: PLAT,PLON    
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
INTEGER,              INTENT(IN) :: NDIM
!
!*     0.2    Declarations of local variables
! 
REAL, DIMENSION(NDIM)     :: ZY
REAL                      :: ZRPK,ZBETA,ZLAT0,ZLON0,ZLATOR,ZLONOR
REAL                      :: ZRDSDG,ZCLAT0,ZSLAT0,ZCLATOR,ZSLATOR
REAL                      :: ZXBM0,ZYBM0,ZRO0,ZGA0 
REAL                      :: ZXP,ZYP,ZEPSI,ZT1,ZCGAM,ZSGAM,ZRACLAT0
!
REAL, DIMENSION(NDIM)     :: ZATA,ZRO2,ZT2,ZXMI0,ZYMI0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------------------------
!
!*     1.     Preliminary calculations for all projections
!             --------------------------------------------
!  
IF (LHOOK) CALL DR_HOOK('OI_LATLON_CONF_PROJ',0,ZHOOK_HANDLE)
ZRDSDG = XPI/180.             ! Degree to radian conversion factor
ZEPSI  = 10.*EPSILON(1.)      ! A small number
!
! By definition, (PLONOR,PLATOR) are the geographical 
! coordinates, and (ZXBM0,ZYBM0) the conformal cartesian 
! point of coordinates (x=0,y=0).
!
ZXBM0 = 0.
ZYBM0 = 0.
!
!-------------------------------------------------------------------------------
!
!*     2.     POLAR STEREOGRAPHIC AND LAMBERT CONFORMAL CASES
!             -----------------------------------------------
!                   (PRPK=1 P-stereo, 0<PRPK<1 Lambert)
!
IF(PRPK /= 0.) THEN
!
  IF (PRPK<0.) THEN     ! projection from north pole
    ZRPK=-PRPK
    ZBETA=-PBETA
    ZLAT0=-PLAT0
    ZLON0=PLON0+180.
    ZLATOR=-PLATOR
    ZLONOR=PLONOR+180.
    ZY(:)=-PY(:)
    ZYBM0=-ZYBM0
  ELSE                  ! projection from south pole
    ZRPK=PRPK
    ZBETA=PBETA
    ZLAT0=PLAT0
    ZLON0=PLON0
    ZLATOR=PLATOR
    ZLONOR=PLONOR
    ZY(:)=PY(:)
  ENDIF    
!
!*     2.1    Preliminary calculations
!
  ZCLAT0  = COS(ZRDSDG*ZLAT0)
  ZSLAT0  = SIN(ZRDSDG*ZLAT0)
  ZCLATOR = COS(ZRDSDG*ZLATOR)
  ZSLATOR = SIN(ZRDSDG*ZLATOR)
  ZRO0    = (XRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)     &
            * ((1.+ZSLAT0)*ABS(ZCLATOR)/(1.+ZSLATOR))**ZRPK  
  ZGA0    = (ZRPK*(ZLONOR-ZLON0)-ZBETA)*ZRDSDG
  ZXP     = ZXBM0-ZRO0*SIN(ZGA0)
  ZYP     = ZYBM0+ZRO0*COS(ZGA0)
!
!*    2.2    Longitude
!
  WHERE  (ABS(ZY(:)-ZYP) < ZEPSI    &
       .AND.ABS(PX(:)-ZXP) < ZEPSI)  
    ZATA(:) = 0.
  ELSEWHERE
    ZATA(:) = ATAN2(-(ZXP-PX(:)),(ZYP-ZY(:)))/ZRDSDG
  END WHERE
  !
  PLON(:) = (ZBETA+ZATA(:))/ZRPK+ZLON0
!
!*   2.3     Latitude
!
  ZRO2(:) = (PX(:)-ZXP)**2+(ZY(:)-ZYP)**2
  ZT1     = (XRADIUS*(ABS(ZCLAT0))**(1.-ZRPK))**(2./ZRPK)   &
            * (1+ZSLAT0)**2  
  ZT2(:)  = (ZRPK**2*ZRO2(:))**(1./ZRPK)
  !
  PLAT(:) = (XPI/2.-ACOS((ZT1-ZT2(:))/(ZT1+ZT2(:))))/ZRDSDG
!
  IF (PRPK<0.) THEN     ! projection from north pole
    PLAT(:)=-PLAT(:)
    PLON(:)=PLON(:)+180.
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*  3.        MERCATOR PROJECTION WITH ROTATION
!             ---------------------------------
!                       (PRPK=0)
!
ELSE
!
!*  3.1       Preliminary calculations
!
  ZCGAM    = COS(-ZRDSDG*PBETA)
  ZSGAM    = SIN(-ZRDSDG*PBETA)
  ZRACLAT0 = XRADIUS*COS(ZRDSDG*PLAT0)
!
!*  3.2       Longitude
!
  ZXMI0(:) = PX(:)-ZXBM0
  ZYMI0(:) = PY(:)-ZYBM0
!
  PLON(:) = (ZXMI0(:)*ZCGAM+ZYMI0(:)*ZSGAM)     &
            / (ZRACLAT0*ZRDSDG)+PLONOR  
!
!*  3.3       Latitude
!
  ZT1     = ALOG(TAN(XPI/4.+PLATOR*ZRDSDG/2.))
  ZT2(:)  = (-ZXMI0(:)*ZSGAM+ZYMI0(:)*ZCGAM)/ZRACLAT0
  !
  PLAT(:) = (-XPI/2.+2.*ATAN(EXP(ZT1+ZT2(:))))/ZRDSDG
!
!---------------------------------------------------------------------------------
!
!*  4.        EXIT
!             ----
!
END IF
PLON(:)=PLON(:)+NINT((PLON0-PLON(:))/360.)*360.
IF (LHOOK) CALL DR_HOOK('OI_LATLON_CONF_PROJ',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------------
END SUBROUTINE OI_LATLON_CONF_PROJ
