!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE LATLONTOXY1D(PLAT0,PLON0,PRPK,PBETA,PLATOR,PLONOR, &
                                 PXHATM,PYHATM,PLAT,PLON,KN,PRADIUS)  
!      ################################################
!
!!****  *SM_LATLONTOXY1D * - Routine to compute conformal coordinates
!!
!!
!!     PURPOSE
!!     -------
!        This routine computes the cartesian conformal coordinates 
!      of an array given in latitude-longitude coordinates
!        Three map projections are available: 
!      - polar-stereographic (PRPK=1),
!      - lambert conformal  (0<PRPK<1),
!      - mercator (PRPK=0).
!
!
!!**   METHOD
!!     ------
!!       Spherical earth approximation is used. Longitude origin is 
!!     set in Greenwich, and is positive eastwards. An anticlockwise 
!!     rotation of XBETA degrees is applied to the conformal frame 
!!     with respect to the geographical directions.
!!
!!       WARNING: ALL INPUT AND OUTPUT ANGLES ARE IN DEGREES...
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     EXPLICIT ARGUMENTS
!!     ------------------
!!       PXHAT,PYHAT(:)  : 1D arrays of the "velocity" gridpoints
!!                         cartesian conformal coordinates (meters,input).
!!       PLATOR   : Latitude of the (1,1) point of the "mass" grid
!!                      (degrees,input);
!!       PLONOR   : Longitude of the (1,1) point of the "mass" grid
!!                      (degrees,input);
!!       PXHATM   : conformal coordinate x  (meters, mass-grid, input)
!!       PYHATM   : conformal coordinate y  (meters, mass-grid, input)
!!       PLAT    : latitude                (degrees, mass-grid, output)
!!       PLON    : longitude               (degrees, mass-grid, output)
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!       Module MODD_CST         : contains Physical constants
!!          XPI         : Pi;    
!!          XRADIUS     : Earth radius (meters);
!!
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
!!       Original PM  24/05/94
!!       Updated  PM  27/07/94
!!       Updated  VD  23/08/94
!!       Updated  VM  24/10/95 projection from north pole (PRPK<0) and 
!!                             longitudes set between PLON0-180. and PLON0+180.
!!
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_CSTS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
INTEGER,             INTENT(IN) :: KN
REAL,                INTENT(IN) :: PLAT0
REAL,                INTENT(IN) :: PLON0
REAL,                INTENT(IN) :: PRPK
REAL,                INTENT(IN) :: PBETA
REAL,                INTENT(IN) :: PLATOR ! Latitude of the origine point
REAL,                INTENT(IN) :: PLONOR ! Longitude of the origine point
REAL, DIMENSION(KN), INTENT(IN) :: PLAT,PLON
REAL, DIMENSION(KN), INTENT(OUT):: PXHATM,PYHATM
REAL, OPTIONAL,      INTENT(IN) :: PRADIUS
!
!*     0.2    Declarations of local variables
! 
REAL,DIMENSION(KN) :: ZLAT,ZLON
REAL :: ZRPK,ZLAT0,ZLON0,ZLATOR,ZLONOR
REAL :: ZRDSDG,ZCLAT0,ZSLAT0,ZCLATOR,ZSLATOR
REAL :: ZRO0,ZGA0,ZBETA,ZCGAM,ZSGAM
REAL :: ZXP,ZYP,ZRACLAT0,ZXE,ZYE
REAL :: ZRADIUS
!
REAL,DIMENSION(KN) :: ZCLAT,ZSLAT,ZRO,ZGA,ZXPR,ZYPR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!*     1.     PRELIMINARY CALCULATION FOR ALL PROJECTIONS
!             -------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLONTOXY1D',0,ZHOOK_HANDLE)
ZRDSDG = XPI/180.         ! Degree to radian conversion factor
!
! By definition, (PLONOR,PLATOR) are the geographical 
! coordinates, and (ZXBM0,ZYBM0) the conformal cartesian 
! coordinates of the (1,1) point of the "mass" grid.
!
!
ZLON(:)=PLON(:)
ZLON(:)=ZLON(:)+NINT((PLON0-ZLON(:))/360.)*360.
!
ZLONOR=PLONOR
ZLONOR=ZLONOR+NINT((PLON0-ZLONOR)/360.)*360.
!
ZRADIUS = XRADIUS
IF (PRESENT(PRADIUS)) ZRADIUS = PRADIUS
!
!------------------------------------------------------------------------------
!
!*     2.     POLAR SEREOGRAPHIC AND LAMBERT CONFORMAL CASES
!             ----------------------------------------------
!                   (PRPK=1 P-stereo, 0<PRPK<1 Lambert)
!
IF(PRPK  /=  0.) THEN
!
  IF (PRPK<0.) THEN     ! projection from north pole
    ZRPK=-PRPK
    ZBETA=-PBETA
    ZLAT0=-PLAT0
    ZLON0=PLON0+180.
    ZLATOR=-PLATOR
    ZLONOR=ZLONOR+180.
    ZLAT(:)=-PLAT(:)
    ZLON(:)=ZLON(:)+180.
  ELSE                  ! projection from south pole
    ZRPK=PRPK
    ZBETA=PBETA
    ZLAT0=PLAT0
    ZLON0=PLON0
    ZLATOR=PLATOR
    ZLONOR=ZLONOR
    ZLAT(:)=PLAT(:)
    ZLON(:)=ZLON(:)
  ENDIF    
!
!*     2.1    Preliminary calculations
!
  ZCLAT0  = COS(ZRDSDG*ZLAT0)
  ZSLAT0  = SIN(ZRDSDG*ZLAT0)
  ZCLATOR = COS(ZRDSDG*ZLATOR)
  ZSLATOR = SIN(ZRDSDG*ZLATOR)
  ZRO0    = (ZRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)    &
            * ((1.+ZSLAT0)*ABS(ZCLATOR)/(1.+ZSLATOR))**ZRPK  
  ZGA0    = (ZRPK*(ZLONOR-ZLON0)-ZBETA)*ZRDSDG
  ZXP     = -ZRO0*SIN(ZGA0)
  ZYP     = ZRO0*COS(ZGA0)
!
!*    2.2    Conformal coordinates in meters
!
  ZCLAT(:)  = COS(ZRDSDG*ZLAT(:))
  ZSLAT(:)  = SIN(ZRDSDG*ZLAT(:))
  ZRO(:)    = (ZRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)    &
               * ((1.+ZSLAT0)*ABS(ZCLAT(:))/(1.+ZSLAT(:)))**ZRPK  
  ZGA(:)    = (ZRPK*(ZLON(:)-ZLON0)-ZBETA)*ZRDSDG
!
  PXHATM(:) = ZXP+ZRO(:)*SIN(ZGA(:))
  PYHATM(:) = ZYP-ZRO(:)*COS(ZGA(:))
!
  IF (PRPK<0.) THEN     ! projection from north pole
    PYHATM(:)=-PYHATM(:)
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
  ZRACLAT0 = ZRADIUS*COS(ZRDSDG*PLAT0)
  ZXE      = - ZRACLAT0*(PLONOR-PLON0)*ZRDSDG 
  ZYE      = - ZRACLAT0*LOG(TAN(XPI/4.+PLATOR*ZRDSDG/2.))
!
!*  3.2       Conformal coordinates
!
  ZXPR(:)   = ZRACLAT0*(ZLON(:)-PLON0)*ZRDSDG+ZXE
  ZYPR(:)   = ZRACLAT0*LOG(TAN(XPI/4.+PLAT(:)*ZRDSDG/2.))+ZYE
  !
  PXHATM = ZXPR(:)*ZCGAM-ZYPR(:)*ZSGAM
  PYHATM = ZXPR(:)*ZSGAM+ZYPR(:)*ZCGAM
!
!-------------------------------------------------------------------------------
!
!*  4.        EXIT
!             ----
!
END IF
IF (LHOOK) CALL DR_HOOK('LATLONTOXY1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE LATLONTOXY1D
