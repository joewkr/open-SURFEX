!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!##########################
MODULE MODE_GRIDTYPE_GAUSS
!##########################
!
!############################################################################
!############################################################################
!############################################################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
CONTAINS
!
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRIDTYPE_GAUSS(PGRID_PAR,KNLATI, PLAPO,PLOPO,PCODIL,KNLOPA, &
                                    KL,PLAT,PLON,PLAT_XY,PLON_XY,PMESH_SIZE    , &
                                    PLONINF,PLATINF,PLONSUP,PLATSUP              )
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_GAUSS* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      (B. Decharme) 2008 multiples changes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN) :: KNLATI     ! number of pseudo-latitudes
REAL,                       INTENT(IN) :: PLAPO      ! latitude of the rotated pole (deg)
REAL,                       INTENT(IN) :: PLOPO      ! logitude of the rotated pole (rad)
REAL,                       INTENT(IN) :: PCODIL     ! stretching factor
INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA     ! number of pseudo-longitudes
!                                                    ! on each pseudo-latitude circle
!                                                    ! on pseudo-northern hemisphere
!                                                    ! (starting from the rotated pole)
INTEGER,                    INTENT(IN) :: KL         ! number of points used
REAL,   DIMENSION(:),       INTENT(IN) :: PLAT       ! latitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLON       ! longitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLAT_XY    ! pseudo-latitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PLON_XY    ! pseudo-longitudes of points
REAL,   DIMENSION(:),       INTENT(IN) :: PMESH_SIZE ! Mesh size
!                                                                               _____ Sup
REAL,   DIMENSION(:),       INTENT(IN) :: PLATSUP    ! Grid corner Latitude    |     |
REAL,   DIMENSION(:),       INTENT(IN) :: PLONSUP    ! Grid corner Longitude   |     |
REAL,   DIMENSION(:),       INTENT(IN) :: PLATINF    ! Grid corner Latitude    |_____|
REAL,   DIMENSION(:),       INTENT(IN) :: PLONINF    ! Grid corner Longitude  Inf
!
REAL, DIMENSION(:), POINTER :: PGRID_PAR         ! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PUT_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
ALLOCATE(PGRID_PAR(5+KNLATI+9*KL))
PGRID_PAR(1) = KNLATI
PGRID_PAR(2) = PLAPO
PGRID_PAR(3) = PLOPO
PGRID_PAR(4) = PCODIL
PGRID_PAR(5:4+KNLATI)= KNLOPA(:)
PGRID_PAR(5+KNLATI) = KL
PGRID_PAR(6+KNLATI:5+KNLATI+KL) = PLAT(:)
PGRID_PAR(6+KNLATI+KL:5+KNLATI+2*KL) = PLON(:)
PGRID_PAR(6+KNLATI+2*KL:5+KNLATI+3*KL) = PLAT_XY(:)
PGRID_PAR(6+KNLATI+3*KL:5+KNLATI+4*KL) = PLON_XY(:)
PGRID_PAR(6+KNLATI+4*KL:5+KNLATI+5*KL) = PMESH_SIZE(:)
PGRID_PAR(6+KNLATI+5*KL:5+KNLATI+6*KL) = PLONINF(:)
PGRID_PAR(6+KNLATI+6*KL:5+KNLATI+7*KL) = PLATINF(:)
PGRID_PAR(6+KNLATI+7*KL:5+KNLATI+8*KL) = PLONSUP(:)
PGRID_PAR(6+KNLATI+8*KL:5+KNLATI+9*KL) = PLATSUP(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:PUT_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_GAUSS(PGRID_PAR,KNLATI,                      &
                                      PLAPO,PLOPO,PCODIL,KNLOPA,KL,        &
                                      PLAT,PLON,PLAT_XY,PLON_XY,PMESH_SIZE,&
                                      PLONINF,PLATINF,PLONSUP,PLATSUP      )
!     ####################################################################
!
!!****  *GET_GRIDTYPE_GAUSS* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN) :: PGRID_PAR! parameters defining this grid
INTEGER, INTENT(OUT), OPTIONAL :: KNLATI   ! number of pseudo-latitudes
REAL,    INTENT(OUT), OPTIONAL :: PLAPO    ! latitude of the rotated pole (deg)
REAL,    INTENT(OUT), OPTIONAL :: PLOPO    ! logitude of the rotated pole (deg)
REAL,    INTENT(OUT), OPTIONAL :: PCODIL   ! stretching factor
INTEGER, DIMENSION(:), INTENT(OUT), OPTIONAL :: KNLOPA ! number of pseudo-longitudes
!                                                     ! on each pseudo-latitude circle
!                                                     ! on pseudo-northern hemisphere
!                                                     ! (starting from the rotated pole)
INTEGER, INTENT(OUT), OPTIONAL :: KL   ! number of points
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLAT    ! latitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLON    ! longitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLAT_XY ! pseudo-latitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLON_XY ! pseudo-longitude
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PMESH_SIZE ! Mesh size
!                                                                                  _____ Sup
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLATSUP    ! Grid corner Latitude    |     |
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLONSUP    ! Grid corner Longitude   |     |
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLATINF    ! Grid corner Latitude    |_____|
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT) :: PLONINF    ! Grid corner Longitude  Inf
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL
INTEGER :: INLATI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GET_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
IF (PRESENT(KNLATI))  KNLATI = PGRID_PAR(1)
IF (PRESENT(PLAPO))   PLAPO  = PGRID_PAR(2)
IF (PRESENT(PLOPO))   PLOPO  = PGRID_PAR(3)
IF (PRESENT(PCODIL))  PCODIL = PGRID_PAR(4)
!
INLATI = PGRID_PAR(1)
!
IF (PRESENT(KNLOPA)) THEN
  KNLOPA(:) = PGRID_PAR(5:4+INLATI)
END IF
!
IL    = PGRID_PAR(5+INLATI)
!
IF (PRESENT(KL)) THEN
  KL = IL
END IF
!
IF (PRESENT(PLAT)) THEN
  IF (SIZE(PLAT)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLAT')
  END IF
 PLAT(:) = PGRID_PAR(6+INLATI:5+INLATI+IL)
END IF
!
IF (PRESENT(PLON)) THEN
  IF (SIZE(PLON)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLON')
  END IF
 PLON(:) = PGRID_PAR(6+INLATI+IL:5+INLATI+2*IL)
END IF
!
IF (PRESENT(PLAT_XY)) THEN
  IF (SIZE(PLAT_XY)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLAT_XY')
  END IF
 PLAT_XY(:) = PGRID_PAR(6+INLATI+2*IL:5+INLATI+3*IL)
END IF
!
IF (PRESENT(PLON_XY)) THEN
  IF (SIZE(PLON_XY)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLON_XY')
  END IF
 PLON_XY(:) = PGRID_PAR(6+INLATI+3*IL:5+INLATI+4*IL)
END IF
!
IF (PRESENT(PMESH_SIZE)) THEN
  IF (SIZE(PMESH_SIZE)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PMESH_SIZE')
  END IF
 PMESH_SIZE(:) = PGRID_PAR(6+INLATI+4*IL:5+INLATI+5*IL)
END IF
!
IF (PRESENT(PLONINF)) THEN
  IF (SIZE(PLONINF)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLONINF')
  END IF
 PLONINF(:) = PGRID_PAR(6+INLATI+5*IL:5+INLATI+6*IL)
END IF
!
IF (PRESENT(PLATINF)) THEN
  IF (SIZE(PLATINF)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLATINF')
  END IF
 PLATINF(:) = PGRID_PAR(6+INLATI+6*IL:5+INLATI+7*IL)
END IF
!
IF (PRESENT(PLONSUP)) THEN
  IF (SIZE(PLONSUP)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLONSUP')
  END IF
 PLONSUP(:) = PGRID_PAR(6+INLATI+7*IL:5+INLATI+8*IL)
END IF
!
IF (PRESENT(PLATSUP)) THEN
  IF (SIZE(PLATSUP)/=IL) THEN
    CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: WRONG SIZE FOR PLATSUP')
  END IF
 PLATSUP(:) = PGRID_PAR(6+INLATI+8*IL:5+INLATI+9*IL)
END IF
!
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GET_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE LATLON_GAUSS(PLON_XY,PLAT_XY,KL,PLOPO,PLAPO,PCODIL,PLON,PLAT)
!     ####################################################################
!
!!****  *LATLON_GAUSS* - computes the coordinates on the real sphere
!!
!!    AUTHOR
!!    ------
!!      F. Taillefer  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS, ONLY : XPI
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                   INTENT(IN) :: KL      ! number of grid points
REAL,                      INTENT(IN) :: PLAPO   ! pole latitude
REAL,                      INTENT(IN) :: PLOPO   ! pole longitude
REAL,                      INTENT(IN) :: PCODIL  ! stretching factor
REAL,  DIMENSION(KL),      INTENT(IN) :: PLAT_XY ! pseudo-latitudes of points  (deg)
REAL,  DIMENSION(KL),      INTENT(IN) :: PLON_XY ! pseudo-longitudes of points (deg)
REAL,  DIMENSION(KL),      INTENT(OUT):: PLAT    ! latitudes of points  (deg)
REAL,  DIMENSION(KL),      INTENT(OUT):: PLON    ! longitudes of points (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
! called 1 variables : on the transformed sphere (rotated ans stretched)
! called 2 variables : on the rotated but no stretched sphere
! called 3 variables : on the real sphere

INTEGER :: JP
REAL :: ZCLO3,ZCONR,ZINTERM,ZLAT1,ZLAT2,ZLAT3,ZLON1,ZLON2,ZLON3
REAL :: ZLATP,ZLONP,ZSLA3,ZSLO3,ZR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATLON_GAUSS',0,ZHOOK_HANDLE)
!
ZCONR=XPI/180.
!
ZLONP=ZCONR*PLOPO
ZLATP=ZCONR*PLAPO
!
DO JP = 1,KL
  ZLON1=ZCONR*PLON_XY(JP)
  ZLAT1=ZCONR*PLAT_XY(JP)
! move from the stretched to the no stretched sphere
  ZINTERM=1./PCODIL*COS(ZLAT1)/MAX(1.E-12,1.+SIN(ZLAT1))
  ZLAT2=2.*ATAN((1.-ZINTERM)/(1.+ZINTERM))
  ZLON2=ZLON1
! move from the rotated sphere to the real one
!
! calculation of the latitude
  ZSLA3=-COS(ZLAT2)*COS(ZLON2)*COS(ZLATP)+SIN(ZLAT2)*SIN(ZLATP)
  IF (ZSLA3>1. .OR. ZSLA3<-1.) THEN
    WRITE(0,*) 'be carefull --> sinus >1'
    ZSLA3=MIN(1.,MAX(-1.,ZSLA3))
  ENDIF
  ZLAT3=ASIN(ZSLA3)
!
! calculation of the sine and cosine of the longitude
  ZSLO3=(COS(ZLAT2)*COS(ZLON2)*SIN(ZLATP)*SIN(ZLONP)&
      +COS(ZLAT2)*SIN(ZLON2)*COS(ZLONP)&
      +SIN(ZLAT2)*COS(ZLATP)*SIN(ZLONP)) / COS(ZLAT3)
  ZCLO3=(COS(ZLAT2)*COS(ZLON2)*SIN(ZLATP)*COS(ZLONP)&
      -COS(ZLAT2)*SIN(ZLON2)*SIN(ZLONP)&
      +SIN(ZLAT2)*COS(ZLATP)*COS(ZLONP)) / COS(ZLAT3)
!
! Conversion from rectangular to polar to get the longitude
  ZR=SQRT(ZCLO3*ZCLO3+ZSLO3*ZSLO3)
  IF (ZCLO3==0.) THEN
    IF (ZSLO3==0.) THEN
      ZLON3=0.
    ELSEIF (ZSLO3>0.) THEN
      ZLON3=XPI/2.
    ELSE
      ZLON3=-XPI/2.
    ENDIF
  ELSE
    ZINTERM=ATAN(ZSLO3/ZCLO3)
    IF (ZCLO3>=0.) THEN
      ZLON3=ZINTERM
    ELSEIF (ZSLO3>=0.) THEN
      ZLON3=ZINTERM+XPI
    ELSE
      ZLON3=ZINTERM-XPI
    ENDIF
  ENDIF
!
! Conversion from radians to degrees
  PLON(JP)=ZLON3/ZCONR
  IF (PLON(JP)<0.) PLON(JP)=PLON(JP)+360.
  PLAT(JP)=ZLAT3/ZCONR
!
END DO
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATLON_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE LATLON_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE COMP_GRIDTYPE_GAUSS(KNLATI,KNLOPA,KL,KTYP,PLAT_XY,PLON_XY)
!     ####################################################################
!
!!****  *COMP_GRIDTYPE_GAUSS* - computes the gaussian grid
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!!        F.Taillefer  10/2007 : adapt gaussian latitudes calculation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE EGGANGLES , ONLY : P_ASIN
USE MODD_CSTS,  ONLY : XPI
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                    INTENT(IN) :: KNLATI  ! number of pseudo-latitudes
INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA  ! number of pseudo-longitudes
!                                                 ! on each pseudo-latitude circle
!                                                 ! on pseudo-northern hemisphere
!                                                 ! (starting from the rotated pole)
INTEGER,                    INTENT(IN) :: KL      ! number of points used
INTEGER,                    INTENT(IN) :: KTYP    ! type of transform
REAL,   DIMENSION(KL),   INTENT(INOUT) :: PLAT_XY ! pseudo-latitudes of points  (deg)
REAL,   DIMENSION(KL),   INTENT(INOUT) :: PLON_XY ! pseudo-longitudes of points (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JL ! point loop counter
INTEGER :: JX ! longitude loop counter
INTEGER :: JY ! latitude loop counter
REAL                      :: ZRD, ZI
REAL, DIMENSION(KNLATI)   :: ZNLOPA, ZSINLA, ZWG
REAL, DIMENSION(KNLATI)   :: ZDSINLA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:COMP_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
!
ZRD = 180. / XPI
ZI=0.
IF (KTYP==1) ZI=180.
!
ZNLOPA=FLOAT(KNLOPA)
!
! gaussian latitudes calculation
 CALL LATITUDES_GAUSS(KNLATI,ZSINLA,ZDSINLA,ZWG)
!
JL=KNLATI/2
DO JY=1,JL
  ZWG(JY)= P_ASIN(ZSINLA(JY))
  ZWG(KNLATI+1-JY)= -P_ASIN(ZSINLA(JY))
END DO
!
JL=0
!* loop on latitudes (from north pole)
DO JY=1,KNLATI
!* loop on longitudes (from 0°, to the east)
  DO JX=1,KNLOPA(JY)
    JL=JL+1
    PLAT_XY(JL) = ZWG(JY)*ZRD
    PLON_XY(JL) = 360. * FLOAT(JX-1) / ZNLOPA(JY) + ZI
  END DO
END DO
!
IF (JL/=KL) THEN
  WRITE(0,*) ' PB in the total number of points of the gaussian grid '
  WRITE(0,*) '   check your namelist and rerun !'
  CALL ABOR1_SFX('MODE_GRIDTYPE_GAUSS: PB IN THE TOTAL NUMBER OF POINTS')
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:COMP_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE COMP_GRIDTYPE_GAUSS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GAUSS_GRID_LIMITS(KNLATI,KNLOPA,PXINF,PXSUP,PYINF,PYSUP)
!     ####################################################################
!
!!****  *GAUSS_GRID_LIMITS* - computes the gaussian grid "boxes"
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!!         F. Taillefer  08/2007  pb with the gaussian latitudes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE EGGANGLES , ONLY : P_ASIN
USE MODD_CSTS,  ONLY : XPI
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                   INTENT(IN) :: KNLATI! number of pseudo-latitudes
INTEGER, DIMENSION(KNLATI),INTENT(IN) :: KNLOPA! number of pseudo-longitudes
!                                              ! on each pseudo-latitude circle
!                                              ! on pseudo-northern hemisphere
!                                              ! (starting from the rotated pole)
REAL,   DIMENSION(:),      INTENT(OUT):: PXINF ! minimum pseudo longitude of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PXSUP ! maximum pseudo longitude of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PYINF ! minimum pseudo latitude  of the grid point (deg)
REAL,   DIMENSION(:),      INTENT(OUT):: PYSUP ! maximum pseudo latitude  of the grid point (deg)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JL ! point loop counter
INTEGER :: JX ! longitude loop counter
INTEGER :: JY ! latitude loop counter
REAL                    :: ZNLATI, ZRD
REAL, DIMENSION(KNLATI) :: ZNLOPA, ZSINLA, ZWG
REAL, DIMENSION(KNLATI) :: ZDSINLA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GAUSS_GRID_LIMITS',0,ZHOOK_HANDLE)
!
ZRD = 180. / XPI
!
ZNLATI=FLOAT(KNLATI)
ZNLOPA=FLOAT(KNLOPA)
!
! gaussian latitudes calculation
 CALL LATITUDES_GAUSS(KNLATI,ZSINLA,ZDSINLA,ZWG)
!
JL=KNLATI/2
DO JY=1,JL
  ZWG(JY)= P_ASIN(ZSINLA(JY))*ZRD
  ZWG(KNLATI+1-JY)= -P_ASIN(ZSINLA(JY))*ZRD
END DO
!
JL=0
!
!* loop on latitudes (from north pole)
DO JY=1,KNLATI
!* loop on longitudes (from 0°, to the east)
  DO JX=1,KNLOPA(JY)
    JL=JL+1
    IF (JY==1) THEN
      PYSUP(JL) = 90.
    ELSE
      PYSUP(JL)= ZWG(JY)+(ZWG(JY-1)-ZWG(JY))/2.
    ENDIF
    IF (JY==KNLATI) THEN
      PYINF(JL) = -90.
    ELSE
      PYINF(JL) = ZWG(JY)-(ZWG(JY)-ZWG(JY+1))/2.
    ENDIF
    PXSUP  (JL) = 360. * (FLOAT(JX)-0.5) / ZNLOPA(JY)
    PXINF  (JL) = 360. * (FLOAT(JX)-1.5) / ZNLOPA(JY)
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:GAUSS_GRID_LIMITS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GAUSS_GRID_LIMITS
!############################################################################
  !############################################################################
  !        ####################################################################
SUBROUTINE XY_GAUSS(PCODIL,KSIZE_DLAT,KSIZE_LON,PNODATA,PVALUE,PLAT_XY,PLON_XY)
  !      ####################################################################
  !
  !!****  *LATLON_GAUSS * - Routine to compute coordinates on a transform sphere
  !!                        from geographical coordinates
  !!
  !!     PURPOSE
  !!     -------
  !        This routine computes the latitude and longitude a real coordinates
  !        given array to an arpege model coordinates (rotated stretched)
  !
  !
  !
  !!**   METHOD
  !!     ------
  !!       use of rotations routines (eggmrt) and streching conformal formulae
  !!       to pass from real sphere (PLAT,PLON) transform sphere (PLAT_XY, PLON_XY)
  !!
  !!     EXTERNAL
  !!     --------
  !!       None
  !!
  !!     REFERENCE
  !!     ---------
  !!         Arpege DOC "Sphere Transphormee" Chapitre 7 version du 4/6/1991
  !!         J-D Gril for GEO_GAUSS 2005
  !!         J-D Gril Doc for EGGANGLES routines (new EGGX) 2005
  !!
  !!     AUTHOR
  !!     ------
  !!      J-D Gril
  !!
  !!     MODIFICATION
  !!     ------------
  !!       Original  10/2005
  !
  !-------------------------------------------------------------------------------
  !
  !*     0.     DECLARATIONS
  !             ------------
  !
  USE MODD_GET_MESH_INDEX_GAUSS, ONLY : XLON, XLAT, XCOST, XSINTC, XSINTS, XCOSN, XSINN, &
                                        XLONP, XLATP, XCOSP, XSINP, XPI, X1, X2, XDR
  !
  IMPLICIT NONE
  !
  !*     0.1    Declarations of arguments and results
  !
  REAL,                 INTENT(IN) :: PCODIL
  INTEGER,              INTENT(IN) :: KSIZE_DLAT
  INTEGER,              INTENT(IN) :: KSIZE_LON
  REAL,                 INTENT(IN) :: PNODATA
  REAL, DIMENSION(:),   INTENT(IN) :: PVALUE  ! value of the point to add
  REAL, DIMENSION(:),   INTENT(OUT):: PLAT_XY,PLON_XY
  !
  !*     0.2    Declarations of local variables
  !
  REAL :: ZCOS1, ZV1, ZINVS, ZLAT1, ZCOS2, ZLON1, ZINVC, ZSINN
  REAL :: ZV2,  ZSINT, ZCOST, ZV3, ZLAT2, ZM, ZLON2
  !
  INTEGER :: JJ, IDN, IDT
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE_OMP
  !--------------------------------------------------------------------------------
  !
  !*     1.     Preliminary calculations
  !             ------------------------
  !
  !
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:XY_GAUSS',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(IDN,IDT,ZCOS1, ZV1, ZLAT1, ZCOS2, ZLON1, ZINVC, &
!$OMP ZSINN, ZV2, ZINVS, ZSINT, ZCOST, ZV3, ZLAT2, ZM, ZLON2)
  DO JJ=1,SIZE(PVALUE)
    !
    IF (PVALUE(JJ)==PNODATA) CYCLE
    !
    IDN = MOD(JJ,KSIZE_LON)
    IF (IDN==0) IDN=KSIZE_LON
    IDT = CEILING(1.*JJ/KSIZE_DLAT)
    !
    ZCOS1 = XCOSN(IDN)*XCOST(IDT)
    !ZCOS1 = COS(XLAT(IDT))*COS(XLON(IDN)-XLONP)
    ZV1 = MIN(1.,MAX(-1.,XSINTS(IDT)+XCOSP*ZCOS1))
    !ZV1 = MIN(1.,MAX(-1.,SIN(XLATP)*SIN(XLAT(IDT))+COS(XLATP)*ZCOS1))
    ZLAT1 = ASIN(ZV1)
    !
    ZCOS2 = COS(ZLAT1)
    !
    ZLON1 = 0.0
    IF (ZCOS2 /= 0.0) THEN
      !ZINVC = 1./ZCOS2
      !ZSINN = - XCOST(IDT)*XSINN(IDN)*ZINVC
      !ZV2 = MIN(1.,MAX(-1.,(XSINTC(IDT)-XSINP*ZCOS1)*ZINVC))
      ZSINN = - XCOST(IDT)*XSINN(IDN)/ZCOS2
      !ZSINN = - COS(XLAT(IDT))*SIN(XLON(IDN)-XLONP)/ZCOS2
      ZV2 = MIN(1.,MAX(-1.,(XSINTC(IDT)-XSINP*ZCOS1)/ZCOS2))
      !ZV2 = MIN(1.,MAX(-1.,(COS(XLATP)*SIN(XLAT(IDT))-SIN(XLATP)*ZCOS1)/ZCOS2))
      ZLON1 = ACOS(ZV2) * SIGN(1.,ZSINN)
    ENDIF
    !
    !-----------------------
    !ZINVS = 1./(X2+X1*ZV1)
    !ZSINT = ZINVS*(X1+X2*ZV1)
    !ZCOST = ZINVS*ZCOS2*PCODIL*2.0
    ZSINT = (X1+X2*ZV1)/(X2+X1*ZV1)
    ZCOST = 2.0*PCODIL*ZCOS2/(X2+X1*ZV1)
    !
    ZV3 = MIN(1.,MAX(-1.,ZCOST))
    ZLAT2 = ACOS(ZV3)*SIGN(1.,ZSINT)
    !
    ZM = MOD(ZLON1,XPI)
    ZLON2 = (ZM-XPI*MOD(REAL(INT(ZLON1/XPI)),2.0))*SIGN(1.0,ZLON1)*SIGN(1.0,ZM)
    !
    !---------------------------------------------------------------------------------
    !
    !*     3.      EXIT
    !              ----
    !
    PLAT_XY(JJ) = ZLAT2 / XDR
    PLON_XY(JJ) = ZLON2 / XDR
    !
    IF (ABS(PLON_XY(JJ)-360.)<1.E-4) PLON_XY(JJ) = 0.
    !
  ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:XY_GAUSS',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!---------------------------------------------------------------------------------
  END SUBROUTINE XY_GAUSS
  !---------------------------------------------------------------------------------
  !############################################################################
  !        ####################################################################
  SUBROUTINE MAP_FACTOR_GAUSS(PLAPO,PLOPO,PCODIL, &
                                   PLAT,PLON,PMAP)
    !      ####################################################################
    !
    !!****  *MAP_FACTOR_GAUSS * - Routine to compute map factor for points
    !!      in real sphere coordinates
    !!
    !!
    !!     PURPOSE
    !!     -------
    !
    !!     REFERENCE
    !!     ---------
    !!         Arpege DOC "Sphere Transphormee" Chapitre 7 version du 4/6/1991
    !!         J-D Gril for GEO_GAUSS 2005
    !!         J-D Gril Doc for EGGANGLES routines (new EGGX) 2005
    !!
    !!     AUTHOR
    !!     ------
    !!      J-D Gril
    !!
    !!     MODIFICATION
    !!     ------------
    !!       Original  10/2005
    !!
    !-------------------------------------------------------------------------------
    !
    !*     0.     DECLARATIONS
    !             ------------
    !
    USE EGGANGLES,ONLY : LOLA, ANGLE_DOMAIN
    USE MODE_GEO_GAUSS,ONLY : MAP_FAC
    !
    IMPLICIT NONE
    !
    !*     0.1    Declarations of arguments and results
    !
    REAL,                 INTENT(IN) :: PLAPO  ! latitude of  pole
    REAL,                 INTENT(IN) :: PLOPO  ! longitude of pole
    REAL,                 INTENT(IN) :: PCODIL ! coefficient of dilatation
    REAL, DIMENSION(:),   INTENT(IN) :: PLAT   ! given geographic latitudes
    REAL, DIMENSION(:),   INTENT(IN) :: PLON   ! given geographic longitudes
    !
    REAL, DIMENSION(SIZE(PLAT)),   INTENT(OUT):: PMAP   ! map factor
    !
    !*     0.2    Declarations of local variables
    !
    !
    TYPE(LOLA)                          :: TZPOLE
    TYPE(LOLA), DIMENSION(SIZE(PLAT))   :: TZPTCI
    !
    REAL :: ZPI
    REAL :: ZDR
    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    !-------------------------------------------------------------------------------
    !
    !*     1.     PRELIMINARY CALCULATION
    !             -----------------------
    !
    IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MAP_FACTOR_GAUSS',0,ZHOOK_HANDLE)
    ZPI = 4.*ATAN(1.)
    ZDR = ZPI / 180.
    !
    TZPTCI(:)%LON = ANGLE_DOMAIN(PLON(:),DOM='0+',UNIT='D') * ZDR
    TZPTCI(:)%LAT = PLAT(:) * ZDR
    TZPOLE%LON    = ANGLE_DOMAIN(PLOPO,DOM='0+',UNIT='D') * ZDR
    TZPOLE%LAT    = PLAPO * ZDR
    !
    !-------------------------------------------------------------------------------
    !
    !*     2.    Calcul
    !            ------
    !
    PMAP(:) = MAP_FAC(TZPOLE,PCODIL,TZPTCI)
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MAP_FACTOR_GAUSS',1,ZHOOK_HANDLE)
    !
    !-------------------------------------------------------------------------------
  END SUBROUTINE MAP_FACTOR_GAUSS
  !-------------------------------------------------------------------------------
  !############################################################################
  !##################################
  SUBROUTINE LATITUDES_GAUSS(KN,PL,PDL,PW)
  !##################################

  !**** *SUGAW36 * - Routine to initialize the Gaussian
  !                  abcissa and the associated weights
  !                  Frozen CY36 version.

  !     Purpose.
  !     --------
  !           Initialize arrays PL,PDL and PW (quadrature abscissas and weights)
  !**   Interface.
  !     ----------
  !        *CALL* *SUGAW36(...) *

  !        Explicit arguments :
  !        --------------------
  !           INPUT:
  !              KN       : Number of Gauss abscissas

  !           OUTPUT:
  !              PL (KN)  : Abscissas of Gauss
  !              PDL(KN)  : Idem in quadruple precision (OPTIONNAL)
  !              PW (KN)  : Weights of the Gaussian integration

  !     PL (i) is the abscissa i starting from the northern pole, it is
  !     the cosine of the colatitude of the corresponding row of the collocation grid.

  !     KOUTENV  : Contains printings environment variables.
  !                KOUTENV(1): output logical unit.
  !                KOUTENV(2): option for level of printings.
  !
  !        Implicit arguments :
  !        --------------------
  !       None

  !     Method.
  !     -------
  !        See documentation

  !     Externals.
  !     ----------

  !     Reference.
  !     ----------

  !     S.L. Belousov, Tables of normalized associated Legendre Polynomials, Pergamon Press (1962)
  !     P.N. Swarztrauber, On computing the points and weights for Gauss-Legendre quadrature,
  !     SIAM J. Sci. Comput. Vol. 24 (3) pp. 945-954 (2002)

  !     Author.
  !     -------
  !      Mats Hamrud and Philippe Courtier  *ECMWF*
  !      Original          : 87-10-15

  !     Modifications.
  !     --------------
  !      Philippe Courtier : 92-12-19 Multitasking
  !      Ryad El Khatib    : 94-04-20 Remove unused comdecks pardim and yomdim
  !      Mats Hamrud       : 94-08-12 Printing level
  !      K. Yessad (Sep 2008): cleaning, improve comments.
  !      K. YESSAD (NOV 2008): make consistent arp/SUGAW36 and tfl/SUGAW.
  !      Nils Wedi + Mats Hamrud, 2009-02-05 revised following Swarztrauber, 2002
  !      K. Yessad (June 2009): externalisation + in-lining.
  !      GCO + K. Yessad (Dec 2012): restore quadruple precision features.
  !      K. Yessad (Dec 2012): simplify use of SUGAW36.
  !     ------------------------------------------------------------------


  !     ------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: KN
  REAL,INTENT(OUT)   :: PL(KN)
  REAL,INTENT(OUT)   :: PDL(KN)
  REAL,INTENT(OUT)   :: PW(KN)

  !     ------------------------------------------------------------------

  REAL, DIMENSION(0:KN,0:KN) :: ZFN
  REAL, DIMENSION(0:KN/2) :: ZFNLAT
  REAL, DIMENSION(KN) :: ZREG, ZLI, ZT, ZMOD, ZM, ZR

  REAL :: ZFNN, ZACOS
  REAL ::  ZPI, Z, ZRA, ZEPS, ZW, ZX, ZXN
  REAL :: ZDLX, ZDLXN, ZDLK, ZDLLDN, ZZDLXN, ZDLMOD

  INTEGER, DIMENSION(KN) :: ITER

  INTEGER :: JN, JGL, IODD, IK
  INTEGER :: INS2, IALLOW, ISYM, IFLAG, ITEMAX, JTER

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  !     ------------------------------------------------------------------

  !     ------------------------------------------------------------------
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS',0,ZHOOK_HANDLE)
  !     ------------------------------------------------------------------
  !*       1. Initialization.
  !           ---------------
  !
  !*       1.1 Calculation of ZFNLAT.
  !            (Fourier coefficients of series expansion for
  !            the ordinary Legendre polynomials).
  ! Belousov, Swarztrauber use ZFN(0,0)=SQRT(2._JPRB)
  ! IFS normalisation chosen to be 0.5*Integral(Pnm**2) = 1
  ZFN(0,0) = 2.
  DO JN=1,KN
    ZFNN=ZFN(0,0)
    DO JGL=1,JN
      ZFNN = ZFNN*SQRT(1.-0.25/(JGL**2))
    ENDDO
    IODD=MOD(JN,2)
    ZFN(JN,JN)=ZFNN
    DO JGL=2,JN-IODD,2
      ZFN(JN,JN-JGL)=ZFN(JN,JN-JGL+2)*FLOAT((JGL-1)*(2*JN-JGL+2))/FLOAT(JGL*(2*JN-JGL+1))
    ENDDO
  ENDDO

  IODD=MOD(KN,2)
  IK=IODD
  DO JGL=IODD,KN,2
    ZFNLAT(IK)=ZFN(KN,JGL)
    IK=IK+1
  ENDDO

  !*       1.2 Find first approximation of the roots of the
  !            Legendre polynomial of degree KN.
  ZPI=2.0*ASIN(1.0) ! Constant Pi
  INS2 = KN/2+MOD(KN,2)
  DO JGL=1,INS2
    Z = (4*JGL-1)*ZPI/(4*KN+2)
    PL(JGL) = Z+1.0/(TAN(Z)*8*(KN**2))
    ZREG(JGL) = COS(Z)
    ZLI (JGL) = COS(PL(JGL))
  ENDDO

  !     ------------------------------------------------------------------

  !*      2. Computes roots and weights for transformed theta
  !          ------------------------------------------------

  ZEPS = EPSILON(Z)
  ITEMAX = 20
  IODD=MOD(KN,2)

  DO JGL=INS2,1,-1

    ! * Initialization.

    ZX = PL(JGL)
    ZDLX = ZX
    IFLAG = 0

    ! * Newton iteration.

    DO JTER=1,ITEMAX+1

      ITER(JGL) = JTER

      ! - Newton iteration step.

      ZDLK = 0.0
      IF( IODD==0 ) ZDLK=0.5*ZFNLAT(0)
      ZZDLXN = 0.0
      ZDLLDN = 0.0
      IK=1

      IF(IFLAG == 0)THEN
        DO JN=2-IODD,KN,2
          ! normalised ordinary Legendre polynomial == \overbar{P_n}^0
          ZDLK = ZDLK + ZFNLAT(IK)*COS(JN*ZDLX)
          ! normalised derivative == d/d\theta(\overbar{P_n}^0)
          ZDLLDN = ZDLLDN - ZFNLAT(IK)*JN*SIN(JN*ZDLX)
          IK=IK+1
        ENDDO
        ! Newton method
        ZDLMOD = -ZDLK/ZDLLDN
        ZZDLXN = ZDLX+ZDLMOD
        ZXN = ZZDLXN
        ZDLXN = ZZDLXN
        ZMOD(JGL) = ZDLMOD
      ENDIF

      ! - Computes weight.

      IF(IFLAG == 1)THEN
        DO JN=2-IODD,KN,2
          ! normalised derivative
          ZDLLDN = ZDLLDN - ZFNLAT(IK)*JN*SIN(JN*ZDLX)
          IK=IK+1
        ENDDO
        ZW = (2*KN+1)/ZDLLDN**2
      ENDIF

      ZX = ZXN
      ZDLX = ZDLXN

      IF(IFLAG == 1) EXIT
      IF(ABS(ZMOD(JGL)) <= ZEPS*1000.) IFLAG = 1
    ENDDO

    PL(JGL) = ZXN
    PDL(JGL) = ZDLXN
    PW(JGL) = ZW

  ENDDO

  ! convert to physical latitude space PMU
  DO JGL=1,INS2
    PL(JGL) = COS(PL(JGL))
    PDL(JGL) = COS(PDL(JGL))
  ENDDO

  DO JGL=1,KN/2
    ISYM = KN-JGL+1
    PL (ISYM) = -PL (JGL)
    PDL(ISYM) = -PDL(JGL)
    PW (ISYM) =  PW (JGL)
  ENDDO

  !     ------------------------------------------------------------------

  !*      3. Diagnostics.
  !          ------------

    ZRA=6371229.         ! Earth radius
    DO JGL=1,INS2
      ZACOS = ACOS(PL(JGL))
      ZM(JGL) = (ZACOS-ACOS(ZLI (JGL)))*ZRA
      ZR(JGL) = (ZACOS-ACOS(ZREG(JGL)))*ZRA
      ZT(JGL) =  ZACOS*180./ZPI
    ENDDO

  IALLOW = 10
  DO JGL=1,INS2
    IF(ITER(JGL) > IALLOW)THEN
      WRITE(*,FMT='('' CONVERGENCE FAILED IN MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS '')')
      WRITE(*,FMT='('' ALLOWED : '',I4,''&
       &NECESSARY : '',&
       &I4)')IALLOW,ITER(JGL)
      CALL ABOR1_SFX(' FAILURE IN MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS ')
    ENDIF

      !WRITE(*,FMT=&
      ! &'('' ROW ='',I4,'' ITERATIONS='',I4,'' ROOT='',F30.20,&
      ! &'' WEIGHT='',F30.20,'' MODIF :'',E8.2)')JGL,ITER(JGL),PL(JGL)&
      ! &,PW(JGL),PL(JGL)-ZLI(JGL)
      !WRITE(*,FMT=&
      ! &'(10X,'' LAST INC. : '',E8.2,'' MODIF IN M : '',F10.3,&
      ! &'' FROM THE REGULAR GRID : '',F10.3,'' COLAT '',F10.3)')&
      ! &ZMOD(JGL),ZM(JGL),ZR(JGL),ZT(JGL)
  ENDDO

  !     ------------------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:LATITUDES_GAUSS',1,ZHOOK_HANDLE)
  END SUBROUTINE LATITUDES_GAUSS
  !##################################
  !############################################################################
  !############################################################################
  !##################################
  SUBROUTINE MESH_SIZE_GAUSS(KL,KNLATI,KNLOPA,PLAPO,PLOPO,PCODIL,&
                               PLAT_XY,PLAT,PLON,PMESH_SIZE)
  !##################################
  !
  !!****  *MESH_SIZE_GAUSS* - routine to compute the global mesh size
  !!
  !!    PURPOSE
  !!    -------
  !!
  !!**  METHOD
  !!    ------
  !!
  !!    AUTHOR
  !!    ------
  !!              *Meteo France*
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    06/2008
  !-------------------------------------------------------------------------------
  !
  USE MODD_CSTS, ONLY : XRADIUS, XPI
  !
  IMPLICIT NONE
  !
  INTEGER,                    INTENT(IN) :: KL        ! number of point
  INTEGER,                    INTENT(IN) :: KNLATI    ! number of pseudo-latitudes
  INTEGER, DIMENSION(KNLATI), INTENT(IN) :: KNLOPA ! number of pseudo-longitudes on each
  REAL,                       INTENT(IN) :: PLAPO     ! latitude  of the rotated pole (deg)
  REAL,                       INTENT(IN) :: PLOPO     ! longitude of the rotated pole (deg)
  REAL,                       INTENT(IN) :: PCODIL    ! stretching factor (must be greater than or equal to 1)
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLAT_XY  ! pseudo latitudes
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLAT       ! latitude  (degrees)
  REAL,    DIMENSION(KL),     INTENT(IN) :: PLON       ! longitude (degrees)
  REAL,    DIMENSION(KL),     INTENT(OUT):: PMESH_SIZE ! global mesh size
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: IXX      ! number of points in the latitude circle of each grid point
  INTEGER, DIMENSION(:), ALLOCATABLE :: IYY      ! latitude circle of each grid point
  REAL,    DIMENSION(:), ALLOCATABLE :: ZDLAT  !
  REAL,    DIMENSION(:), ALLOCATABLE :: PLAT_XY_C  !  pseudo-latitude for each circle
  REAL,    DIMENSION(:), ALLOCATABLE :: ZXX   ! X-length of each mesh in the gaussian grid
  REAL,    DIMENSION(:), ALLOCATABLE :: ZYY   ! Y-length of each mesh in the gaussian grid
  REAL,    DIMENSION(:), ALLOCATABLE :: ZMAP  ! map factor
  INTEGER :: IL
  INTEGER :: JL       ! loop counter on number of points
  INTEGER :: JLTOT    ! loop counter on total number of points
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MESH_SIZE_GAUSS',0,ZHOOK_HANDLE)
  ALLOCATE(IXX(KL))
  ALLOCATE(IYY(KL))
  ALLOCATE(ZXX(KL))
  ALLOCATE(PLAT_XY_C(KNLATI))
  ALLOCATE(ZDLAT(KNLATI))
  ALLOCATE(ZYY(KL))
  ALLOCATE(ZMAP(KL))
  !
  !        1.0   Length according to the X axis (longitudes)
  !              ------------------------------
  !
  IL=1
  DO JL = 1,KNLATI
    DO JLTOT = 1,KNLOPA(JL)
       IXX(IL)=KNLOPA(JL)
       IYY(IL)=JL
       IL=IL+1
    ENDDO
  ENDDO
  !
  !
  DO JL = 1,KL
     ZXX(JL) = 2.*XPI*XRADIUS*COS(PLAT_XY(JL)*XPI/180.)/FLOAT(IXX(JL))
  ENDDO
  !
  !        2.0   Length according to the Y axis (latitudes)
  !              ------------------------------
  !
  IL=1
  DO JL = 1,KNLATI
     PLAT_XY_C(JL)=PLAT_XY(IL)
     IL=IL+KNLOPA(JL)
  ENDDO
  !
  ZDLAT(1)=90.-PLAT_XY_C(1)+((PLAT_XY_C(1)-PLAT_XY_C(2))/2.)
  DO JL = 2,KNLATI-1
     ZDLAT(JL)=((PLAT_XY_C(JL-1)-PLAT_XY_C(JL))/2.)+((PLAT_XY_C(JL)-PLAT_XY_C(JL+1))/2.)
  ENDDO
  ZDLAT(KNLATI)=((PLAT_XY_C(KNLATI-1)-PLAT_XY_C(KNLATI))/2.)+PLAT_XY_C(KNLATI)+90.
  ZDLAT(:)=ZDLAT(:)*XPI*XRADIUS/180.
  !
  DO JL = 1,KL
     ZYY(JL)=ZDLAT(IYY(JL))
  ENDDO
  !
  !        3.0   grid mesh
  !              ---------
  !
  CALL MAP_FACTOR_GAUSS(PLAPO,PLOPO,PCODIL,PLAT,PLON,ZMAP)
  !
  PMESH_SIZE(:) = ZXX(:) * ZYY(:) * ZMAP(:)**2
  !
  DEALLOCATE(ZXX )
  DEALLOCATE(ZYY )
  DEALLOCATE(ZMAP)
  DEALLOCATE(PLAT_XY_C)
  DEALLOCATE(ZDLAT)
  DEALLOCATE(IXX  )
  DEALLOCATE(IYY  )
  IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_GAUSS:MESH_SIZE_GAUSS',1,ZHOOK_HANDLE)
  !
  !-------------------------------------------------------------------------------
  END SUBROUTINE MESH_SIZE_GAUSS

  !############################################################################
  !############################################################################
  !############################################################################

END MODULE MODE_GRIDTYPE_GAUSS

