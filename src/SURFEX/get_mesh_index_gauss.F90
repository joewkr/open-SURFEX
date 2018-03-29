!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     ###############################################################
MODULE MODI_GET_MESH_INDEX_GAUSS
CONTAINS
      SUBROUTINE GET_MESH_INDEX_GAUSS(KNBLINES,KSSO,PGRID_PAR,PLAT,PLON,&
                                      KINDEX,KISSOX,KISSOY,PVALUE,PNODATA)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_GAUSS* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE EGGANGLES,ONLY : LOLA, ANGLE_DOMAIN
!
USE MODD_GET_MESH_INDEX_GAUSS, ONLY : XXCEN, XYCEN, XYINF, XYSUP, XXINF, XXSUP,       &
                                      NNLATI, NNLOPA, XLAPO, XLOPO, XCODIL, XSINTS,   &
                                      XLON, XLAT, XCOST, XSINTC, XCOSN, XSINN, XLONP, &
                                      XLATP, XCOSP, XSINP, XPI, X1, X2, XDR,          &
                                      NFRACDX, NFRACGX, NFRACDY, NFACTY, XXDIF, XYDIF,&
                                      LROTSTRETCH
!
USE MODE_GRIDTYPE_GAUSS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)   :: KNBLINES
INTEGER,                       INTENT(IN)   :: KSSO        ! number of subgrid mesh in each direction
REAL,    DIMENSION(:),         INTENT(IN)   :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(:),         INTENT(IN)   :: PLAT      ! latitude of the point  (degrees)
REAL,    DIMENSION(:),         INTENT(IN)   :: PLON      ! longitude of the point (degrees)
INTEGER, DIMENSION(:,:),       INTENT(OUT)  :: KINDEX    ! index of the grid mesh where the point is
INTEGER, DIMENSION(:,:),      INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(:,:),      INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
REAL, DIMENSION(:), OPTIONAL, INTENT(IN)    :: PVALUE  ! value of the point to add
REAL, OPTIONAL, INTENT(IN) :: PNODATA

!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(SIZE(PLAT))       :: ZX, ZY       ! pseudo longitude of input point
REAL, DIMENSION(SIZE(PLAT))       :: ZVALUE
REAL :: ZPC2
REAL :: ZNODATA
!
INTEGER, DIMENSION(SIZE(PLAT))    :: ICJ
INTEGER :: ILGRID, ISIZE_LON, ISIZE_DLAT, INBLINES  ! number of grid points
INTEGER :: IFACTX, ISIZEX, ISIZEY
INTEGER :: JI, JJ, JL       ! loop counter in x
INTEGER :: JGRID, IGRID0    ! loop counter on grid  points
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_1',0,ZHOOK_HANDLE)
!
IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
  ZVALUE(:) = PVALUE(:)
  ZNODATA = PNODATA
ELSE
  ZVALUE(:) = 1
  ZNODATA = 0
ENDIF
!
KINDEX(:,:) = 0
KISSOX(:,:) = 0
KISSOY(:,:) = 0
!
IF (.NOT. ALLOCATED(NNLOPA)) THEN
  !
  !*    1.     Gets parameters of the projection
  !            ---------------------------------
  !
  XPI = 4.*ATAN(1.)
  XDR = XPI / 180.
  !
  CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,NNLATI,KL=ILGRID)
  !
  ALLOCATE(NNLOPA(0:NNLATI))
  ALLOCATE(XXCEN(ILGRID))
  ALLOCATE(XYCEN(ILGRID))
  CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,NNLATI,XLAPO,XLOPO,XCODIL,NNLOPA(1:NNLATI), &
                          ILGRID,PLAT_XY=XYCEN,PLON_XY=XXCEN             )
  NNLOPA(0) = 0
  !
  ZPC2  = XCODIL*XCODIL
  X1 = 1.0-ZPC2
  X2 = 1.0+ZPC2
  !
  XLONP = ANGLE_DOMAIN(XLOPO,DOM='0+',UNIT='D') * XDR
  XLATP = XLAPO * XDR
  !
  XCOSP = COS(XLATP)
  XSINP = SIN(XLATP)
  !
  !*    2.     Limits of grid meshes in x and y
  !            --------------------------------
  !
  ALLOCATE(XXINF(ILGRID))
  ALLOCATE(XYINF(ILGRID))
  ALLOCATE(XXSUP(ILGRID))
  ALLOCATE(XYSUP(ILGRID))
  ALLOCATE(XXDIF(ILGRID))
  ALLOCATE(XYDIF(ILGRID))
  !
  CALL GAUSS_GRID_LIMITS(NNLATI,NNLOPA(1:NNLATI),XXINF,XXSUP,XYINF,XYSUP)
  DO JJ=1,ILGRID
    XXDIF(JJ) = 1. / (XXSUP(JJ) - XXINF(JJ))
    XYDIF(JJ) = 1. / (XYSUP(JJ) - XYINF(JJ))
  ENDDO
  !
  IFACTX = FLOOR(SQRT(FLOAT(NNLATI))) + 1
  ISIZEX = FLOOR(FLOAT(NNLATI) / IFACTX)
  !
  ALLOCATE(NFRACDX(0:IFACTX))
  ALLOCATE(NFRACGX(0:IFACTX))
  !
  NFRACDX(0) = 0
  NFRACGX(0) = 1
  NFRACDX(1) = 1
  NFRACGX(1) = 1
  NFRACDX(IFACTX) = NNLATI
  NFRACGX(IFACTX) = SUM(NNLOPA(:))
  DO JJ=2,IFACTX-1
    NFRACDX(JJ) = 1 + (JJ-1) * ISIZEX
    NFRACGX(JJ) = NFRACGX(JJ-1) + SUM(NNLOPA(NFRACDX(JJ-1):(JJ-1)*ISIZEX))
  ENDDO
  !
  !
  ALLOCATE(NFACTY(NNLATI))
  NFACTY(:) = FLOOR(SQRT(FLOAT(NNLOPA(1:NNLATI))))+1
  !
  ALLOCATE(NFRACDY(NNLATI,0:MAXVAL(NFACTY)))
  !
  DO JJ=1,NNLATI
    ISIZEY = FLOOR(FLOAT(NNLOPA(JJ)) / NFACTY(JJ))
    NFRACDY(JJ,0) = 0
    NFRACDY(JJ,1) = 1
    NFRACDY(JJ,NFACTY(JJ)) = NNLOPA(JJ)
    DO JI=2,NFACTY(JJ)-1
      NFRACDY(JJ,JI) = 1 + (JI-1) * ISIZEY
    ENDDO
  ENDDO
  !
  !*    3.     Find if rotated pole and/or stretching to improve CPU time
  !            ----------------------------------------------------------
  !
  LROTSTRETCH = .TRUE.
  IF (XCODIL==1.0.AND.XLAPO==90.0.AND.XLOPO==0.0) LROTSTRETCH = .FALSE.
  !
ENDIF
!
! case where the grid is not regular: all points are considered independently
IF (KNBLINES==0) THEN
  INBLINES = SIZE(PLAT)
ELSE
  INBLINES = KNBLINES
ENDIF
!
ISIZE_DLAT = SIZE(PLAT)/INBLINES
!
! case where the grid is not regular: all points are considered independently
IF (KNBLINES==0) THEN
  ISIZE_LON = INBLINES
ELSE
  ISIZE_LON = ISIZE_DLAT
ENDIF
!
IF (ALLOCATED(XLON)) THEN
  IF ( ISIZE_LON/=SIZE(XLON) .OR. INBLINES/=SIZE(XLAT) ) THEN
    DEALLOCATE(XLON)
    DEALLOCATE(XLAT)
    DEALLOCATE(XCOST)
    DEALLOCATE(XSINTC)
    DEALLOCATE(XSINTS)
    DEALLOCATE(XCOSN)
    DEALLOCATE(XSINN)
  ENDIF
ENDIF
!
IF (.NOT.ALLOCATED(XLON)) THEN
  !
  ALLOCATE(XLON(ISIZE_LON))
  ALLOCATE(XLAT(INBLINES))
  !
  ALLOCATE(XCOST (SIZE(XLAT)))
  ALLOCATE(XSINTC(SIZE(XLAT)))
  ALLOCATE(XSINTS(SIZE(XLAT)))
  ALLOCATE(XCOSN (SIZE(XLON)))
  ALLOCATE(XSINN (SIZE(XLON)))
  !
  XLON(:) = ANGLE_DOMAIN(PLON(1:ISIZE_LON),DOM='0+',UNIT='D') * XDR
  XCOSN(:) = COS(XLON(:)-XLONP)
  XSINN(:) = SIN(XLON(:)-XLONP)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_2',0,ZHOOK_HANDLE)

!
!*    3.     Projection of input points into pseudo coordinates
!            --------------------------------------------------
!
DO JJ=1,INBLINES
  XLAT  (JJ) = PLAT(JJ*ISIZE_DLAT) * XDR
  XSINTC(JJ) = SIN(XLAT(JJ)) * XCOSP
  XSINTS(JJ) = SIN(XLAT(JJ)) * XSINP
  XCOST (JJ) = COS(XLAT(JJ))
ENDDO
!
IF (LROTSTRETCH) THEN
  CALL XY_GAUSS(XCODIL,ISIZE_DLAT,ISIZE_LON,ZNODATA,ZVALUE,ZY,ZX)
ELSE
  ZX(:) = PLON(:)
  ZY(:) = PLAT(:)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    5.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
ICJ(:) = 0
!
IFACTX = SIZE(NFRACDX)
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_2',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JJ,JI,JGRID)
DO JL=1,SIZE(PLAT)
  !
 IF (ZVALUE(JL)==ZNODATA) CYCLE
  !
  fracx: &
  DO JJ=1,IFACTX
    !
    IF (ZY(JL)>=XYINF(NFRACGX(JJ))) THEN
      !
      JGRID = NFRACGX(JJ-1)
      !
      DO JI=NFRACDX(JJ-1)+1,NFRACDX(JJ)-1
        !
        JGRID = JGRID + NNLOPA(JI-1)
        !
        IF (ZY(JL)>=XYINF(JGRID)) THEN
          !
          ICJ(JL) = JI
          EXIT fracx
          !
        ENDIF
        !
      ENDDO
      !
      ICJ(JL) = NFRACDX(JJ)
      !
      exit fracx
      !
    ENDIF
    !
  ENDDO fracx
  !
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_3',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_4',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(IGRID0,JGRID,JI,JJ)
DO JL=1,SIZE(PLAT)
  !
  IF (ZVALUE(JL)==ZNODATA) CYCLE
  !
  IGRID0 = 0
  IF (ICJ(JL)/=1) IGRID0 = SUM(NNLOPA(1:ICJ(JL)-1))
  !
  !* loop on grid points: longitude
  fracy: &
  DO JJ = 1,NFACTY(ICJ(JL))
    !
    IF (ZX(JL)<XXSUP(IGRID0+NFRACDY(ICJ(JL),JJ))) THEN
      !
      JGRID = IGRID0 + NFRACDY(ICJ(JL),JJ-1)
      !
      DO JI=NFRACDY(ICJ(JL),JJ-1)+1,NFRACDY(ICJ(JL),JJ)
        !
        JGRID = JGRID + 1
        IF (ZX(JL)<=XXCEN(JGRID)-180. .AND. ZX(JL)<XXSUP(JGRID)-360.) ZX(JL) = ZX(JL) + 360.
    !* imput point is in this grid mesh
        IF (ZX(JL)>=XXINF(JGRID) .AND. ZX(JL)<XXSUP(JGRID)) THEN
          !
          KINDEX(1,JL) = JGRID
          !
          EXIT fracy
          !
        ENDIF
        !
      ENDDO
      !
    END IF
    !
  END DO fracy
  !
END DO
!£$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_4',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
IF (KSSO/=0) THEN
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_5',0,ZHOOK_HANDLE_OMP)
!$OMP DO
  DO JL=1,SIZE(PLAT)
    IF (KINDEX(1,JL)/=0) THEN
      KISSOX(1,JL) = 1 + INT( FLOAT(KSSO) * (ZX(JL)-XXINF(KINDEX(1,JL)))/(XXSUP(KINDEX(1,JL))-XXINF(KINDEX(1,JL))) )
      KISSOY(1,JL) = 1 + INT( FLOAT(KSSO) * (ZY(JL)-XYINF(KINDEX(1,JL)))/(XYSUP(KINDEX(1,JL))-XYINF(KINDEX(1,JL))) )
    ENDIF
  ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS_5',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_GAUSS
END MODULE MODI_GET_MESH_INDEX_GAUSS
