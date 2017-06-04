!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLAT_ROT(KL,PGRID_PAR,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_LONLAT_ROT* get the grid mesh where point (lat,lon) is located
!!
!!    Note that this subroutine operates on the rotated grid. Thus incoming
!!    PLON, PLAT on regular grid is rotated.
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Samuelsson  SMHI
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/2012
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_LONLAT_ROT, ONLY : XLONLIM, XLATLIM, NLAT, NLON, XLON0, XPOLON, XPOLAT
USE MODE_GRIDTYPE_LONLAT_ROT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!RJ: missing modi
USE MODI_REGROT_LONLAT_ROT
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(:), INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLAT      ! latitude of the point
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLON      ! longitude of the point
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KINDEX    ! index of the grid mesh where the point is
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                           :: JI       ! loop counter in x
INTEGER                           :: JJ       ! loop counter in y
INTEGER                           :: JL       ! loop counter on input points
!
REAL    :: ZWEST   ! West longitude in rotated grid (degrees)
REAL    :: ZSOUTH  ! South latitude in rotated grid  (degrees)
REAL    :: ZDLON   ! Longitudal grid spacing  (degrees)
REAL    :: ZDLAT   ! Latitudal grid spacing  (degrees)
!
REAL, DIMENSION(SIZE(PLON)) :: ZLON
REAL, DIMENSION(SIZE(PLAT)) :: ZLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_ROT',0,ZHOOK_HANDLE)
IF (.NOT. ALLOCATED(XLATLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                                 ZWEST,ZSOUTH,ZDLON,ZDLAT,XPOLON,XPOLAT,  &
                                 NLON,NLAT                                )  
!
!----------------------------------------------------------------------------
!
!*    2.     Limits of grid meshes
!            ---------------------
!
  ALLOCATE(XLONLIM(NLON+1))
  DO JI=1,NLON+1
    XLONLIM(JI) = ZWEST + FLOAT(JI-1)*ZDLON
  END DO
  XLONLIM = XLONLIM - ZDLON/2.

  ALLOCATE(XLATLIM(NLAT+1))
  DO JI=1,NLAT+1
    XLATLIM(JI) = ZSOUTH + FLOAT(JI-1)*ZDLAT
  END DO
  XLATLIM = XLATLIM - ZDLAT/2.
!
  XLON0 = 0.5*(MINVAL(XLONLIM)+MAXVAL(XLONLIM))
!
END IF
!----------------------------------------------------------------------------
!
!*    3.     Reshifts the longitudes with respect to projection reference point
!            ------------------------------------------------------------------
!
!
 CALL REGROT_LONLAT_ROT(PLON,PLAT,ZLON,ZLAT,    &
                             KL,1,KL,1,        &
                             XPOLON,XPOLAT,1   )  
!
WHERE (ZLON(:)>180.) ZLON(:)=ZLON(:)-360.
WHERE (ZLON(:)<-180.) ZLON(:)=ZLON(:)+360.
!
ZLON(:) = ZLON(:)+NINT((XLON0-ZLON(:))/360.)*360.
!
!----------------------------------------------------------------------------
!
!*    4.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
IF (KL/=NLON*NLAT) THEN
  KINDEX = 0
  KISSOX = 0
  KISSOY = 0
END IF
!
!
DO JL=1,SIZE(ZLAT)
  IF (     ZLON(JL)<XLONLIM(1) .OR. ZLON(JL)>=XLONLIM(NLON+1) &
        .OR. ZLAT(JL)<XLATLIM(1) .OR. ZLAT(JL)>=XLATLIM(NLAT+1) ) THEN  
    KINDEX(JL) = 0
    IF (KSSO/=0) THEN
      KISSOX(JL) = 0
      KISSOY(JL) = 0
    END IF
    CYCLE
  END IF
  JI = COUNT (ZLON(JL)>=XLONLIM(:))
  JJ = COUNT (ZLAT(JL)>=XLATLIM(:))
  KINDEX(JL) = (JJ-1) * NLON + JI
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
  IF (KSSO/=0) THEN
    KISSOX(JL) = 1 + INT( FLOAT(KSSO) * (ZLON(JL)-XLONLIM(JI))/(XLONLIM(JI+1)-XLONLIM(JI)) )
    KISSOY(JL) = 1 + INT( FLOAT(KSSO) * (ZLAT(JL)-XLATLIM(JJ))/(XLATLIM(JJ+1)-XLATLIM(JJ)) )
  END IF
END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_ROT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLAT_ROT
