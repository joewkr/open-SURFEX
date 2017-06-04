!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################
      SUBROUTINE LATLONMASK_CONF_PROJ(KGRID_PAR,PGRID_PAR,OLATLONMASK)
!     ##################################
!
!!**** *LATLONMASK* builds the latiude and longitude mask including the grid
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    Two tests are performed:
!!
!!   1) test if the points of the mask are in the domain
!!
!!   2) fills the mask points corresponding to points scanning
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!      
!!      Original        19/07/95
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_CONF_PROJ
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)  :: KGRID_PAR   ! size of PGRID_PAR
REAL,    DIMENSION(KGRID_PAR), INTENT(IN)  :: PGRID_PAR   ! parameters defining this grid
LOGICAL, DIMENSION(720,360),   INTENT(OUT) :: OLATLONMASK ! mask where data are to be read
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: IIMAX    ! x coordinates array dimension
INTEGER                           :: IJMAX    ! y coordinates array dimension
INTEGER                           :: JI, JJ   ! loop counters
INTEGER                           :: JLON,JLAT! loop counters
REAL                              :: ZXMIN    ! minimum of X for domain
REAL                              :: ZXMAX    ! maximum of X for domain
REAL                              :: ZYMIN    ! minimum of Y for domain
REAL                              :: ZYMAX    ! maximum of Y for domain
REAL, DIMENSION(:),   ALLOCATABLE :: ZDX      ! X grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZDY      ! Y grid mesh size
REAL, DIMENSION(:),   ALLOCATABLE :: ZX       ! X conformal coordinate of center of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZY       ! Y conformal coordinate of center of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZXCORNER ! X conformal coordinate of corner of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZYCORNER ! Y conformal coordinate of corner of grid mesh
REAL, DIMENSION(:),   ALLOCATABLE :: ZLON     ! corner points longitudes
REAL, DIMENSION(:),   ALLOCATABLE :: ZLAT     ! corner points latitudes
REAL, DIMENSION(:,:), ALLOCATABLE :: ZLON2D   ! corner points longitudes
REAL, DIMENSION(:,:), ALLOCATABLE :: ZLAT2D   ! corner points latitudes
REAL, DIMENSION(720,360)          :: ZX_MASK  ! mask points X value
REAL, DIMENSION(720,360)          :: ZY_MASK  ! mask points Y value
INTEGER, DIMENSION(720,360)       :: ICOUNT1  ! counter
INTEGER, DIMENSION(720,360)       :: ICOUNT2  ! counter
REAL, DIMENSION(720)              :: ZLON_MASK! mask points longitudes
REAL, DIMENSION(720)              :: ZLAT_MASK! mask points latitudes
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL                              :: ZRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL                              :: ZBETA    ! angle between grid and reference longitude
REAL                              :: ZLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL                              :: ZLONOR   ! longitude of point of coordinates X=0, Y=0
!
INTEGER                           :: IVERB=1  ! verbosity level
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLONMASK_CONF_PROJ',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,KIMAX=IIMAX,KJMAX=IJMAX)
!
!
ALLOCATE(ZX  (IIMAX*IJMAX))
ALLOCATE(ZY  (IIMAX*IJMAX))
ALLOCATE(ZDX (IIMAX*IJMAX))
ALLOCATE(ZDY (IIMAX*IJMAX))
ALLOCATE(ZXCORNER  ((IIMAX+1)*(IJMAX+1)))
ALLOCATE(ZYCORNER  ((IIMAX+1)*(IJMAX+1)))
ALLOCATE(ZLON      ((IIMAX+1)*(IJMAX+1)))
ALLOCATE(ZLAT      ((IIMAX+1)*(IJMAX+1)))
!
!-------------------------------------------------------------------------------
!
OLATLONMASK(:,:) = .FALSE.
!
!-------------------------------------------------------------------------------
!
!*      1.   Limits of the domain conformal plane coordinates
!            ------------------------------------------------
!
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY)
!
ZXMIN =      MINVAL(ZX) - 3.* MAXVAL(ZDX)/2.
ZXMAX =      MAXVAL(ZX) + 3.* MAXVAL(ZDX)/2.
!
ZYMIN =      MINVAL(ZY) - 3.* MAXVAL(ZDY)/2.
ZYMAX =      MAXVAL(ZY) + 3.* MAXVAL(ZDY)/2.
!
DO JJ=1,IJMAX
  DO JI=1,IIMAX
    ZXCORNER(JI+(JJ-1)*(IIMAX+1)) = ZX(JI+(JJ-1)*IIMAX) - 0.5*ZDX(JI+(JJ-1)*IIMAX)
    ZYCORNER(JI+(JJ-1)*(IIMAX+1)) = ZY(JI+(JJ-1)*IIMAX) - 0.5*ZDY(JI+(JJ-1)*IIMAX)
  END DO
  ZXCORNER(IIMAX+1+(JJ-1)*(IIMAX+1)) = ZX(IIMAX+(JJ-1)*IIMAX) + 0.5*ZDX(IIMAX+(JJ-1)*IIMAX)
  ZYCORNER(IIMAX+1+(JJ-1)*(IIMAX+1)) = ZY(IIMAX+(JJ-1)*IIMAX) - 0.5*ZDX(IIMAX+(JJ-1)*IIMAX)
END DO
DO JI=1,IIMAX
  ZXCORNER(JI+IJMAX*(IIMAX+1)) = ZX(JI+(IJMAX-1)*IIMAX) - 0.5*ZDX(JI+(IJMAX-1)*IIMAX)
  ZYCORNER(JI+IJMAX*(IIMAX+1)) = ZY(JI+(IJMAX-1)*IIMAX) + 0.5*ZDY(JI+(IJMAX-1)*IIMAX)
END DO
ZXCORNER((IJMAX+1)*(IIMAX+1)) = ZX(IJMAX*IIMAX) + 0.5*ZDX(IJMAX*IIMAX)
ZYCORNER((IJMAX+1)*(IIMAX+1)) = ZY(IJMAX*IIMAX) + 0.5*ZDY(IJMAX*IIMAX)
!
DEALLOCATE(ZDX)
DEALLOCATE(ZDY)
DEALLOCATE(ZX)
DEALLOCATE(ZY)
!
!-------------------------------------------------------------------------------
!
!*      2.   Definition of the coordinates at center of the mask meshes
!            ----------------------------------------------------------
!
ZLON_MASK(:)= (/ (  JLON     /2. - 0.25 , JLON=1,720 ) /)
!
!*      3.   Longitude correction / LON0
!            ---------------------------
!
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,                   &
                              PLAT0=ZLAT0,PLON0=ZLON0,PRPK=ZRPK,        &
                              PBETA=ZBETA,PLATOR=ZLATOR,PLONOR=ZLONOR   )  
!
ZLON_MASK(:)=ZLON_MASK(:)+NINT((ZLON0-ZLON_MASK(:))/360.)*360.
!
!*      4.   X and Y of the points of the mask
!            ---------------------------------
!
DO JLAT=1,SIZE(OLATLONMASK,2)
  ZLAT_MASK(:) = (JLAT-180)/2. - 0.25
  CALL XY_CONF_PROJ(ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR,ZLONOR, &
                      ZX_MASK(:,JLAT),ZY_MASK(:,JLAT),    &
                      ZLAT_MASK(:),ZLON_MASK(:)   )
END DO
!
!*      5.   Are the points in the domain?
!            ----------------------------
!
WHERE (        ZX_MASK(:,:) >= ZXMIN .AND. ZX_MASK(:,:) <= ZXMAX   &
           .AND. ZY_MASK(:,:) >= ZYMIN .AND. ZY_MASK(:,:) <= ZYMAX )  
  OLATLONMASK(:,:) = .TRUE.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*      6.   Latitude and longitude of the points of the domain
!            --------------------------------------------------
!
 CALL LATLON_CONF_PROJ(ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR,ZLONOR, &
                      ZXCORNER,ZYCORNER,ZLAT,ZLON           )  
!
!*      7.   Longitudes between 0. and 360.
!            ------------------------------
!
ZLON(:) = ZLON(:) + NINT((180.-ZLON(:))/360.)*360.
!
!*      8.   Loop on grid points
!            --------------------
!
ICOUNT1(:,:) = 0
ICOUNT2(:,:) = 0
!
ALLOCATE(ZLAT2D(IIMAX+1,IJMAX+1))
ALLOCATE(ZLON2D(IIMAX+1,IJMAX+1))
!
DO JJ=1,IJMAX+1
  DO JI=1,IIMAX+1
    ZLAT2D(JI,JJ) = ZLAT(JI+(JJ-1)*(IIMAX+1))
    ZLON2D(JI,JJ) = ZLON(JI+(JJ-1)*(IIMAX+1))
  END DO
END DO
!
DO JJ=1,IJMAX+1
  DO JI=1,IIMAX+1
    !
    !*      8.1  localisation of the point
    !            -------------------------
    !
    JLAT = MIN( 1 + INT( ( ZLAT2D(JI,JJ) + 90. ) * 2. ) ,360)
    JLON = MIN( 1 + INT( ( ZLON2D(JI,JJ)       ) * 2. ) ,720)
    !
    ICOUNT1(JLON,JLAT) = ICOUNT1(JLON,JLAT) + 1
    !
    !*      8.2  Does point contain data?
    !            ------------------------
    !
    !
    !*      8.3  point contains data
    !            -------------------
    !
    ICOUNT2(JLON,JLAT) = ICOUNT2(JLON,JLAT) + 1
    !
    !*      8.4  Boundary effects
    !            ----------------
    !
    JLAT = MIN( 1 + INT( ( ZLAT2D(MIN(JI+1,IIMAX),JJ) + 90. ) * 2. ) ,360)
    JLON = MIN( 1 + INT( ( ZLON2D(MIN(JI+1,IIMAX),JJ)       ) * 2. ) ,720)
    ICOUNT1(JLON,JLAT) = ICOUNT1(JLON,JLAT) + 1
    ICOUNT2(JLON,JLAT) = ICOUNT2(JLON,JLAT) + 1
    !
    JLAT = MIN( 1 + INT( ( ZLAT2D(JI,MIN(JJ+1,IJMAX)) + 90. ) * 2. ) ,360)
    JLON = MIN( 1 + INT( ( ZLON2D(JI,MIN(JJ+1,IJMAX))       ) * 2. ) ,720)
    ICOUNT1(JLON,JLAT) = ICOUNT1(JLON,JLAT) + 1
    ICOUNT2(JLON,JLAT) = ICOUNT2(JLON,JLAT) + 1
    !
    JLAT = MIN( 1 + INT( ( ZLAT2D(MAX(JI-1,1),JJ) + 90. ) * 2. ) ,360)
    JLON = MIN( 1 + INT( ( ZLON2D(MAX(JI-1,1),JJ)       ) * 2. ) ,720)
    ICOUNT1(JLON,JLAT) = ICOUNT1(JLON,JLAT) + 1
    ICOUNT2(JLON,JLAT) = ICOUNT2(JLON,JLAT) + 1
    !
    JLAT = MIN( 1 + INT( ( ZLAT2D(JI,MAX(JJ-1,1)) + 90. ) * 2. ) ,360)
    JLON = MIN( 1 + INT( ( ZLON2D(JI,MAX(JJ-1,1))       ) * 2. ) ,720)
    ICOUNT1(JLON,JLAT) = ICOUNT1(JLON,JLAT) + 1
    ICOUNT2(JLON,JLAT) = ICOUNT2(JLON,JLAT) + 1
    !
  END DO
END DO
!
!*      9.   Surface type check (if points are present in mask mesh)
!            ------------------
!
WHERE (ICOUNT1(:,:) > 0 .AND. ICOUNT2(:,:) == 0)
  OLATLONMASK(:,:) = .FALSE.
END WHERE
!
WHERE (ICOUNT1(:,:) > 0 .AND. ICOUNT2(:,:) > 0)
  OLATLONMASK(:,:) = .TRUE.
END WHERE
!
ZLAT_MASK(1:360)= (/ ( (JLAT-180)/2. - 0.25 , JLAT=1,360 ) /) 
!
DO JLON=1,720
  DO JLAT=1,360
    IF ( (ICOUNT1(JLON,JLAT) > 0 .OR. OLATLONMASK(JLON,JLAT)) .AND.  IVERB > 1) &
      WRITE(*,'(2(I3,1X),2(F6.2,1X),2(F8.0,1X),L1)') JLON,JLAT,ZLON_MASK(JLON),ZLAT_MASK(JLAT), &
                ZX_MASK(JLON,JLAT),ZY_MASK(JLON,JLAT),OLATLONMASK(JLON,JLAT)  

  END DO
END DO
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLON  )
DEALLOCATE(ZLAT  )
DEALLOCATE(ZLON2D)
DEALLOCATE(ZLAT2D)
DEALLOCATE(ZXCORNER)
DEALLOCATE(ZYCORNER)
IF (LHOOK) CALL DR_HOOK('LATLONMASK_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LATLONMASK_CONF_PROJ
