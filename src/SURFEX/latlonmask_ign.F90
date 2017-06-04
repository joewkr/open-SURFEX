!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################
      SUBROUTINE LATLONMASK_IGN(KGRID_PAR,PGRID_PAR,OLATLONMASK)
!     ##################################
!
!!**** *LATLONMASK* builds the latitude and longitude mask including the grid
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    A simple method is used :
!!
!!   XMIN, XMAX, YMIN, YMAX are calculated for the grid 
!!   This domain is extended to account for deformation between lambert and lat lon.
!!   All lat lon values in this extended domains are set to true in the mask.
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
!!      E. Martin       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!      
!!      Original        10/2007  
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_IGN
USE MODD_IGN, ONLY : XEXPAND
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
REAL                              :: ZXMIN    ! minimum of X for domain
REAL                              :: ZXMAX    ! maximum of X for domain
REAL                              :: ZYMIN    ! minimum of Y for domain
REAL                              :: ZYMAX    ! maximum of Y for domain
REAL, DIMENSION(720,360)          :: ZX_MASK  ! mask points X value
REAL, DIMENSION(720,360)          :: ZY_MASK  ! mask points Y value
REAL, DIMENSION(720,360)          :: ZLON_MASK! mask points longitudes
REAL, DIMENSION(720,360)          :: ZLAT_MASK! mask points latitudes
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X Lambert   coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y Lambert   coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX      ! Grid dimension in X 
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY      ! Grid dimension in Y 
!
INTEGER                           :: ILAMBERT ! Lambert projection type
INTEGER                           :: IL       ! Number og grid points
INTEGER                           :: JLAT, JLON
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLONMASK_IGN',0,ZHOOK_HANDLE)
OLATLONMASK(:,:) = .FALSE.
!
!-------------------------------------------------------------------------------
!
!*      1.   Limits of the domain in Lambert IGN coordinates 
!            (including expansion factor)
!            ------------------------------------------------
!
     CALL GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT=ILAMBERT,KL=IL)
!
     ALLOCATE(ZX (IL))
     ALLOCATE(ZY (IL))
     ALLOCATE(ZDX(IL))
     ALLOCATE(ZDY(IL))
!
     CALL GET_GRIDTYPE_IGN(PGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY)
!
!*    2.     Limits of grid meshes in x and y
!            --------------------------------
!
     ZXMIN = MINVAL(ZX(:)-ZDX(:)/2.) - XEXPAND
     ZXMAX = MAXVAL(ZX(:)+ZDX(:)/2.) + XEXPAND
     ZYMIN = MINVAL(ZY(:)-ZDY(:)/2.) - XEXPAND
     ZYMAX = MAXVAL(ZY(:)+ZDY(:)/2.) + XEXPAND
     DEALLOCATE(ZX )
     DEALLOCATE(ZY )
     DEALLOCATE(ZDX)
     DEALLOCATE(ZDY)
!
!-------------------------------------------------------------------------------
!
!*      2.   Definition of the coordinates at center of the mask meshes
!            ----------------------------------------------------------
!
!
ZLON_MASK(:,:)= SPREAD( (/ (  JLON     /2. - 0.25 , JLON=1,720 ) /) , DIM=2, NCOPIES=360 )
ZLAT_MASK(:,:)= SPREAD( (/ ( (JLAT-180)/2. - 0.25 , JLAT=1,360 ) /) , DIM=1, NCOPIES=720 )
!
!*      3.   Longitude correction (-180 /+180 )
!            --------------------------------
!
ZLON_MASK(:,:)=ZLON_MASK(:,:)+NINT((-ZLON_MASK(:,:))/360.)*360.
!
!
!*      4.   X and Y of the points of the mask
!            ---------------------------------
!
 CALL GET_GRIDTYPE_IGN (PGRID_PAR, KLAMBERT=ILAMBERT)
DO JLAT=1,SIZE(ZLAT_MASK,2)
  CALL XY_IGN(ILAMBERT,ZX_MASK(:,JLAT),ZY_MASK(:,JLAT),          &
                ZLAT_MASK(:,JLAT),ZLON_MASK(:,JLAT)   )  

END DO
!
!*      5.   Are the points in the domain?
!            ----------------------------
!
WHERE (        ZX_MASK(:,:) >= ZXMIN .AND. ZX_MASK(:,:) <= ZXMAX   &
           .AND. ZY_MASK(:,:) >= ZYMIN .AND. ZY_MASK(:,:) <= ZYMAX )  
  OLATLONMASK(:,:) = .TRUE.
END WHERE
IF (LHOOK) CALL DR_HOOK('LATLONMASK_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE LATLONMASK_IGN
