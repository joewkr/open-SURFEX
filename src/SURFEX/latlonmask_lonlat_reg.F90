!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################
      SUBROUTINE LATLONMASK_LONLAT_REG(KGRID_PAR,PGRID_PAR,OLATLONMASK)
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
USE MODE_GRIDTYPE_LONLAT_REG
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
REAL    :: ZLONMIN
REAL    :: ZLONMAX
REAL    :: ZLATMIN
REAL    :: ZLATMAX
REAL    :: ZLON0
!
INTEGER :: JLAT
INTEGER :: JLON
!
REAL, DIMENSION(720,360) :: ZLON_MASK! mask points longitudes
REAL, DIMENSION(720,360) :: ZLAT_MASK! mask points latitudes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLONMASK_LONLAT_REG',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX, &
                               ZLATMIN,ZLATMAX            )  
!
!-------------------------------------------------------------------------------
!
OLATLONMASK(:,:) = .FALSE.
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
!-------------------------------------------------------------------------------
!
!*      3.   Set definition of longitudes according to grid 
!            ----------------------------------------------
!
ZLON0 = 0.5*(ZLONMIN+ZLONMAX)
ZLON_MASK(:,:)=ZLON_MASK(:,:)+NINT((ZLON0-ZLON_MASK(:,:))/360.)*360.
!
!-------------------------------------------------------------------------------
!
DO JLAT=1,360
  DO JLON=1,720
    IF (      ZLON_MASK(JLON,JLAT) + 0.25 >= ZLONMIN &
          .AND. ZLON_MASK(JLON,JLAT) - 0.25 <= ZLONMAX &
          .AND. ZLAT_MASK(JLON,JLAT) + 0.25 >= ZLATMIN &
          .AND. ZLAT_MASK(JLON,JLAT) - 0.25 <= ZLATMAX ) OLATLONMASK(JLON,JLAT) = .TRUE.  
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('LATLONMASK_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LATLONMASK_LONLAT_REG
