!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################
      SUBROUTINE LATLONMASK_CARTESIAN(KGRID_PAR,PGRID_PAR,OLATLONMASK)
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
USE MODE_GRIDTYPE_CARTESIAN
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
INTEGER                           :: JLON,JLAT! loop counters
REAL, DIMENSION(720,360)          :: ZLON_MASK! mask points longitudes
REAL, DIMENSION(720,360)          :: ZLAT_MASK! mask points latitudes
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('LATLONMASK_CARTESIAN',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0)
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
!*      3.   Longitude correction / LON0
!            ---------------------------
!
ZLON_MASK(:,:)=ZLON_MASK(:,:)+NINT((ZLON0-ZLON_MASK(:,:))/360.)*360.
!
!
WHERE (        ZLON_MASK(:,:) -0.25 <= ZLON0 .AND. ZLON0 <= ZLON_MASK(:,:) +0.25 &
           .AND. ZLAT_MASK(:,:) -0.25 <= ZLON0 .AND. ZLAT0 <= ZLAT_MASK(:,:) +0.25 )  
  OLATLONMASK(:,:) = .TRUE.
END WHERE
IF (LHOOK) CALL DR_HOOK('LATLONMASK_CARTESIAN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LATLONMASK_CARTESIAN
