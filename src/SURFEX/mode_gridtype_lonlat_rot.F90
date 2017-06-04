!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_LONLAT_ROT
!     ##############################
!
!############################################################################
!############################################################################
!############################################################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                                           PWEST,PSOUTH,PDLON,PDLAT,PPOLON,PPOLAT,  &
                                           KLON,KLAT,KL,PLON,PLAT                   )  
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_LONLAT_ROT* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      P. Samuelsson   SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2012 
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
REAL,               INTENT(IN)  :: PWEST   ! West longitude in rotated grid (degrees)
REAL,               INTENT(IN)  :: PSOUTH  ! South latitude in rotated grid  (degrees)
REAL,               INTENT(IN)  :: PDLON   ! Longitudal grid spacing  (degrees)
REAL,               INTENT(IN)  :: PDLAT   ! Latitudal grid spacing  (degrees)
REAL,               INTENT(IN)  :: PPOLON  ! Longitude of rotated pole (degrees)
REAL,               INTENT(IN)  :: PPOLAT  ! Latitude of rotated pole  (degrees)
INTEGER,            INTENT(IN)  :: KLON     ! number of points in longitude
INTEGER,            INTENT(IN)  :: KLAT     ! number of points in latitude
INTEGER,            INTENT(IN)  :: KL       ! number of points used
REAL, DIMENSION(:), INTENT(IN)  :: PLON     ! regular longitudes of all points 
REAL, DIMENSION(:), INTENT(IN)  :: PLAT     ! regular latitudes  of all points
REAL,   DIMENSION(:),POINTER    :: PGRID_PAR! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:PUT_GRIDTYPE_LONLAT_ROT',0,ZHOOK_HANDLE)
ALLOCATE(PGRID_PAR(9+2*KL))
PGRID_PAR(1) = PWEST
PGRID_PAR(2) = PSOUTH
PGRID_PAR(3) = PDLON
PGRID_PAR(4) = PDLAT
PGRID_PAR(5) = PPOLON
PGRID_PAR(6) = PPOLAT
PGRID_PAR(7) = FLOAT(KLON)
PGRID_PAR(8) = FLOAT(KLAT)
PGRID_PAR(9) = FLOAT(KL)
PGRID_PAR(10:9+KL)      = PLON(:)
PGRID_PAR(10+KL:9+2*KL) = PLAT(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:PUT_GRIDTYPE_LONLAT_ROT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_LONLAT_ROT
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_LONLAT_ROT(PGRID_PAR,                                 &
                                           PWEST,PSOUTH,PDLON,PDLAT,PPOLON,PPOLAT,  &
                                           KLON,KLAT,KL,PLON,PLAT                   )  
!     ####################################################################
!
!!****  *GET_GRIDTYPE_LONLAT_ROT* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      P. Samuelsson   SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2012 
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
REAL, DIMENSION(:), INTENT(IN)            :: PGRID_PAR! parameters defining this grid
REAL,               INTENT(OUT), OPTIONAL :: PWEST    ! West longitude in rotated grid (degrees)
REAL,               INTENT(OUT), OPTIONAL :: PSOUTH   ! South latitude in rotated grid  (degrees)
REAL,               INTENT(OUT), OPTIONAL :: PDLON    ! Longitudal grid spacing  (degrees)
REAL,               INTENT(OUT), OPTIONAL :: PDLAT    ! Latitudal grid spacing  (degrees)
REAL,               INTENT(OUT), OPTIONAL :: PPOLON   ! Longitude of rotated pole (degrees)
REAL,               INTENT(OUT), OPTIONAL :: PPOLAT   ! Latitude of rotated pole  (degrees)
INTEGER,            INTENT(OUT), OPTIONAL :: KLON     ! number of points in longitude
INTEGER,            INTENT(OUT), OPTIONAL :: KLAT     ! number of points in latitude
INTEGER,            INTENT(OUT), OPTIONAL :: KL       ! number of points used
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLON     ! longitudes of all points
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLAT     ! latitudes  of all points
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILON, ILAT
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:GET_GRIDTYPE_LONLAT_ROT',0,ZHOOK_HANDLE)
ILON = NINT(PGRID_PAR(7))
ILAT = NINT(PGRID_PAR(8))
IL   = NINT(PGRID_PAR(9))
!
IF (PRESENT(PWEST   )) PWEST   = PGRID_PAR(1)
IF (PRESENT(PSOUTH  )) PSOUTH  = PGRID_PAR(2)
IF (PRESENT(PDLON   )) PDLON   = PGRID_PAR(3)
IF (PRESENT(PDLAT   )) PDLAT   = PGRID_PAR(4)
IF (PRESENT(PPOLON  )) PPOLON  = PGRID_PAR(5)
IF (PRESENT(PPOLAT  )) PPOLAT  = PGRID_PAR(6)
IF (PRESENT(KLON    )) KLON    = ILON
IF (PRESENT(KLAT    )) KLAT    = ILAT
IF (PRESENT(KL      )) KL      = IL
IF (PRESENT(PLON    )) PLON(:) = PGRID_PAR(10:9+IL)
IF (PRESENT(PLAT    )) PLAT(:) = PGRID_PAR(10+IL:9+2*IL)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:GET_GRIDTYPE_LONLAT_ROT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_LONLAT_ROT
!---------------------------------------------------------------------------------
!
!      ###################################################
       SUBROUTINE LATLON_LONLAT_ROT(PWEST,PSOUTH,PDLON,PDLAT,PPOLON,PPOLAT, &
                                      KLON,KLAT,PLON,PLAT                   )  
!      ###################################################
!
!!****  *LATLON_LONLAT_ROT * - Routine to compute regular geographical coordinates
!!
!!     PURPOSE
!!     -------
!!       
!!     AUTHOR
!!     ------
!!      P. Samuelsson   SMHI
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2012 
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
!RJ: missing modi
USE MODI_REGROT_LONLAT_ROT
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,               INTENT(IN)  :: PWEST    ! West longitude in rotated grid (degrees)
REAL,               INTENT(IN)  :: PSOUTH   ! South latitude in rotated grid  (degrees)
REAL,               INTENT(IN)  :: PDLON    ! Longitudal grid spacing  (degrees)
REAL,               INTENT(IN)  :: PDLAT    ! Latitudal grid spacing  (degrees)
REAL,               INTENT(IN)  :: PPOLON   ! Longitude of rotated south pole (degrees)
REAL,               INTENT(IN)  :: PPOLAT   ! Latitude of rotated south pole  (degrees)
INTEGER,            INTENT(IN)  :: KLON     ! number of points in longitude
INTEGER,            INTENT(IN)  :: KLAT     ! number of points in latitude
REAL, DIMENSION(:), INTENT(OUT) :: PLON,PLAT    
                                            ! returned geographic latitudes and 
                                            ! longitudes of the processed points 
                                            ! (degrees).
!
!*     0.2    Declarations of local variables
! 
INTEGER :: JLON, JLAT
INTEGER :: JL, KL
INTEGER :: JC

REAL, DIMENSION(:), ALLOCATABLE :: ZLATROT ! rotated latitude  of all points
REAL, DIMENSION(:), ALLOCATABLE :: ZLONROT ! rotated longitude of all points


REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:LATLON_LONLAT_ROT',0,ZHOOK_HANDLE)

KL = KLON * KLAT

ALLOCATE(ZLATROT(KL))
ALLOCATE(ZLONROT(KL))

!
DO JLAT=1,KLAT
  DO JLON=1,KLON
    JL = JLON + KLON * (JLAT-1)
    ZLATROT(JL) = PSOUTH + PDLAT * (JLAT-1)
    ZLONROT(JL) = PWEST  + PDLON * (JLON-1)
  END DO
END DO

 CALL REGROT_LONLAT_ROT(PLON,PLAT,ZLONROT,ZLATROT,   &
                             KL,1,KL,1,             &
                             PPOLON,PPOLAT,-1       )  

WHERE (PLON(:)>180.) PLON(:)=PLON(:)-360.
WHERE (PLON(:)<-180.) PLON(:)=PLON(:)+360.

IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_ROT:LATLON_LONLAT_ROT',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_LONLAT_ROT
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!
END MODULE MODE_GRIDTYPE_LONLAT_ROT
