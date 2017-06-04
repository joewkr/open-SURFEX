!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_LONLAT_REG
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
      SUBROUTINE PUT_GRIDTYPE_LONLAT_REG(PGRID_PAR,PLONMIN,PLONMAX,             &
                                           PLATMIN,PLATMAX,KLON,KLAT,KL,PLON,PLAT )  
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_LONLAT_REG* - routine to store in PGRID_PAR the horizontal grid
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
REAL,               INTENT(IN)  :: PLONMIN  ! minimum longitude
REAL,               INTENT(IN)  :: PLONMAX  ! maximum longitude
REAL,               INTENT(IN)  :: PLATMIN  ! minimum latitude
REAL,               INTENT(IN)  :: PLATMAX  ! maximum latitude
INTEGER,            INTENT(IN)  :: KLON     ! number of points in longitude
INTEGER,            INTENT(IN)  :: KLAT     ! number of points in latitude
INTEGER,            INTENT(IN)  :: KL       ! number of points used
REAL, DIMENSION(:), INTENT(IN)  :: PLON     ! longitudes of all points
REAL, DIMENSION(:), INTENT(IN)  :: PLAT     ! latitudes  of all points
REAL,   DIMENSION(:),POINTER    :: PGRID_PAR! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:PUT_GRIDTYPE_LONLAT_REG',0,ZHOOK_HANDLE)
ALLOCATE(PGRID_PAR(7+2*KL))
PGRID_PAR(1) = PLONMIN
PGRID_PAR(2) = PLONMAX
PGRID_PAR(3) = PLATMIN
PGRID_PAR(4) = PLATMAX
PGRID_PAR(5) = FLOAT(KLON)
PGRID_PAR(6) = FLOAT(KLAT)
PGRID_PAR(7) = FLOAT(KL)
PGRID_PAR(8:7+KL)      = PLON(:)
PGRID_PAR(8+KL:7+2*KL) = PLAT(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:PUT_GRIDTYPE_LONLAT_REG',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_LONLAT_REG
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,PLONMIN,PLONMAX, &
                                           PLATMIN,PLATMAX,KLON,KLAT, &
                                           KL,PLON,PLAT               )  
!     ####################################################################
!
!!****  *GET_GRIDTYPE_LONLAT_REG* - routine to get from PGRID_PAR the horizontal grid
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
REAL, DIMENSION(:), INTENT(IN)            :: PGRID_PAR! parameters defining this grid
REAL,               INTENT(OUT), OPTIONAL :: PLONMIN  ! minimum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLONMAX  ! maximum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMIN  ! minimum latitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMAX  ! maximum latitude
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
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:GET_GRIDTYPE_LONLAT_REG',0,ZHOOK_HANDLE)
ILON = NINT(PGRID_PAR(5))
ILAT = NINT(PGRID_PAR(6))
IL   = NINT(PGRID_PAR(7))
!
IF (PRESENT(PLONMIN))  PLONMIN = PGRID_PAR(1)
IF (PRESENT(PLONMAX))  PLONMAX = PGRID_PAR(2)
IF (PRESENT(PLATMIN))  PLATMIN = PGRID_PAR(3)
IF (PRESENT(PLATMAX))  PLATMAX = PGRID_PAR(4)
IF (PRESENT(KLON    )) KLON    = ILON
IF (PRESENT(KLAT    )) KLAT    = ILAT
IF (PRESENT(KL      )) KL      = IL
IF (PRESENT(PLON    )) PLON(:) = PGRID_PAR(8:7+IL)
IF (PRESENT(PLAT    )) PLAT(:) = PGRID_PAR(8+IL:7+2*IL)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:GET_GRIDTYPE_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_LONLAT_REG
!---------------------------------------------------------------------------------
!
!      ###################################################
       SUBROUTINE LATLON_LONLAT_REG(PLONMIN,PLONMAX,PLATMIN,PLATMAX,&
                                      KLON,KLAT,PLON,PLAT             )  
!      ###################################################
!
!!****  *LATLON_LONLAT_REG * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!!       
!!     AUTHOR
!!     ------
!!      V. Masson  *Meteo-France*
!!
!!     MODIFICATION
!!     ------------
!!       Original   03/2004
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,               INTENT(IN)  :: PLONMIN  ! minimum longitude
REAL,               INTENT(IN)  :: PLONMAX  ! maximum longitude
REAL,               INTENT(IN)  :: PLATMIN  ! minimum latitude
REAL,               INTENT(IN)  :: PLATMAX  ! maximum latitude
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
INTEGER :: JL
INTEGER :: JC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:LATLON_LONLAT_REG',0,ZHOOK_HANDLE)
JL = 0
!
DO JLAT=1,KLAT
  DO JLON=1,KLON
    JC = JLON + (JLAT-1) * KLON 
    JL = JL + 1
    PLON(JL) = PLONMIN + (PLONMAX-PLONMIN) * (FLOAT(JLON)-0.5) / FLOAT(KLON)
    PLAT(JL) = PLATMIN + (PLATMAX-PLATMIN) * (FLOAT(JLAT)-0.5) / FLOAT(KLAT)
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLAT_REG:LATLON_LONLAT_REG',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_LONLAT_REG
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!
END MODULE MODE_GRIDTYPE_LONLAT_REG
