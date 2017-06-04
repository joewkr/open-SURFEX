!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_CARTESIAN
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
      SUBROUTINE PUT_GRIDTYPE_CARTESIAN(PGRID_PAR,PLAT0,PLON0,         &
                                          KIMAX,KJMAX,PX,PY,PDX,PDY      )  
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_CARTESIAN* - routine to store in PGRID_PAR the horizontal grid
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
REAL,               INTENT(IN)  :: PLAT0    ! reference latitude
REAL,               INTENT(IN)  :: PLON0    ! reference longitude
INTEGER,            INTENT(IN)  :: KIMAX    ! number of points in I direction
INTEGER,            INTENT(IN)  :: KJMAX    ! number of points in J direction
REAL, DIMENSION(:), INTENT(IN)  :: PX       ! X conformal coordinate of left boundary of grid mesh
REAL, DIMENSION(:), INTENT(IN)  :: PY       ! Y conformal coordinate of bottom boundary of grid mesh
REAL, DIMENSION(:), INTENT(IN)  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(IN)  :: PDY      ! Y grid mesh size
REAL, DIMENSION(:), POINTER     :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: IL       ! number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:PUT_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
IL = SIZE(PX)
ALLOCATE(PGRID_PAR(4+4*IL))
PGRID_PAR(1) = PLAT0
PGRID_PAR(2) = PLON0
PGRID_PAR(3) = FLOAT(KIMAX)
PGRID_PAR(4) = FLOAT(KJMAX)
PGRID_PAR(4     +1:4+  IL) = PX(:)
PGRID_PAR(4+  IL+1:4+2*IL) = PY(:)
PGRID_PAR(4+2*IL+1:4+3*IL) = PDX(:)
PGRID_PAR(4+3*IL+1:4+4*IL) = PDY(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:PUT_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_CARTESIAN
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_CARTESIAN(PGRID_PAR,PLAT0,PLON0,           &
                                          KIMAX,KJMAX,PX,PY,PDX,PDY,KL     )  
!     ####################################################################
!
!!****  *GET_GRIDTYPE_CARTESIAN* - routine to get from PGRID_PAR the horizontal grid
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
REAL,               INTENT(OUT), OPTIONAL :: PLAT0    ! reference latitude
REAL,               INTENT(OUT), OPTIONAL :: PLON0    ! reference longitude
INTEGER,            INTENT(OUT), OPTIONAL :: KIMAX    ! number of points in I direction
INTEGER,            INTENT(OUT), OPTIONAL :: KJMAX    ! number of points in J direction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PX       ! X conformal coor. of grid mesh 
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PY       ! Y conformal coor. of grid mesh
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDY      ! Y grid mesh size
INTEGER,            INTENT(OUT), OPTIONAL :: KL       ! number of points
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:GET_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
IF (PRESENT(PLAT0))  PLAT0 = PGRID_PAR(1)
IF (PRESENT(PLON0))  PLON0 = PGRID_PAR(2)
IF (PRESENT(KIMAX))  KIMAX = NINT(PGRID_PAR(3))
IF (PRESENT(KJMAX))  KJMAX = NINT(PGRID_PAR(4))
!
IF (PRESENT(PX)) THEN
  IL = SIZE(PX)
  PX(:) = PGRID_PAR(4+1:4+IL)
END IF

IF (PRESENT(PY)) THEN
  IL = SIZE(PY)
  PY(:) = PGRID_PAR(4+IL+1:4+2*IL)
END IF

IF (PRESENT(PDX)) THEN
  IL = SIZE(PDX)
  PDX(:)= PGRID_PAR(4+2*IL+1:4+3*IL)
END IF

IF (PRESENT(PDY)) THEN
  IL = SIZE(PDY)
  PDY(:)= PGRID_PAR(4+3*IL+1:4+4*IL)
END IF
!
IF (PRESENT(KL)) KL = (SIZE(PGRID_PAR)-4)/4
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:GET_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_CARTESIAN
!############################################################################
!############################################################################
!############################################################################
!      ###################################################
       SUBROUTINE LATLON_CARTESIAN(PLAT0,PLON0,PLAT,PLON)
!      ###################################################
!
!!****  *LATLON_CARTESIAN * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!
!
!!**   METHOD
!!     ------
!!       
!!     AUTHOR
!!     ------
!!      V. Masson  *Meteo France*
!!
!!     MODIFICATION
!!     ------------
!!       Original  06/2004
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,                 INTENT(IN) :: PLAT0  ! Reference latitude
REAL,                 INTENT(IN) :: PLON0  ! Reference longitude
REAL, DIMENSION(:),   INTENT(OUT):: PLAT,PLON    
REAL(KIND=JPRB) :: ZHOOK_HANDLE
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
!*     0.2    Declarations of local variables
! 
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:LATLON_CARTESIAN',0,ZHOOK_HANDLE)
PLON(:) = PLON0 
PLAT(:) = PLAT0 
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CARTESIAN:LATLON_CARTESIAN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_CARTESIAN
!---------------------------------------------------------------------------------
!
END MODULE MODE_GRIDTYPE_CARTESIAN
