!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_GRIDTYPE_LONLAT_ROT (&
                                           HPROGRAM,KGRID_PAR,KLU,OREAD,KSIZE,PGRID_PAR,KRESP,HDIR)
!     ################################################################
!
!!****  *READ_GRIDTYPE_LONLAT_ROT* - routine to initialise the horizontal grid
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
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
!
!
!
USE MODI_READ_SURF
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_LONLAT_ROT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
CHARACTER(LEN=6),       INTENT(IN)    :: HPROGRAM   ! calling program
INTEGER,                INTENT(INOUT) :: KGRID_PAR  ! real size of PGRID_PAR
INTEGER,                INTENT(IN)    :: KLU        ! number of points
LOGICAL,                INTENT(IN)    :: OREAD      ! flag to read the grid
INTEGER,                INTENT(IN)    :: KSIZE      ! estimated size of PGRID_PAR
REAL, DIMENSION(KSIZE), INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
INTEGER,                INTENT(OUT)   :: KRESP      ! error return code
 CHARACTER(LEN=1),       INTENT(IN)    :: HDIR       ! reading directive
!                                                   ! 'A' : all field
!                                                   ! 'H' : field on this processor only
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL    :: ZWEST   ! West longitude in rotated grid (degrees)
REAL    :: ZSOUTH  ! South latitude in rotated grid  (degrees)
REAL    :: ZDLON   ! Longitudal grid spacing  (degrees)
REAL    :: ZDLAT   ! Latitudal grid spacing  (degrees)
REAL    :: ZPOLON  ! Longitude of rotated pole (degrees)
REAL    :: ZPOLAT  ! Latitude of rotated pole  (degrees)
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
REAL, DIMENSION(KLU) :: ZLON ! longitudes
REAL, DIMENSION(KLU) :: ZLAT ! latitudes
!
INTEGER :: ILUOUT
!---------------------------------------------------------------------------
REAL, DIMENSION(:),   POINTER     :: ZGRID_PAR=>NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of the grid
!              -------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_LONLAT_ROT',0,ZHOOK_HANDLE)

 CALL READ_SURF(&
                HPROGRAM,'WEST',ZWEST,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'SOUTH',ZSOUTH,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'DLON',ZDLON,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'DLAT',ZDLAT,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'POLON',ZPOLON,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'POLAT',ZPOLAT,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'NLON',ILON,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'NLAT',ILAT,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'REG_LON',ZLON,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'REG_LAT',ZLAT,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       2.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_LONLAT_ROT(ZGRID_PAR,                                 &
                               ZWEST,ZSOUTH,ZDLON,ZDLAT,ZPOLON,ZPOLAT,  &
                               ILON,ILAT,KLU,ZLON,ZLAT                  )  
!
!---------------------------------------------------------------------------
IF (OREAD) THEN
  IF (SIZE(PGRID_PAR) /= SIZE(ZGRID_PAR)) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*)'size of PGRID_PAR =', SIZE(PGRID_PAR)
    WRITE(ILUOUT,*)'size of ZGRID_PAR =', SIZE(ZGRID_PAR)
    CALL ABOR1_SFX('READ_GRIDTYPE_LONLAT_ROT: SIZE OF PGRID_PAR IS NOT CORRECT')
  END IF
  !
  PGRID_PAR = ZGRID_PAR
ELSE
  KGRID_PAR = SIZE(ZGRID_PAR)
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_LONLAT_ROT',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE_LONLAT_ROT
