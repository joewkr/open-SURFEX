!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!################################################################
SUBROUTINE READ_GRIDTYPE_GAUSS (HPROGRAM,KGRID_PAR,KLU,OREAD,KSIZE,PGRID_PAR,KRESP,HDIR)
!################################################################
!
!!****  *READ_GRIDTYPE_GAUSS* - routine to initialise the horizontal grid
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
USE MODI_READ_SURF
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_GAUSS
!
USE MODD_SURF_PAR, ONLY : XUNDEF
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: INLATI  ! number of pseudo-latitudes
REAL    :: ZLAPO   ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO   ! longitude of the rotated pole (deg)
REAL    :: ZCODIL  ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL,    DIMENSION(KLU) :: ZLAT    ! latitudes
REAL,    DIMENSION(KLU) :: ZLON    ! longitudes
REAL,    DIMENSION(KLU) :: ZLAT_XY ! pseudo-latitudes
REAL,    DIMENSION(KLU) :: ZLON_XY ! pseudo-longitudes
REAL,    DIMENSION(KLU) :: ZMESH_SIZE ! Mesh size
!                                                                 _____ Sup
REAL,    DIMENSION(KLU) :: ZLATSUP     ! Grid corner Latitude    |     |
REAL,    DIMENSION(KLU) :: ZLONSUP     ! Grid corner Longitude   |     |
REAL,    DIMENSION(KLU) :: ZLATINF     ! Grid corner Latitude    |_____|
REAL,    DIMENSION(KLU) :: ZLONINF     ! Grid corner Longitude  Inf
!
INTEGER :: IVERSION
INTEGER                 :: ILUOUT
!---------------------------------------------------------------------------
REAL, DIMENSION(:),   POINTER     :: ZGRID_PAR=>NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of projection parameters
!              --------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_GAUSS',0,ZHOOK_HANDLE)
!
 CALL READ_SURF(HPROGRAM,'LAPO',ZLAPO, KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LOPO',ZLOPO,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'CODIL',ZCODIL,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_SURF(HPROGRAM,'NLATI',INLATI,KRESP,HDIR=HDIR)
ALLOCATE(INLOPA(INLATI))
IF (HDIR=='A') THEN
  CALL READ_SURF(HPROGRAM,'NLOPA',INLOPA(:),KRESP,HDIR=HDIR)
ELSE
  CALL READ_SURF(HPROGRAM,'NLOPA',INLOPA(:),KRESP,HDIR='-')
ENDIF
 CALL READ_SURF(HPROGRAM,'LATGAUSS',ZLAT(:),KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LONGAUSS',ZLON(:),KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LAT_G_XY',ZLAT_XY(:),KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LON_G_XY',ZLON_XY(:),KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'MESHGAUSS',ZMESH_SIZE(:),KRESP,HDIR=HDIR)
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,KRESP,HDIR=HDIR)
!
IF (IVERSION>=8) THEN
  CALL READ_SURF(HPROGRAM,'LONINF',ZLONINF(:),KRESP,HDIR=HDIR)
  CALL READ_SURF(HPROGRAM,'LATINF',ZLATINF(:),KRESP,HDIR=HDIR)
  CALL READ_SURF(HPROGRAM,'LONSUP',ZLONSUP(:),KRESP,HDIR=HDIR)
  CALL READ_SURF(HPROGRAM,'LATSUP',ZLATSUP(:),KRESP,HDIR=HDIR)
ELSE
  ZLONINF(:) = XUNDEF
  ZLATINF(:) = XUNDEF
  ZLONSUP(:) = XUNDEF
  ZLATSUP(:) = XUNDEF
ENDIF
!
!---------------------------------------------------------------------------
!
!*       4.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_GAUSS(ZGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA, &
                          KLU,ZLAT,ZLON,ZLAT_XY,ZLON_XY,ZMESH_SIZE,  & 
                          ZLONINF,ZLATINF,ZLONSUP,ZLATSUP            )  
!
DEALLOCATE(INLOPA)
!---------------------------------------------------------------------------
IF (OREAD) THEN
  IF (SIZE(PGRID_PAR) /= SIZE(ZGRID_PAR)) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*)'size of PGRID_PAR =', SIZE(PGRID_PAR)
    WRITE(ILUOUT,*)'size of ZGRID_PAR =', SIZE(ZGRID_PAR)
    CALL ABOR1_SFX('READ_GRIDTYPE_GAUSS: SIZE OF PGRID_PAR IS NOT CORRECT')
  END IF
  !
  PGRID_PAR = ZGRID_PAR
ELSE
  KGRID_PAR = SIZE(ZGRID_PAR)
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_GAUSS',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE_GAUSS
