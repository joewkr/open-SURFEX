!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_GRIDTYPE_CARTESIAN (&
                                          HPROGRAM,KGRID_PAR,KLU,OREAD,KSIZE,PGRID_PAR,KRESP,HDIR)
!     ################################################################
!
!!****  *READ_GRIDTYPE_CARTESIAN* - routine to initialise the horizontal grid
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
!
!
!
USE MODI_READ_SURF
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_CARTESIAN
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
 CHARACTER(LEN=1),       INTENT(IN)   :: HDIR       ! reading directive ('A','H')
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
INTEGER                           :: IIMAX    ! number of points in I direction
INTEGER                           :: IJMAX    ! number of points in J direction
REAL, DIMENSION(KLU)              :: ZX       ! X conformal coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZY       ! Y conformal coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZDX      ! X grid mesh size
REAL, DIMENSION(KLU)              :: ZDY      ! Y grid mesh size
!
INTEGER                           :: ILUOUT
!---------------------------------------------------------------------------
REAL, DIMENSION(:),   POINTER     :: ZGRID_PAR=>NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_CARTESIAN',0,ZHOOK_HANDLE)
 CALL READ_SURF(&
                HPROGRAM,'LAT0',ZLAT0,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'LON0',ZLON0,KRESP,HDIR=HDIR)
!
 CALL READ_SURF(&
                HPROGRAM,'IMAX ',IIMAX, KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'JMAX ',IJMAX, KRESP,HDIR=HDIR)
!
 CALL READ_SURF(&
                HPROGRAM,'XX',ZX,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'YY',ZY,KRESP,HDIR=HDIR)
!
 CALL READ_SURF(&
                HPROGRAM,'DX',ZDX,KRESP,HDIR=HDIR)
 CALL READ_SURF(&
                HPROGRAM,'DY',ZDY,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       4.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_CARTESIAN(ZGRID_PAR,ZLAT0,ZLON0,           &
                              IIMAX,IJMAX,                     &
                              ZX,ZY,ZDX,ZDY                    )  
!
!---------------------------------------------------------------------------
IF (OREAD) THEN
  IF (SIZE(PGRID_PAR) /= SIZE(ZGRID_PAR)) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*)'size of PGRID_PAR =', SIZE(PGRID_PAR)
    WRITE(ILUOUT,*)'size of ZGRID_PAR =', SIZE(ZGRID_PAR)
    CALL ABOR1_SFX('READ_GRIDTYPE_CARTESIAN: SIZE OF PGRID_PAR IS NOT CORRECT')
  END IF
  !
  PGRID_PAR = ZGRID_PAR
ELSE
  KGRID_PAR = SIZE(ZGRID_PAR)
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_CARTESIAN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE_CARTESIAN
