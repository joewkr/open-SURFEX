!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_GRIDTYPE_IGN (HPROGRAM,KGRID_PAR,KLU,OREAD,KSIZE,PGRID_PAR,KRESP,HDIR)
!     ################################################################
!
!!****  *READ_GRIDTYPE_IGN* - routine to initialise the horizontal grid
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
!!      E. Martin   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!!      07/2011     add maximum domain dimension for output (B. Decharme)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_READ_SURF
USE MODI_GET_LUOUT
USE MODI_GET_XYALL_IGN
!
USE MODE_GRIDTYPE_IGN
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
INTEGER                           :: ILAMBERT ! Lambert type
REAL, DIMENSION(KLU)              :: ZX       ! X Lambert coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZY       ! Y  Lambert coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZDX      ! X grid mesh size
REAL, DIMENSION(KLU)              :: ZDY      ! Y grid mesh size
!
REAL, DIMENSION(KLU*3)            :: ZXALL    ! maximum domain X coordinate of grid mesh
REAL, DIMENSION(KLU*3)            :: ZYALL    ! maximum domain Y coordinate of grid mesh
INTEGER                           :: IDIMX    ! maximum domain length in X
INTEGER                           :: IDIMY    ! maximum domain length in Y
INTEGER                           :: ILUOUT
!---------------------------------------------------------------------------
REAL, DIMENSION(:),   POINTER     :: ZGRID_PAR=>NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of projection parameters
!              --------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
 CALL READ_SURF(HPROGRAM,'LAMBERT',ILAMBERT,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_SURF(HPROGRAM,'XX',ZX,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'XY',ZY,KRESP,HDIR=HDIR)
!
 CALL READ_SURF(HPROGRAM,'DX',ZDX,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'DY',ZDY,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       7.    maximum domain lengths
!              ----------------------
!
IF (HDIR=='A') THEN
  CALL GET_XYALL_IGN(ZX,ZY,ZDX,ZDY,ZXALL,ZYALL,IDIMX,IDIMY)
 CALL PUT_GRIDTYPE_IGN(ZGRID_PAR,ILAMBERT,ZX,ZY,ZDX,ZDY,        &
                      IDIMX,IDIMY,ZXALL(1:IDIMX),ZYALL(1:IDIMY))
ELSE
  IDIMX = 0
  IDIMY = 0
  CALL PUT_GRIDTYPE_IGN(ZGRID_PAR,ILAMBERT,ZX,ZY,ZDX,ZDY,        &
                      IDIMX,IDIMY,ZXALL,ZYALL)
ENDIF
!
!--------------------------------------------------------------------------
!
!*       4.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
!
!---------------------------------------------------------------------------
IF (OREAD) THEN
  IF (SIZE(PGRID_PAR) /= SIZE(ZGRID_PAR)) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*)'size of PGRID_PAR =', SIZE(PGRID_PAR)
    WRITE(ILUOUT,*)'size of ZGRID_PAR =', SIZE(ZGRID_PAR)
    CALL ABOR1_SFX('READ_GRIDTYPE_IGN: SIZE OF PGRID_PAR IS NOT CORRECT')
  END IF
  !
  PGRID_PAR = ZGRID_PAR
ELSE
  KGRID_PAR = SIZE(ZGRID_PAR)
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE_IGN
