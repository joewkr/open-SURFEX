!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_GRID_DIM_LONLATVAL(KGRID_PAR,PGRID_PAR,ORECT,KDIM1,KDIM2)
!     ##############################################################
!
!!**** *GET_GRID_DIM_LONLAT_REG* get the grid mesh dimensions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_LONLATVAL
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
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
LOGICAL,                         INTENT(OUT)   :: ORECT     ! T if rectangular grid
INTEGER,                         INTENT(OUT)   :: KDIM1     ! 1st dimension
INTEGER,                         INTENT(OUT)   :: KDIM2     ! 2nd dimension
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL,DIMENSION(:),ALLOCATABLE :: ZX, ZY,V0
INTEGER:: JX, JY, IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_DIM_LONLATVAL',0,ZHOOK_HANDLE)
ORECT = .TRUE.
!
 CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,KL=IL)
!
ALLOCATE(ZX(IL))
ALLOCATE(ZY(IL))
ALLOCATE(V0(IL))
!
 CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,PX=ZX,PY=ZY)
!
V0(:)=1
DO JX=1,IL
  IF (V0(JX)==1) THEN
    DO JY=1,IL
      IF (ZX(JX)==ZX(JY) .AND. JX.NE.JY) V0(JY)=0
    ENDDO
  ENDIF
ENDDO
KDIM1=SUM(V0)
!
V0(:)=1
DO JX=1,IL
  IF (V0(JX)==1) THEN
    DO JY=1,IL
      IF (ZY(JX)==ZY(JY) .AND. JX.NE.JY) V0(JY)=0
    ENDDO
  ENDIF
ENDDO
KDIM2=SUM(V0)
!
DEALLOCATE(ZX)
DEALLOCATE(ZY)
DEALLOCATE(V0)
IF (LHOOK) CALL DR_HOOK('GET_GRID_DIM_LONLATVAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_DIM_LONLATVAL
