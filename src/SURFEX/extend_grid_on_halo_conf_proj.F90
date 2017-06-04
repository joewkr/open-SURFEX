!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE EXTEND_GRID_ON_HALO_CONF_PROJ(HPROGRAM,KDIM_FULL,KSIZE_FULL,KGRID_PAR,PGRID_PAR)
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program extends a splitted PGD grid on the SURFEX halo
!!
!!    METHOD
!!    ------
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
!!    M.Moge                   CNRS - LA
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/03/2015
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODE_EXTEND_GRID_PARAMETER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM  ! host program 
INTEGER,            INTENT(IN)    :: KDIM_FULL ! total number of points
INTEGER,            INTENT(OUT)   :: KSIZE_FULL! number of points on this processor
INTEGER,            INTENT(INOUT) :: KGRID_PAR ! size of PGRID_PAR pointer
REAL, DIMENSION(:), POINTER       :: PGRID_PAR ! parameters defining this grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!* original grid
REAL                            :: ZLAT0, ZLON0, ZRPK, ZBETA, ZLATOR, ZLONOR
INTEGER                         :: IIMAX, IJMAX
REAL, DIMENSION(INT(PGRID_PAR(11)))      :: ZX, ZY, ZDX, ZDY
!
!* extended grid
INTEGER                         :: IIMAX_EXTENDED, IJMAX_EXTENDED
REAL, DIMENSION(:), ALLOCATABLE :: ZX_EXTENDED, ZY_EXTENDED, ZDX_EXTENDED, ZDY_EXTENDED
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('EXTEND_GRID_CONF_PROJ',0,ZHOOK_HANDLE)
!
!*    1.      Gets Parameters of the Grid
!
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,&
                            ZLATOR,ZLONOR,IIMAX,IJMAX,        &
                            ZX,ZY,ZDX,ZDY                     )
!
!
!*    2.      Splits the (pertinent) parameters of the grid
!
 CALL EXTEND_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','IMAX  ',IIMAX,IIMAX_EXTENDED)
 CALL EXTEND_GRID_PARAMETERN0(HPROGRAM,'CONF PROJ ','JMAX  ',IJMAX,IJMAX_EXTENDED)
!
KSIZE_FULL = IIMAX_EXTENDED * IJMAX_EXTENDED
!
ALLOCATE(ZX_EXTENDED (KSIZE_FULL))
ALLOCATE(ZY_EXTENDED (KSIZE_FULL))
ALLOCATE(ZDX_EXTENDED(KSIZE_FULL))
ALLOCATE(ZDY_EXTENDED(KSIZE_FULL))
 CALL EXTEND_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','XX    ',SIZE(ZX),KSIZE_FULL,IIMAX,IJMAX,ZX,ZX_EXTENDED)
 CALL EXTEND_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','YY    ',SIZE(ZY),KSIZE_FULL,IIMAX,IJMAX,ZY,ZY_EXTENDED)
 CALL EXTEND_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DX    ',SIZE(ZDX),KSIZE_FULL,IIMAX,IJMAX,ZDX,ZDX_EXTENDED)
 CALL EXTEND_GRID_PARAMETERX1(HPROGRAM,'CONF PROJ ','DY    ',SIZE(ZDY),KSIZE_FULL,IIMAX,IJMAX,ZDY,ZDY_EXTENDED)
!
!
!*    3.      Stores Parameters of the Grid in grid pointer
!
NULLIFY(PGRID_PAR)
 CALL PUT_GRIDTYPE_CONF_PROJ(PGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,       &
                            ZLATOR,ZLONOR,IIMAX_EXTENDED,IJMAX_EXTENDED,  &
                            ZX_EXTENDED,ZY_EXTENDED,ZDX_EXTENDED,ZDY_EXTENDED   )
                            !
!
KGRID_PAR = SIZE(PGRID_PAR)
!
DEALLOCATE(ZX_EXTENDED )
DEALLOCATE(ZY_EXTENDED )
DEALLOCATE(ZDX_EXTENDED)
DEALLOCATE(ZDY_EXTENDED)
!
IF (LHOOK) CALL DR_HOOK('EXTEND_GRID_CONF_PROJ',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE EXTEND_GRID_ON_HALO_CONF_PROJ
