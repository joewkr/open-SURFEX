!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE SPLIT_GRID_CARTESIAN(HPROGRAM,KDIM_FULL,KSIZE_FULL,KGRID_PAR,PGRID_PAR,KHALO)
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program splits a PGD grid on several processors (according to host program)
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
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     08/11
!!    Modification 01/03/2015 KHALO as arguments (M.Moge)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_CARTESIAN
USE MODE_SPLIT_GRID_PARAMETER
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
INTEGER, OPTIONAL,  INTENT(IN)    :: KHALO ! size of the Halo
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!* original grid
REAL                            :: ZLAT0, ZLON0
INTEGER                         :: IIMAX, IJMAX
REAL, DIMENSION(KDIM_FULL)      :: ZX, ZY, ZDX, ZDY
!
!* splitted grid on processor
INTEGER                         :: IIMAX_SPLIT, IJMAX_SPLIT
REAL, DIMENSION(:), ALLOCATABLE :: ZX_SPLIT, ZY_SPLIT, ZDX_SPLIT, ZDY_SPLIT
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID_CARTESIAN',0,ZHOOK_HANDLE)
!
!*    1.      Gets Parameters of the Grid
!
 CALL GET_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0,           &
                            IIMAX,IJMAX,                     &
                            ZX,ZY,ZDX,ZDY                    )
!
!
!*    2.      Splits the (pertinent) parameters of the grid
!
IF (PRESENT(KHALO)) THEN
  CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CARTESIAN ','IMAX  ',IIMAX,IIMAX_SPLIT,KHALO)
  CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CARTESIAN ','JMAX  ',IJMAX,IJMAX_SPLIT,KHALO)
ELSE
  CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CARTESIAN ','IMAX  ',IIMAX,IIMAX_SPLIT)
  CALL SPLIT_GRID_PARAMETERN0(HPROGRAM,'CARTESIAN ','JMAX  ',IJMAX,IJMAX_SPLIT)
ENDIF
!
KSIZE_FULL = IIMAX_SPLIT * IJMAX_SPLIT
!
ALLOCATE(ZX_SPLIT (KSIZE_FULL))
ALLOCATE(ZY_SPLIT (KSIZE_FULL))
ALLOCATE(ZDX_SPLIT(KSIZE_FULL))
ALLOCATE(ZDY_SPLIT(KSIZE_FULL))
!
IF (PRESENT(KHALO)) THEN
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','XX    ',KDIM_FULL,KSIZE_FULL,ZX,ZX_SPLIT,IIMAX,IJMAX,KHALO)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','YY    ',KDIM_FULL,KSIZE_FULL,ZY,ZY_SPLIT,IIMAX,IJMAX,KHALO)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','DX    ',KDIM_FULL,KSIZE_FULL,ZDX,ZDX_SPLIT,IIMAX,IJMAX,KHALO)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','DY    ',KDIM_FULL,KSIZE_FULL,ZDY,ZDY_SPLIT,IIMAX,IJMAX,KHALO)
ELSE
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','XX    ',KDIM_FULL,KSIZE_FULL,ZX,ZX_SPLIT)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','YY    ',KDIM_FULL,KSIZE_FULL,ZY,ZY_SPLIT)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','DX    ',KDIM_FULL,KSIZE_FULL,ZDX,ZDX_SPLIT)
  CALL SPLIT_GRID_PARAMETERX1(HPROGRAM,'CARTESIAN ','DY    ',KDIM_FULL,KSIZE_FULL,ZDY,ZDY_SPLIT)
ENDIF
!
!
!*    3.      Stores Parameters of the Grid in grid pointer
!
NULLIFY(PGRID_PAR)
 CALL PUT_GRIDTYPE_CARTESIAN(PGRID_PAR,ZLAT0,ZLON0,                  &
                            IIMAX_SPLIT,IJMAX_SPLIT,                &
                            ZX_SPLIT,ZY_SPLIT,ZDX_SPLIT,ZDY_SPLIT   )
                            !
!
KGRID_PAR = SIZE(PGRID_PAR)
!
DEALLOCATE(ZX_SPLIT )
DEALLOCATE(ZY_SPLIT )
DEALLOCATE(ZDX_SPLIT)
DEALLOCATE(ZDY_SPLIT)
!
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID_CARTESIAN',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE SPLIT_GRID_CARTESIAN
