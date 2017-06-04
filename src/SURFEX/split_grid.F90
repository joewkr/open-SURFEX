!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE SPLIT_GRID(UG, U, HPROGRAM,KGRID_PAR,PGRID_PAR,KHALO)
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
!!    Modification 01/03/2015 pass KGRID_PAR,PGRID_PAR,KHALO as arguments (M.Moge)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_SPLIT_GRID_CONF_PROJ
USE MODI_SPLIT_GRID_CARTESIAN
USE MODI_GET_SIZE_FULL_n
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
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
!
INTEGER, OPTIONAL,           INTENT(INOUT)  :: KGRID_PAR ! size of PGRID_PAR pointer
REAL, DIMENSION(:), OPTIONAL, POINTER, INTENT(INOUT) :: PGRID_PAR ! parameters defining this grid
INTEGER,            INTENT(IN), OPTIONAL   :: KHALO ! size of the Halo
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER :: IRESP, ISIZE_FULL ! error return code
INTEGER :: IHALO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SPLIT_GRID',0,ZHOOK_HANDLE)
!
#ifdef MNH_PARALLEL
IF (PRESENT(KHALO)) THEN
  IHALO = KHALO
ELSE
  IHALO = 0
ENDIF
#else
IHALO = 0
#endif
!
SELECT CASE(UG%G%CGRID)

  CASE('CONF PROJ ')
    IF (PRESENT(KGRID_PAR).AND.PRESENT(PGRID_PAR)) THEN
      CALL SPLIT_GRID_CONF_PROJ(HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL,KGRID_PAR,PGRID_PAR,IHALO)
    ELSE
      CALL SPLIT_GRID_CONF_PROJ(HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL,UG%G%NGRID_PAR,UG%G%XGRID_PAR)
    ENDIF

  CASE('CARTESIAN ')
    IF (PRESENT(KGRID_PAR).AND.PRESENT(PGRID_PAR)) THEN
      CALL SPLIT_GRID_CARTESIAN(HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL,KGRID_PAR,PGRID_PAR,IHALO)
    ELSE
      CALL SPLIT_GRID_CARTESIAN(HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL,UG%G%NGRID_PAR,UG%G%XGRID_PAR)
    ENDIF

  CASE DEFAULT
    CALL GET_SIZE_FULL_n(HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL,ISIZE_FULL)
    U%NSIZE_FULL = ISIZE_FULL

END SELECT
!

IF (LHOOK) CALL DR_HOOK('SPLIT_GRID',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE SPLIT_GRID
