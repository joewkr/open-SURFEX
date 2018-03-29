!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PGD_GRID_IO_INIT
CONTAINS
      SUBROUTINE PGD_GRID_IO_INIT(HPROGRAM,UG,KGRID_PAR,PGRID_PAR,HGRID,ORECT,KIMAX,KJMAX,KDXRATIO,KDYRATIO)
!     ######################################
!!
!!    PURPOSE
!!    -------
!!
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
!!    Original     13/10/03
!!        M.Moge   11/02/15 adding MODULE MODI_PGD_GRID_IO_INIT and INTERFACE + modif of the input args
!!        M.Moge   11/02/15 change in the input arguments : passing KDXRATIO,KDYRATIO
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_MNH
USE MODI_PGD_GRID_IO_INIT_MNH
#endif
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling READ_PGD
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 INTEGER, OPTIONAL,               INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
 REAL, DIMENSION(:), OPTIONAL, INTENT(IN)       :: PGRID_PAR ! grid parameters
 CHARACTER(LEN=10),     INTENT(IN), OPTIONAL    :: HGRID
 LOGICAL,               INTENT(IN), OPTIONAL    :: ORECT
! if KIMAX,KJMAX,KDXRATIO,KDYRATIO present, this means we are in PREP_PGD, and we only initialise the child model,
! using a father model read from a file and previously initialized with INI_PARAZ_ll
 INTEGER,               INTENT(IN), OPTIONAL    :: KIMAX
 INTEGER,               INTENT(IN), OPTIONAL    :: KJMAX
 INTEGER,               INTENT(IN), OPTIONAL    :: KDXRATIO
 INTEGER,               INTENT(IN), OPTIONAL    :: KDYRATIO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_GRID_IO_INIT',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
  IF (PRESENT(KGRID_PAR).AND.PRESENT(PGRID_PAR)) THEN
#ifdef MNH_PARALLEL
    IF ( PRESENT(KIMAX) .AND. PRESENT(KJMAX) .AND. PRESENT(HGRID) .AND. PRESENT(ORECT) &
      .AND. PRESENT(KDXRATIO) .AND. PRESENT(KDYRATIO) ) THEN
      CALL PGD_GRID_IO_INIT_MNH(UG,KGRID_PAR,PGRID_PAR,HGRID,ORECT,KIMAX,KJMAX,KDXRATIO,KDYRATIO)
    ELSE
      CALL PGD_GRID_IO_INIT_MNH(UG,KGRID_PAR,PGRID_PAR)
    ENDIF
#endif
  ELSE
#ifndef MNH_PARALLEL
#ifdef SFX_MNH
    CALL PGD_GRID_IO_INIT_MNH(UG)
#endif
#endif
  ENDIF
END IF
IF (LHOOK) CALL DR_HOOK('PGD_GRID_IO_INIT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GRID_IO_INIT
END MODULE MODI_PGD_GRID_IO_INIT
