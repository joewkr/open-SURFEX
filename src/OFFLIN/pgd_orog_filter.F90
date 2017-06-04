!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_OROG_FILTER (U, UG, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_OROGRAPHY* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    12/2008 E. Martin : add case 'MAX' for choice of orography
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PGD_GRID,       ONLY : CGRID, XGRID_PAR
!
USE MODI_READ_NAM_PGD_OROG_FILTER
USE MODI_OROGRAPHY_FILTER
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZZS, ZSEA
INTEGER                  :: NZSFILTER   ! number of orographic spatial filter iterations
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_OROG_FILTER(HPROGRAM, NZSFILTER )  
!
!-------------------------------------------------------------------------------
!
!*   11.      Filtering of orography
!             ----------------------
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZZS (U%NDIM_FULL))
  ALLOCATE(ZSEA(U%NDIM_FULL))
ELSE
  ALLOCATE(ZZS(0))
  ALLOCATE(ZSEA(0))
ENDIF
 CALL GATHER_AND_WRITE_MPI(U%XZS,ZZS)
 CALL GATHER_AND_WRITE_MPI(U%XSEA,ZSEA)
!
IF (NRANK==NPIO) CALL OROGRAPHY_FILTER(CGRID, UG%XGRID_FULL_PAR, ZSEA, NZSFILTER, ZZS)
!
 CALL READ_AND_SEND_MPI(ZZS,U%XZS)
!
DEALLOCATE(ZZS,ZSEA)
!
IF (LHOOK) CALL DR_HOOK('PGD_OROG_FILTER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_OROG_FILTER
