!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PUT_PGD_GRID(HGRID,KSIZE_FULL,KGRID_PAR,PGRID_PAR)
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Stores a grid in module MODD_SURF_ATM_GRID_n
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
!!    Original     07/2011
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID, ONLY : CGRID, NGRID_PAR, XGRID_PAR, NL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=10), INTENT(IN)   :: HGRID      ! type of horizontal grid
INTEGER,           INTENT(IN)   :: KSIZE_FULL ! number of points
INTEGER,           INTENT(IN)   :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
!*       1.    Defaults
!              --------
!
IF (LHOOK) CALL DR_HOOK('PUT_PGD_GRID',0,ZHOOK_HANDLE)
!
CGRID = HGRID
NL=KSIZE_FULL
NGRID_PAR=KGRID_PAR
ALLOCATE(XGRID_PAR(NGRID_PAR))
XGRID_PAR = PGRID_PAR
!
IF (LHOOK) CALL DR_HOOK('GRID_FROM_FILE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PUT_PGD_GRID
