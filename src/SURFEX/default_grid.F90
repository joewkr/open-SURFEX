!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_GRID(HPROGRAM,HGRID)
!     ######################################
!!
!!    PURPOSE
!!    -------
!!   set default for the surface GRID.
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
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_MNH
USE MODI_DEFAULT_GRID_MNH
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling READ_PGD
 CHARACTER(LEN=10), INTENT(OUT) :: HGRID    ! type of grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_GRID',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL DEFAULT_GRID_MNH(HGRID)
#endif
ELSE
  HGRID = 'NONE      '
END IF
IF (LHOOK) CALL DR_HOOK('DEFAULT_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_GRID
