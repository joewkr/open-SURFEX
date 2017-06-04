!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_OUTPUT_GRID (UG, G, KSIZE_FULL, KLUOUT)
!     #######################################
!!
!!    PURPOSE
!!    -------
!!    Computes variables used for interpolation
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
!!    Original     01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODI_GET_GRID_COORD
!
USE MODD_PREP, ONLY : XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT, LINTERP
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
!
TYPE(GRID_t), INTENT(INOUT) :: UG
TYPE(GRID_t), INTENT(INOUT) :: G
INTEGER, INTENT(IN) :: KSIZE_FULL
!
INTEGER,           INTENT(IN)  :: KLUOUT     ! output listing logical unit
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_OUTPUT_GRID',0,ZHOOK_HANDLE)
!
IF (.NOT.ALLOCATED(XLAT_OUT)) ALLOCATE(XLAT_OUT(SIZE(G%XLAT)))
IF (.NOT.ALLOCATED(XLON_OUT)) ALLOCATE(XLON_OUT(SIZE(G%XLAT)))
IF (.NOT.ALLOCATED(XX_OUT)) ALLOCATE(XX_OUT  (SIZE(G%XLAT)))
IF (.NOT.ALLOCATED(XY_OUT)) ALLOCATE(XY_OUT  (SIZE(G%XLAT)))
!
IF (.NOT.ALLOCATED(LINTERP)) ALLOCATE(LINTERP (SIZE(G%XLAT)))
!
IF (SIZE(G%XLAT)==0) THEN
  IF (LHOOK) CALL DR_HOOK('PREP_OUTPUT_GRID',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
XLAT_OUT = G%XLAT
XLON_OUT = G%XLON
LINTERP  = .TRUE.
!
 CALL GET_GRID_COORD(UG%CGRID, UG%NGRID_PAR, UG%XGRID_PAR, KSIZE_FULL, &
                     KLUOUT,XX_OUT,XY_OUT,SIZE(G%XLAT),G%CGRID,G%XGRID_PAR)
!
IF (LHOOK) CALL DR_HOOK('PREP_OUTPUT_GRID',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_OUTPUT_GRID
