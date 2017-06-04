!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_CARTESIAN(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PREP,           ONLY : XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT, LINTERP
USE MODD_GRID_CARTESIAN, ONLY : XX, XY, NX, NY, XCX, XCY, NCIJ
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_BILIN_VALUE
USE MODI_BILIN_EXTRAP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CARTESIAN',0,ZHOOK_HANDLE)
!
!*      4.    Interpolation with bilinear
!
CALL BILIN_VALUE(KLUOUT,NX,NY,PFIELDIN,XCX,XCY,NCIJ(:,1),NCIJ(:,2),PFIELDOUT)
CALL BILIN_EXTRAP(KLUOUT,NX,NY,NCIJ,XX,XY,PFIELDIN,XX_OUT,XY_OUT,PFIELDOUT,LINTERP)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CARTESIAN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_CARTESIAN
