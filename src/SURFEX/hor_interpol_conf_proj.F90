!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_CONF_PROJ(GCP,KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
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
!!    MODIFICATION
!!    ------------
!!
!!    02/04/12 M. Tomasini  Add an index in the second dimension of the ISBA 
!!                          array variables for BILIN interpolation routine to 
!!                          not bug in case 2D (this is not the more beautiful
!!                          method; the BILIN routine should better be adapted)
!!                          Search  ! Ajout MT
!!    10/02/15 M.Moge  using SIZE(PFIELDOUT,1) instead of SIZE(XLAT_OUT)
!-------------------------------------------------------------------------------
!
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t,XY,XX, XXI, XYI, &
                                  XCX, XCY, NCIJ
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
USE MODD_PREP,           ONLY : XLAT_OUT, XLON_OUT, LINTERP
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
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_DUPLIQUE ! .true. where physical value is needed ! Ajout MT
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CONF_PROJ',0,ZHOOK_HANDLE)
!
!*      4.    Interpolation with bilinear
!
 CALL BILIN_VALUE(KLUOUT,GCP%NX,GCP%NY,PFIELDIN,XCX,XCY,NCIJ(:,1),NCIJ(:,2),PFIELDOUT)
 CALL BILIN_EXTRAP(KLUOUT,GCP%NX,GCP%NY,NCIJ,XX,XY,PFIELDIN,XXI,XYI,PFIELDOUT,LINTERP)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_CONF_PROJ
