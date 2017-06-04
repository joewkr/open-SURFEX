!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_FLAKE_n (HSELECT, G, F, HPROGRAM)
!     ###################################################
!
!!****  *WRITESURF_PGD_FLAKE_n* - writes FLAKE fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!      B. Decharme 07/2011 : delete argument HWRITE
!!      M. Moge     02/2015 parallelization using MPI_ALLREDUCE for mesonh
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODE_WRITE_SURF_COV, ONLY : WRITE_SURF_COV
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
USE MODI_WRITE_LCOVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_FLAKE_N',0,ZHOOK_HANDLE)
!
 CALL WRITE_LCOVER(HSELECT,HPROGRAM,F%LCOVER)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(HSELECT, &
                 HPROGRAM,G%CGRID,G%XGRID_PAR,G%XLAT,G%XLON,G%XMESH_SIZE,IRESP)
!
!* FLake parameters
!
YRECFM='WATER_DEPTH'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XWATER_DEPTH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='WATER_FETCH'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XWATER_FETCH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='T_BS'
YCOMMENT='X_Y_'//YRECFM//' (K)'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XT_BS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='DEPTH_BS'
YCOMMENT='X_Y_'//YRECFM//' (m)'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XDEPTH_BS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='EXTCOEF_WAT'
YCOMMENT='X_Y_'//YRECFM//'    '
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,F%XEXTCOEF_WATER(:),IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_FLAKE_n
