!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_SEAFLUX_n (HSELECT, DTS, G, S, HPROGRAM)
!     ###################################################
!
!!****  *WRITE_SEAFLUX_n* - writes SEAFLUX fields
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
 !!      M. Moge     02/2015 parallelization using WRITE_LCOVER
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODE_WRITE_SURF_COV, ONLY : WRITE_SURF_COV
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
USE MODI_WRITESURF_PGD_SEAF_PAR_n
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
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
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
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_SEAFLUX_N',0,ZHOOK_HANDLE)
!
 CALL WRITE_LCOVER(HSELECT,HPROGRAM,S%LCOVER)
!
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,S%XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* bathymetry
!
YRECFM='BATHY'
YCOMMENT='BATHY'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,S%XSEABATHY(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(HSELECT, HPROGRAM,G%CGRID,G%XGRID_PAR,G%XLAT,G%XLON,G%XMESH_SIZE,IRESP)
!
!* sst
!
YRECFM='SST_DATA'
YCOMMENT='(LOGICAL)'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DTS%LSST_DATA,IRESP,HCOMMENT=YCOMMENT)
!
IF (DTS%LSST_DATA) CALL WRITESURF_PGD_SEAF_PAR_n(HSELECT, DTS, HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_SEAFLUX_n
