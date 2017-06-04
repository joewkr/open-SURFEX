!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_SEAFLUX_n (DTCO, DTS, SG, S, U, UG, GCP, HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_SEAFLUX_n* - routine to read SEAFLUX physiographic fields
!!
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_PACK_INIT
USE MODI_READ_LCOVER
USE MODI_READ_PGD_SEAFLUX_PAR_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(GRID_t), INTENT(INOUT) :: SG
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER           :: IVERSION   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_SEA'
 CALL GET_TYPE_DIM_n(DTCO, U, 'SEA   ',SG%NDIM)
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(S%LCOVER(JPCOVER))
ALLOCATE(S%XZS(SG%NDIM))
ALLOCATE(SG%XLAT  (SG%NDIM))
ALLOCATE(SG%XLON  (SG%NDIM))
ALLOCATE(SG%XMESH_SIZE (SG%NDIM))
CALL PACK_INIT(DTCO,U,UG,HPROGRAM,'SEA   ',SG,S%LCOVER,S%XCOVER,S%XZS )
!
!* orography
!
S%XZS(:) = 0.
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
!* bathymetry
!
ALLOCATE(S%XSEABATHY(SG%NDIM))
IF (IVERSION<=3) THEN
  S%XSEABATHY(:) = -300.
ELSE
  YRECFM='BATHY'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSEABATHY(:),IRESP)
END IF
!
!* sst
!
!
IF (IVERSION<3) THEN
  DTS%LSST_DATA = .FALSE.
ELSE
  YRECFM='SST_DATA'
  CALL READ_SURF(HPROGRAM,YRECFM,DTS%LSST_DATA,IRESP)
END IF
!
IF (DTS%LSST_DATA) CALL READ_PGD_SEAFLUX_PAR_n(DTCO, U, GCP, DTS, SG%NDIM, HPROGRAM,SG%NDIM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------!
END SUBROUTINE READ_PGD_SEAFLUX_n
