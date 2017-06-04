!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE ZOOM_PGD_SEAFLUX (DTCO, DTS, SG, S, UG, U, GCP, &
                                   HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE)
!     ##############################################################
!
!!**** *PGD_SEAFLUX* monitor for averaging and interpolations of SEAFLUX physiographic fields
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
!!    P. Le Moigne     Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!!    G. TANGUY   03/2009 : add reading and interpolation of XDATA_SST and 
!!                          TDATA_SST in the case LDATA_SST=T
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
USE MODD_PREP,             ONLY : CINGRID_TYPE, CINTERP_TYPE, LINTERP
!
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PREP_GRID_EXTERN
USE MODI_PREP_OUTPUT_GRID
USE MODI_READ_SURF
USE MODI_HOR_INTERPOL
USE MODI_READ_PGD_SEAFLUX_PAR_n
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_CLEAN_PREP_OUTPUT_GRID
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(GRID_t), INTENT(INOUT) :: SG
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! Type of program
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZSEABATHY, ZWORK
INTEGER :: ILUOUT
INTEGER :: INI
INTEGER :: IRESP
INTEGER           :: JTIME          ! loop index
INTEGER           :: IVERSION, IBUGFIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_SEAFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
!
 CALL OPEN_AUX_IO_SURF(HINIFILE,HINIFILETYPE,'FULL  ')
!
!-------------------------------------------------------------------------------
!
!*    5.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'SEA   ',SG%NDIM)
!
ALLOCATE(S%LCOVER     (JPCOVER))
ALLOCATE(S%XZS        (SG%NDIM))
ALLOCATE(SG%XLAT       (SG%NDIM))
ALLOCATE(SG%XLON       (SG%NDIM))
ALLOCATE(SG%XMESH_SIZE (SG%NDIM))
!
 CALL PACK_PGD(DTCO, U, HPROGRAM, 'SEA   ', SG, S%LCOVER,  S%XCOVER, S%XZS )  
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL PREP_OUTPUT_GRID(UG%G, SG, U%NSIZE_FULL, ILUOUT)
!
 CALL PREP_GRID_EXTERN(GCP, HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
!* mask where interpolations must be done
!
LINTERP(:) = .TRUE.
!
!------------------------------------------------------------------------------
!
!*      3.     Reading of fields
!              -----------------
!
ALLOCATE(ZSEABATHY(INI,1))
 CALL READ_SURF(HPROGRAM,'BATHY',ZSEABATHY(:,1),IRESP,HDIR='A')
!
ALLOCATE(ZWORK(SG%NDIM,1))
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZSEABATHY(:,1:1),ZWORK(:,1:1)) 
ALLOCATE(S%XSEABATHY (SG%NDIM))
S%XSEABATHY(:) = ZWORK(:,1)
DEALLOCATE(ZSEABATHY,ZWORK)
!
!============================================================
! G. TANGUY 03/2009
! reading of fields for SST_DATA
 CALL READ_SURF(HPROGRAM,'SST_DATA',DTS%LSST_DATA,IRESP)
!
IF (DTS%LSST_DATA) &
  CALL READ_PGD_SEAFLUX_PAR_n(DTCO, U, GCP, DTS, SG%NDIM, HPROGRAM,INI,HDIR='A')
!
!============================================================
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
!============================================================
!
 CALL CLEAN_PREP_OUTPUT_GRID
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_SEAFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZOOM_PGD_SEAFLUX
