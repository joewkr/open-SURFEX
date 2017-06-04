!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_SEAFLUX (DTCO, DTS, SG, S, UG, U, USS, &
                              HPROGRAM)
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Lebeaupin-B C. 01/2008 : include bathymetry
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
!
USE MODI_READ_NAM_PGD_SEABATHY
USE MODI_PGD_BATHYFIELD
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PACK_PGD_SEAFLUX
USE MODI_PGD_SEAFLUX_PAR
!
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
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(NL)               :: ZSEABATHY ! bathymetry on all surface points
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YSEABATHY         ! file name for bathymetrie
 CHARACTER(LEN=6)         :: YSEABATHYFILETYPE ! bathymetry data file type
 CHARACTER(LEN=28)        :: YNCVARNAME        ! variable to read in netcdf
                                              ! file
REAL                     :: XUNIF_SEABATHY    ! uniform value of bathymetry
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX',0,ZHOOK_HANDLE)
 CALL READ_NAM_PGD_SEABATHY(HPROGRAM,YSEABATHY,YSEABATHYFILETYPE,YNCVARNAME,&
       XUNIF_SEABATHY)  
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
!-------------------------------------------------------------------------------
!
!*    4.      Bathymetry
!             ----------
!
 CALL PGD_BATHYFIELD(UG, U, USS, HPROGRAM,'bathymetry','SEA',YSEABATHY,YSEABATHYFILETYPE,&
       YNCVARNAME,XUNIF_SEABATHY,ZSEABATHY(:))  
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
 CALL PACK_PGD(DTCO, U, HPROGRAM, 'SEA   ', SG, S%LCOVER, S%XCOVER, S%XZS )  
!
 CALL PACK_PGD_SEAFLUX(DTCO, SG%NDIM, S, U, HPROGRAM, ZSEABATHY)
!
 CALL PGD_SEAFLUX_PAR(DTCO, DTS, SG%NDIM, UG, U, USS, HPROGRAM)
 !
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_SEAFLUX
