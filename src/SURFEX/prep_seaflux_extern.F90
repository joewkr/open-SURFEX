!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_EXTERN (GCP,HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD)
!     #################################################################################
!
!
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
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
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(:), ALLOCATABLE :: ZMASK
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
!
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IVERSION       ! total 1D dimension
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_EXTERN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
 CALL PREP_GRID_EXTERN(GCP,HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
IF (NRANK/=NPIO) INI = 0
!
ALLOCATE(ZMASK(INI))
IF (IVERSION>=7) THEN
  YRECFM='FRAC_SEA'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZMASK,IRESP,HDIR='A')
ELSE
  ZMASK(:) = 1.
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (NRANK/=NPIO) INI = 0
!---------------------------------------------------------------------------------------
SELECT CASE(HSURF)
!---------------------------------------------------------------------------------------
!
!*     3.      Orography
!              ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(INI,1))
    PFIELD(:,:) = 0.
!
!*      4.  Sea surface temperature
!           -----------------------
!
  CASE('SST    ')
    ALLOCATE(PFIELD(INI,1))
    YRECFM='SST'
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'SEA   ')
    CALL READ_SURF(HFILETYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='E')
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    WHERE (ZMASK(:)==0.) PFIELD(:,1) = XUNDEF    
!
!*      5.  Sea surface salinity
!           --------------------
!
  CASE('SSS    ')
    ALLOCATE(PFIELD(INI,1))
    YRECFM='SSS'
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
    CALL READ_SURF(HFILETYPE,'VERSION',IVERSION,IRESP)
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
    IF(IVERSION>=8)THEN
      CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'SEA   ')
      CALL READ_SURF(HFILETYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='E')
      CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
      WHERE (ZMASK(:)==0.) PFIELD(:,1) = XUNDEF      
    ELSE
      PFIELD = 0.0
    ENDIF
!
!*      6.  Sea ice fraction
!           ----------------
!
  CASE('SIC    ')
    ALLOCATE(PFIELD(INI,1))
    PFIELD = 0.0
!
!---------------------------------------------------------------------------------------
END SELECT
!-------------------------------------------------------------------------------------
!
DEALLOCATE(ZMASK)
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SEAFLUX_EXTERN
