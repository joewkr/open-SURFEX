!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE ZOOM_PGD_ISBA (CHI, DTCO, DTV, IG, IO, S, K, ISS, UG, U, USS, GCP, &
                                HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE,OECOCLIMAP)
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
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
!!    B. Decharme      2008  XWDRAIN
!!    M.Tomasini    17/04/12  Add interpolation for ISBA variables (MODD_DATA_ISBA_n)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_ZOOM_PGD_ISBA_FULL
USE MODI_GET_AOS_n
USE MODI_GET_SSO_n
USE MODI_PACK_PGD_ISBA
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(SSO_t), INTENT(INOUT) :: ISS
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ISIZE_LMEB_PATCH
INTEGER :: IVERSION, IBUGFIX
INTEGER :: IRESP
INTEGER :: ILUOUT
INTEGER :: IL      ! total 1D dimension (output grid, total surface)
INTEGER :: ILU     ! total 1D dimension (output grid, ISBA points only)
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSIP    ! A/S i+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSIM    ! A/S i- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSJP    ! A/S j+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSJM    ! A/S j- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2IP    ! h/2 i+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2IM    ! h/2 i- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2JP    ! h/2 j+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2JM    ! h/2 j- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZSSO_SLOPE! subgrid slope on all surface points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_ISBA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
 CALL OPEN_AUX_IO_SURF(HINIFILE,HINIFILETYPE,'FULL  ')
!
 CALL READ_SURF(HINIFILETYPE,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HINIFILETYPE,'BUG',IBUGFIX,IRESP) 
 CALL READ_SURF(HINIFILETYPE,'PATCH_NUMBER',IO%NPATCH,IRESP)
!
ALLOCATE(IO%LMEB_PATCH(IO%NPATCH))
!
IF (IVERSION>=8) THEN
  !
  CALL READ_SURF(HINIFILETYPE,'MEB_PATCH',IO%LMEB_PATCH(:),IRESP,HDIR='-')
  ISIZE_LMEB_PATCH = COUNT(IO%LMEB_PATCH(:))
  !
  IF (ISIZE_LMEB_PATCH>0)THEN
    CALL READ_SURF(HINIFILETYPE,'FORC_MEASURE',IO%LFORC_MEASURE,IRESP)
    CALL READ_SURF(HINIFILETYPE,'MEB_LITTER',IO%LMEB_LITTER,IRESP)
    CALL READ_SURF(HINIFILETYPE,'MEB_GNDRES',IO%LMEB_GNDRES,IRESP)
  ELSE      
    IO%LFORC_MEASURE = .FALSE.
    IO%LMEB_LITTER   = .FALSE.
    IO%LMEB_GNDRES   = .FALSE.    
  ENDIF
  !
ELSE
  IO%LMEB_PATCH(:)= .FALSE.
  IO%LFORC_MEASURE= .FALSE.
  IO%LMEB_LITTER  = .FALSE.
  IO%LMEB_GNDRES  = .FALSE.
ENDIF
!
!
 CALL READ_SURF(HINIFILETYPE,'GROUND_LAYER',IO%NGROUND_LAYER,IRESP)
 CALL READ_SURF(HINIFILETYPE,'ISBA',IO%CISBA,IRESP)
IF (IVERSION >= 7) THEN
  CALL READ_SURF(HINIFILETYPE,'PEDOTF',IO%CPEDOTF,IRESP)
ELSE
  IO%CPEDOTF = 'CH78'
ENDIF
 CALL READ_SURF(HINIFILETYPE,'PHOTO',IO%CPHOTO,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  CALL READ_SURF(HINIFILETYPE,'TR_ML',IO%LTR_ML,IRESP)
  !
ELSE 
  IO%LTR_ML = .FALSE.
ENDIF
!
IF(IO%CISBA=='DIF') THEN
  ALLOCATE(IO%XSOILGRID(IO%NGROUND_LAYER))
  IO%XSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    CALL READ_SURF(HINIFILETYPE,'SOILGRID',IO%XSOILGRID,IRESP,HDIR='-')
  ELSE
    IO%XSOILGRID(1:IO%NGROUND_LAYER)=XOPTIMGRID(1:IO%NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(IO%XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
IF (IVERSION>=6) THEN
  CALL READ_SURF(HPROGRAM,'NBIOMASS',IO%NNBIOMASS,IRESP)
ELSE
  SELECT CASE (IO%CPHOTO)
    CASE ('AST')
      IO%NNBIOMASS = 1
    CASE ('NIT')
      IO%NNBIOMASS = 3
    CASE ('NCB')
      IO%NNBIOMASS = 6
  END SELECT
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
!------------------------------------------------------------------------------
IO%LECOCLIMAP = OECOCLIMAP
!
!-------------------------------------------------------------------------------
!
!*    7.      Number of points and packing of general fields
!             ----------------------------------------------
!
!
 CALL GET_SURF_SIZE_n(DTCO, U,'NATURE',ILU)
!
ALLOCATE(S%LCOVER     (JPCOVER))
ALLOCATE(S%XZS        (ILU))
ALLOCATE(IG%XLAT       (ILU))
ALLOCATE(IG%XLON       (ILU))
ALLOCATE(IG%XMESH_SIZE (ILU))
ALLOCATE(ISS%XZ0EFFJPDIR(ILU))
!
 CALL PACK_PGD(DTCO, U,  HPROGRAM, 'NATURE', IG, S%LCOVER, S%XCOVER, S%XZS  )  
!
!------------------------------------------------------------------------------
!
!*      3.     Reading of sand, clay, runoffb, wdrain and interpolations
!              --------------------------------------------------
!
ALLOCATE(K%XSAND(ILU,IO%NGROUND_LAYER))
ALLOCATE(K%XCLAY(ILU,IO%NGROUND_LAYER))
ALLOCATE(K%XRUNOFFB(ILU))
ALLOCATE(K%XWDRAIN (ILU))
 CALL ZOOM_PGD_ISBA_FULL(CHI, DTCO, DTV, IG, IO, S, K, UG, U, GCP, &
                         HPROGRAM,HINIFILE,HINIFILETYPE)
!
!-------------------------------------------------------------------------------
!
!*    8.      Packing of ISBA specific fields
!             -------------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'FULL  ',IL)
!
ALLOCATE(ZAOSIP(IL))
ALLOCATE(ZAOSIM(IL))
ALLOCATE(ZAOSJP(IL))
ALLOCATE(ZAOSJM(IL))
ALLOCATE(ZHO2IP(IL))
ALLOCATE(ZHO2IM(IL))
ALLOCATE(ZHO2JP(IL))
ALLOCATE(ZHO2JM(IL))
ALLOCATE(ZSSO_SLOPE(IL))

 CALL GET_AOS_n(USS,HPROGRAM,IL,ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM)
 CALL GET_SSO_n(USS,HPROGRAM,IL,ZSSO_SLOPE)

 CALL PACK_PGD_ISBA(DTCO, IG%NDIM, ISS, U, HPROGRAM,              &
                     ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,              &
                     ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM,              &
                     ZSSO_SLOPE                                   )  
!
DEALLOCATE(ZAOSIP)
DEALLOCATE(ZAOSIM)
DEALLOCATE(ZAOSJP)
DEALLOCATE(ZAOSJM)
DEALLOCATE(ZHO2IP)
DEALLOCATE(ZHO2IM)
DEALLOCATE(ZHO2JP)
DEALLOCATE(ZHO2JM)
DEALLOCATE(ZSSO_SLOPE)
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZOOM_PGD_ISBA
