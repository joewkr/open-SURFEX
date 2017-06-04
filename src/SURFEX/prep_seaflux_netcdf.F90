!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_NETCDF(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_SEAFLUX_NETCDF* - prepares SEAFLUX fields from oceanic analyses in NETCDF
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     C. Lebeaupin Brossier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!      Modified    09/2013 : S. Senesi : extends to SSS and SIC fields 
!!------------------------------------------------------------------
!
USE MODE_READ_NETCDF_MERCATOR
!
!USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_GRID_LATLONREGUL,  ONLY : NILENGTH,NINDEPTH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
!TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
!CHARACTER(LEN=6)              :: YINMODEL ! model from which GRIB file originates
REAL, DIMENSION(:),       POINTER :: ZFIELD   ! field read
 CHARACTER(LEN=28) :: YNCVAR
INTEGER :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Grid type
!              ---------
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_NETCDF',0,ZHOOK_HANDLE)
CINGRID_TYPE='LATLON '
!
!*      2.     Reading of field
!              ----------------
!-----------------
SELECT CASE(HSURF)
!-----------------
!
!* 2.1 Orography
!      ---------
!
  CASE('ZS     ')
    YNCVAR='topo'
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    IF (NRANK==NPIO) THEN
      CALL READ_NETCDF_ZS_SEA(HFILE,YNCVAR,ZFIELD)
      ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
      PFIELD(:,1) = ZFIELD(:)
      DEALLOCATE(ZFIELD)
    ELSE
      ALLOCATE(PFIELD(0,0))
    ENDIF
!
!
!* 2.2 Temperature profiles
!      --------------------
!
  CASE('SST    ','SSS    ','SIC    ')
    IF ( HSURF == 'SST    ') THEN
       YNCVAR='temperature'
    ELSE IF ( HSURF == 'SSS    ') THEN
       YNCVAR='sss'
    ELSE IF ( HSURF == 'SIC    ') THEN
       YNCVAR='sic'
    END IF
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    IF (NRANK==NPIO) THEN 
      CALL READ_NETCDF_SST(HFILE,YNCVAR,ZFIELD)
      ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
      PFIELD(:,1) = ZFIELD(:)
      DEALLOCATE(ZFIELD)
    ENDIF
!
END SELECT
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(CINTERP_TYPE,LEN(CINTERP_TYPE),MPI_CHARACTER,NPIO,NCOMM,INFOMPI)
#endif
  IF (TRIM(CINTERP_TYPE)=="UNIF") THEN
    IF (NRANK/=NPIO) ALLOCATE(PFIELD(1,1))
#ifdef SFX_MPI
    CALL MPI_BCAST(PFIELD(1:1,1:1),KIND(PFIELD)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
  ELSEIF (NRANK/=NPIO) THEN
    ALLOCATE(PFIELD(0,0))
  ENDIF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_NETCDF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SEAFLUX_NETCDF
