!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_SEAFLUX_FIELD (DTCO, UG, U, GCP, DTS, O, OR, KLAT, S, &
                                   HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_HOR_SEAFLUX_FIELD* - reads, interpolates and prepares a sea field
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      P. Le Moigne 09/2007, sst from clim
!!      S. Senesi    09/2013, extends to fields of SSS and SIC
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, XLAT_OUT, XLON_OUT, &
                                XX_OUT, XY_OUT, CMASK
!
USE MODD_GRID_GRIB, ONLY : CINMODEL
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODI_PREP_GRIB_GRID
USE MODI_READ_PREP_SEAFLUX_CONF
USE MODI_PREP_SEAFLUX_GRIB
USE MODI_PREP_SEAFLUX_UNIF
USE MODI_PREP_SEAFLUX_BUFFER
USE MODI_PREP_SEAFLUX_NETCDF
USE MODI_HOR_INTERPOL
USE MODI_GET_LUOUT
USE MODI_PREP_SEAFLUX_EXTERN
USE MODI_PREP_SST_INIT
!
USE MODI_PREP_HOR_OCEAN_FIELDS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(OCEAN_REL_t), INTENT(INOUT) :: OR
INTEGER, INTENT(IN) :: KLAT
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
TYPE (DATE_TIME) :: TZTIME_GRIB    ! current date and time
INTEGER  :: ILUOUT    ! output listing logical unit
INTEGER :: INFOMPI, INL
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
 CHARACTER (LEN=28)            :: CLFILE
INTEGER                       :: IRESP
 CHARACTER (LEN=100)           :: CLCOMMENT
 CHARACTER (LEN=6)             :: CLSCHEME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SEAFLUX_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_SEAFLUX_CONF(O%LMERCATOR, &
                             HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                            HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
 CMASK = 'SEA'
!
NULLIFY (ZFIELDIN, ZFIELDOUT)
!
IF (YDCTL%LPART1) THEN
!
!--------------------------------------------------------------------- ----------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
  IF (GUNIF) THEN
    CALL PREP_SEAFLUX_UNIF(ILUOUT,HSURF,ZFIELDIN)
  ELSE IF (YFILETYPE=='GRIB  ') THEN
    CALL PREP_GRIB_GRID(YFILE,ILUOUT,CINMODEL,CINGRID_TYPE,CINTERP_TYPE,TZTIME_GRIB)
    IF (NRANK==NPIO) CALL PREP_SEAFLUX_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '&
          .OR. YFILETYPE=='FA    '.OR. YFILETYPE=='AROME ') THEN
    CALL PREP_SEAFLUX_EXTERN(GCP,HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='BUFFER') THEN
    CALL PREP_SEAFLUX_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='NETCDF') THEN
    CALL PREP_SEAFLUX_NETCDF(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_SEAFLUX_FIELD: data file type not supported : '//YFILETYPE)
  END IF
!
ENDIF
!
!
!*      4.     Horizontal interpolation
!
 CALL PREP_CTL_INT_PART2 (YDCTL, HSURF, 'SEA', CMASK, ZFIELDIN)
!
IF (YDCTL%LPART3) THEN
!
  IF (NRANK==NPIO) THEN
    INL = SIZE(ZFIELDIN,2)
  ELSEIF (.NOT.ASSOCIATED(ZFIELDIN)) THEN
   ALLOCATE(ZFIELDIN(0,0))
  ENDIF
!
  IF (NPROC>1) THEN
#ifdef SFX_MPI
    CALL MPI_BCAST(INL,KIND(INL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
  ENDIF
  ALLOCATE(ZFIELDOUT(KLAT,INL))
!
  CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZFIELDIN,ZFIELDOUT)
!
ENDIF
!
 CALL PREP_CTL_INT_PART4 (YDCTL, HSURF, 'SEA', CMASK, ZFIELDIN, ZFIELDOUT)
!
IF (YDCTL%LPART5) THEN
!
!*      5.     Return to historical variable
!
  SELECT CASE (HSURF)
  CASE('ZS     ')
    ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
    XZS_LS(:) = ZFIELDOUT(:,1)
  CASE('SST    ')
    ALLOCATE(S%XSST(SIZE(ZFIELDOUT,1)))
    S%XSST(:) = ZFIELDOUT(:,1)
    IF (DTS%LSST_DATA) THEN
      ! XSST is derived from array XDATA_SST from MODD_DATA_SEAFLUX, with time interpolation
      CALL PREP_SST_INIT(DTS, S%TTIME, S%JSX, S%XSST)
    END IF
    IF (O%LMERCATOR) THEN
      ! Preparing input for ocean 1D model
      CALL PREP_HOR_OCEAN_FIELDS(DTCO, UG, U, GCP, O, OR, KLAT, S%XSEABATHY, &
                                 HPROGRAM,HSURF,YFILE,YFILETYPE,ILUOUT,GUNIF)
    ENDIF
  CASE('SSS    ')
    ALLOCATE(S%XSSS(SIZE(ZFIELDOUT,1)))
    S%XSSS(:) = ZFIELDOUT(:,1)
  CASE('SIC    ')
    ALLOCATE(S%XSIC(SIZE(ZFIELDOUT,1)))
    S%XSIC(:) = ZFIELDOUT(:,1)
  END SELECT
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Deallocations
!
IF (ASSOCIATED (ZFIELDIN)) DEALLOCATE(ZFIELDIN )
IF (ASSOCIATED (ZFIELDOUT)) DEALLOCATE(ZFIELDOUT)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SEAFLUX_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SEAFLUX_FIELD
