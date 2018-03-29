!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PREP_HOR_WATFLUX_FIELD
CONTAINS
SUBROUTINE PREP_HOR_WATFLUX_FIELD (DTCO, U, GCP, KLAT, W, &
                                   HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_HOR_WATFLUX_FIELD* - Reads, interpolates and prepares a water field
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
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODD_PREP,          ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, CMASK
!
USE MODD_GRID_GRIB, ONLY : CINMODEL
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODI_PREP_GRIB_GRID
USE MODI_READ_PREP_WATFLUX_CONF
USE MODI_PREP_WATFLUX_GRIB
USE MODI_PREP_WATFLUX_UNIF
USE MODI_PREP_WATFLUX_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_GET_LUOUT
USE MODI_PREP_WATFLUX_EXTERN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER, INTENT(IN) :: KLAT
TYPE(WATFLUX_t), INTENT(INOUT) :: W
TYPE (PREP_CTL),    INTENT (INOUT) :: YDCTL
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
 TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
INTEGER :: INFOMPI, INL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_WATFLUX_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_WATFLUX_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                            HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'WATER'
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
NULLIFY (ZFIELDIN, ZFIELDOUT)
!
IF (YDCTL%LPART1) THEN
  IF (GUNIF) THEN
    CALL PREP_WATFLUX_UNIF(ILUOUT,HSURF,ZFIELDIN)
  ELSE IF (YFILETYPE=='GRIB  ') THEN
    CALL PREP_GRIB_GRID(YFILE,ILUOUT,CINMODEL,CINGRID_TYPE,CINTERP_TYPE,TZTIME_GRIB)
    IF (NRANK==NPIO) CALL PREP_WATFLUX_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '&
          .OR. YFILETYPE=='FA    '.OR. YFILETYPE=='AROME '.OR.YFILETYPE=='NC    ') THEN
     CALL PREP_WATFLUX_EXTERN(GCP,HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='BUFFER') THEN
     CALL PREP_WATFLUX_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_WATFLUX_FIELD: data file type not supported : '//YFILETYPE)
  END IF
ENDIF
!
!
!*      4.     Horizontal interpolation
!
 CALL PREP_CTL_INT_PART2 (YDCTL, HSURF, 'WATER', CMASK, ZFIELDIN)
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
 CALL PREP_CTL_INT_PART4 (YDCTL, HSURF, 'WATER', CMASK, ZFIELDIN, ZFIELDOUT)
!
IF (YDCTL%LPART5) THEN
!
!*      5.     Return to historical variable
!
  SELECT CASE (HSURF)
    CASE('ZS     ')
      ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
      XZS_LS(:) = ZFIELDOUT(:,1)
    CASE('TSWATER')
      ALLOCATE(W%XTS(SIZE(ZFIELDOUT,1)))
      W%XTS(:) = ZFIELDOUT(:,1)
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
IF (LHOOK) CALL DR_HOOK('PREP_HOR_WATFLUX_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_WATFLUX_FIELD
END MODULE MODI_PREP_HOR_WATFLUX_FIELD
