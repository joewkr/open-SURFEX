!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_READ_PREP_FILE_DATE
CONTAINS
SUBROUTINE READ_PREP_FILE_DATE (HPROGRAM,HFILE,HFILETYPE,TPTIME,KLUOUT)
!     #################################################################################
!
!!****  *READ_PREP_FILE_DATE* - reads the date for the surface
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
!!------------------------------------------------------------------
!
!
USE MODD_IO_BUFF, ONLY : CREC, NREC
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRIB_GRID
USE MODI_READ_BUFFER
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
TYPE (DATE_TIME),   INTENT(OUT) :: TPTIME    ! grib date and time
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=12), DIMENSION(SIZE(CREC)) :: HREC   ! list of records already read/written
INTEGER                            :: IREC
 CHARACTER(LEN=6)              :: YINMODEL  ! model from which GRIB file originates
 CHARACTER(LEN=6)             :: YINTERPTYPE
 CHARACTER(LEN=10)             :: YGRIDTYPE ! Grid type
INTEGER                       :: IRESP     ! Error code after redding
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading date in a grib file
!              --------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_FILE_DATE',0,ZHOOK_HANDLE)
IF(HFILETYPE=='GRIB  ') THEN
!
  CALL PREP_GRIB_GRID(HFILE,KLUOUT,YINMODEL,YGRIDTYPE,YINTERPTYPE,TPTIME)
!
ELSE IF(HFILETYPE=='MESONH' .OR. HFILETYPE=='LFI   ' .OR. HFILETYPE=='ASCII '.OR. HFILETYPE=='FA    '.OR.&
        HFILETYPE=='NC    ') THEN
!
  HREC = CREC
  IREC = NREC
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
  CALL READ_SURF(HFILETYPE,'DTCUR           ',TPTIME,IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  CREC = HREC
  NREC = IREC
!
ELSE IF(HFILETYPE=='BUFFER') THEN
!
 CALL READ_BUFFER('YEAR  ',TPTIME%TDATE%YEAR,IRESP)
 CALL READ_BUFFER('MONTH ',TPTIME%TDATE%MONTH,IRESP)
 CALL READ_BUFFER('DAY   ',TPTIME%TDATE%DAY,IRESP)
 CALL READ_BUFFER('TIME  ',TPTIME%TIME,IRESP)
!
ELSE
!
  WRITE(UNIT=KLUOUT, FMT=*) 'STOP IN READ_PREP_FILE_DATE'
  WRITE(UNIT=KLUOUT,  FMT='("FILETYPE =",A6,"NOT SUPPORTED")') HFILETYPE
  CALL ABOR1_SFX("READ_PREP_FILE_DATE: FILETYPE NOT SUPPORTED")
!
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_PREP_FILE_DATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE READ_PREP_FILE_DATE
END MODULE MODI_READ_PREP_FILE_DATE
