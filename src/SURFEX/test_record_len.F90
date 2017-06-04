!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#################################################
SUBROUTINE TEST_RECORD_LEN (HPROGRAM,HREC,HSELECT,ONOWRITE)
!#################################################
!
!!
!!    MODIFICATIONS
!!    -------------
!!      B. Decharme 07/2013 write 'time' in netcdf output files
!-------------------------------------------------------------------------------
!
USE MODI_GET_LUOUT
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
USE MODD_XIOS, ONLY : LXIOS, LXIOS_DEF_CLOSED
#ifdef WXIOS
USE XIOS, ONLY      : XIOS_IS_VALID_FIELD, XIOS_FIELD_IS_ACTIVE
#endif
!
USE MODD_WRITE_SURF_ATM, ONLY : LFIRST_WRITE, LNOWRITE, NCPT_WRITE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be written
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
LOGICAL,            INTENT(OUT) :: ONOWRITE ! flag for article to be written
!
 CHARACTER(LEN=12) :: YREC
INTEGER :: IFIELD,JFIELD
INTEGER :: ILUOUT  ! listing logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TEST_RECORD_LEN',0,ZHOOK_HANDLE)
!
IF (TRIM(HREC)=="time".OR.TRIM(HREC)=="longitude".OR.TRIM(HREC)=="latitude") THEN
  ONOWRITE = .FALSE.
  IF (LHOOK) CALL DR_HOOK('TEST_RECORD_LEN',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
NCPT_WRITE = NCPT_WRITE + 1
!
IF (LFIRST_WRITE) THEN
  !
#ifdef WXIOS
  IF (LXIOS .AND. (TRIM(HPROGRAM)=='XIOS' )) THEN
    !
    IF (LXIOS_DEF_CLOSED) THEN 
      IF (XIOS_IS_VALID_FIELD(HREC)) THEN
        ONOWRITE = .NOT.XIOS_FIELD_IS_ACTIVE(HREC)
      ELSE
        ONOWRITE = .TRUE.
      ENDIF
    ELSE
      ONOWRITE = .FALSE.
    ENDIF
    !
    IF (ONOWRITE) THEN
      LNOWRITE(NCPT_WRITE) = ONOWRITE
      IF (LHOOK) CALL DR_HOOK('TEST_RECORD_LEN',1,ZHOOK_HANDLE)
      RETURN
    ENDIF
    !
  ENDIF
#endif
  !
  IF (LEN_TRIM(HREC)>12) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*) '----------------------------------------------'
    WRITE(ILUOUT,*) 'Error occured when writing a field            '
    WRITE(ILUOUT,*) 'The name of the field is too long             '
    WRITE(ILUOUT,*) 'The name must not be longer than 12 characters'
    WRITE(ILUOUT,*) 'Please shorten the name of your field         '
    WRITE(ILUOUT,FMT='(A32,A12,A1)') ' The field name currently is : "',HREC,'"'
    WRITE(ILUOUT,*) '----------------------------------------------'
    CALL ABOR1_SFX('TEST_RECORD_LEN: FIELD NAME TOO LONG --> '//HREC)
  END IF
  !
  YREC = HREC
  SELECT CASE(HREC(1:4))
    CASE("TEB1","TEB2","TEB3","TEB4","TEB5","TEB6","TEB7","TEB8","TEB9")
      YREC=HREC(6:LEN(HREC))
  END SELECT
  !
  ! if output fields selection is active, test if this field is to be written
  IF (SIZE(HSELECT)>0)  THEN
     IFIELD=COUNT(HSELECT /= '            ')
     ONOWRITE=.TRUE.
     DO JFIELD=1,IFIELD
        IF ( TRIM(HSELECT(JFIELD))==TRIM(YREC) ) THEN
          ONOWRITE=.FALSE.
        ENDIF
     ENDDO
     !special case for netcdf output
     IF(TRIM(YREC)=='time')ONOWRITE=.FALSE.
  ELSE
     ONOWRITE=.FALSE.
  ENDIF
  !
  LNOWRITE(NCPT_WRITE) = ONOWRITE
  !
ELSE
  !
  ONOWRITE = LNOWRITE(NCPT_WRITE)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TEST_RECORD_LEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TEST_RECORD_LEN
