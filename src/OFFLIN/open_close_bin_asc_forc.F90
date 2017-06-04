!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE OPEN_CLOSE_BIN_ASC_FORC(HACTION,HFORCING,HACTION2)
!     ################################################################
!
!!****  *OPEN_CLOSE_BIN_ASC_FORC* - routine to open and close atmospheric forcing files
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
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2008
!!      Modified by P. Le Moigne 07/2008: HACTION2 added
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_IO_SURF_ASC,ONLY : NNI_FORC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION  ! action to do
 CHARACTER(LEN=6),  INTENT(IN)  :: HFORCING ! forcing file type
 CHARACTER(LEN=1),  INTENT(IN)  :: HACTION2 ! 'R': read, 'W': write
 CHARACTER(LEN=7)               :: YSTATUS  ! file status (OLD/NEW)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: INI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_CLOSE_BIN_ASC_FORC',0,ZHOOK_HANDLE)
!
IF (HACTION2=='R') THEN
   YSTATUS='UNKNOWN'
ELSE IF (HACTION2=='W') THEN
   YSTATUS='NEW    '
ELSE
   CALL ABOR1_SFX('OPEN_CLOSE_BIN_ASC_FORC: UNKNOWN FILE STATUS, '//YSTATUS)      
ENDIF
!      
IF (HACTION=='CONF ') THEN
  IF (NRANK==NPIO) OPEN(UNIT=21,FILE='Params_config.txt',FORM='FORMATTED',STATUS=YSTATUS)
  IF (LHOOK) CALL DR_HOOK('OPEN_CLOSE_BIN_ASC_FORC',1,ZHOOK_HANDLE)
  RETURN
END IF
!
IF (HACTION=='OPEN ') THEN
  IF (HFORCING=='ASCII ') THEN
    IF (NRANK==NPIO) THEN
      OPEN(UNIT=22,FILE='Forc_TA.txt      ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=23,FILE='Forc_QA.txt      ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=24,FILE='Forc_WIND.txt    ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=25,FILE='Forc_LW.txt      ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=26,FILE='Forc_DIR_SW.txt  ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=27,FILE='Forc_SCA_SW.txt  ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=28,FILE='Forc_RAIN.txt    ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=29,FILE='Forc_SNOW.txt    ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=30,FILE='Forc_PS.txt      ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=31,FILE='Forc_DIR.txt     ',FORM='FORMATTED',STATUS=YSTATUS)
      OPEN(UNIT=32,FILE='Forc_CO2.txt     ',FORM='FORMATTED',STATUS=YSTATUS)
    ENDIF
  ELSE IF (HFORCING=='BINARY') THEN
    IF (NRANK==NPIO) THEN   
      OPEN(UNIT=22,FILE='Forc_TA.bin      ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=23,FILE='Forc_QA.bin      ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=24,FILE='Forc_WIND.bin    ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=25,FILE='Forc_LW.bin      ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=26,FILE='Forc_DIR_SW.bin  ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=27,FILE='Forc_SCA_SW.bin  ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=28,FILE='Forc_RAIN.bin    ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=29,FILE='Forc_SNOW.bin    ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=30,FILE='Forc_PS.bin      ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=31,FILE='Forc_DIR.bin     ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
      OPEN(UNIT=32,FILE='Forc_CO2.bin     ',FORM='UNFORMATTED',STATUS=YSTATUS,ACCESS='DIRECT',RECL=NNI_FORC*4)
    ENDIF
  ENDIF
ENDIF
!
IF (HACTION=='CLOSE') THEN
  IF (NRANK==NPIO) THEN
    CLOSE(21)
    CLOSE(22)
    CLOSE(23)
    CLOSE(24)
    CLOSE(25)
    CLOSE(26)
    CLOSE(27)
    CLOSE(28)
    CLOSE(29)
    CLOSE(30)
    CLOSE(31)
    CLOSE(32)
  ENDIF
END IF
IF (LHOOK) CALL DR_HOOK('OPEN_CLOSE_BIN_ASC_FORC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_CLOSE_BIN_ASC_FORC
