!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_DEFAULT_NAM_n(HPROGRAM,HACTION,KLUDES,ONAM_WRITTEN)
!     #######################################################
!
!!****  *GET_DEFAULT_NAM* - routine to open a namelist file with new defaults in it
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2003
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef SFX_MNH
USE MODI_MNHGET_DESFM_n
#endif
!
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM
 CHARACTER(LEN=5), INTENT(IN)  :: HACTION ! 'READ ', 'WRITE'
INTEGER, INTENT(OUT) :: KLUDES ! logical unit of .des file
LOGICAL, INTENT(INOUT), OPTIONAL :: ONAM_WRITTEN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL :: GNAM_WRITTEN
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_DEFAULT_NAM_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHGET_DESFM_n(HACTION,KLUDES)
  IF (HACTION=='READ ' .AND. KLUDES.NE.0) REWIND(KLUDES)
#endif
ELSEIF (HPROGRAM=='AROME ') THEN
  GNAM_WRITTEN = .TRUE.
  IF (PRESENT(ONAM_WRITTEN)) GNAM_WRITTEN = ONAM_WRITTEN
#ifdef SFX_ARO
  CALL AROGET_DESFM_n(HACTION,KLUDES)
#endif
  IF (HACTION=='WRITE') THEN
    IF (GNAM_WRITTEN) THEN
      IF (PRESENT(ONAM_WRITTEN)) ONAM_WRITTEN = .FALSE.
    ELSE
      KLUDES = 0
    ENDIF
  ENDIF
ELSE
  GNAM_WRITTEN = .TRUE.
  IF (PRESENT(ONAM_WRITTEN)) GNAM_WRITTEN = ONAM_WRITTEN
  KLUDES = 0
  IF (HACTION=='WRITE' .AND. GNAM_WRITTEN) THEN
     CALL GET_LUOUT(HPROGRAM,KLUDES)
     IF (PRESENT(ONAM_WRITTEN)) ONAM_WRITTEN = .FALSE.
  ENDIF
END IF
IF (LHOOK) CALL DR_HOOK('GET_DEFAULT_NAM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_DEFAULT_NAM_n
