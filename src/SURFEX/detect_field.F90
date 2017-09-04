!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DETECT_FIELD(HPROGRAM,PFIELD,OITSHERE)
!     ################################################
!
!!****  *DETECT_FIELD* - generic routine to check is a field is non-zero
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!
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
!!
!!    AUTHOR
!!    ------
!!      S.Malardel       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      02/2003
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
#ifdef SFX_MNH
USE MODI_DETECT_FIELD_MNH
#endif
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "detect_field_aro.h"
#endif
!
!*       0.1   declarations of arguments
!
 CHARACTER (LEN=6),  INTENT(IN) :: HPROGRAM   ! program
!
REAL, DIMENSION(:,:), INTENT(IN)::PFIELD ! array containing the data field
!
LOGICAL    , INTENT(OUT)         :: OITSHERE  ! T --> PFIELD is non zero somewhere
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   declarations of local variables
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DETECT_FIELD',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
!
#ifdef SFX_MNH
  CALL DETECT_FIELD_MNH(HPROGRAM,SIZE(PFIELD,1),SIZE(PFIELD,2),PFIELD,OITSHERE)
#endif
!
ELSEIF (HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. HPROGRAM=='TEXTE ' &
   .OR. HPROGRAM=='FA    ' .OR. HPROGRAM=='BINARY' .OR. HPROGRAM=='LFI   ' &
   .OR. HPROGRAM=='NC    ' ) THEN
!
  OITSHERE = .TRUE.
!
ELSEIF (HPROGRAM=='AROME ') THEN
!
#ifdef SFX_ARO
  CALL DETECT_FIELD_ARO(HPROGRAM,SIZE(PFIELD,1),SIZE(PFIELD,2),PFIELD,OITSHERE)
#endif
!
ENDIF
IF (LHOOK) CALL DR_HOOK('DETECT_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DETECT_FIELD
