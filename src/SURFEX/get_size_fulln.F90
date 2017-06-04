!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SIZE_FULL_n (HPROGRAM,KDIM_FULL,KSIZE_FULL_IN,KSIZE_FULL_OUT)
!     #######################################################
!
!!****  *GET_SIZE_FULL_n* - get number of points for this proc
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
!!      Original    05/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NSIZE_TASK, NRANK, NPROC
!
USE MODD_SURF_PAR,   ONLY : NUNDEF
!
!
#ifdef SFX_MNH
USE MODI_MNHGET_SIZE_FULL_n
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "aroget_size_full_n.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER         ,  INTENT(IN)  :: KDIM_FULL  ! total number of points
INTEGER         ,  INTENT(IN) :: KSIZE_FULL_IN ! total number of points on this proc
INTEGER         ,  INTENT(OUT) :: KSIZE_FULL_OUT ! total number of points on this proc
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_SIZE_FULL_N',0,ZHOOK_HANDLE)
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL MNHGET_SIZE_FULL_n(HPROGRAM,KDIM_FULL,KSIZE_FULL_OUT)
#endif
END IF
!
IF ( HPROGRAM=='OFFLIN' .OR. HPROGRAM=='ASCII ' .OR. HPROGRAM=='FA    ' .OR. HPROGRAM=='LFI   ' .OR. &
     HPROGRAM=='TEXTE ' .OR. HPROGRAM=='BINARY' .OR. HPROGRAM=='NC    ') THEN
#ifdef SFX_OL
  IF (KSIZE_FULL_IN/=NUNDEF .AND. KSIZE_FULL_IN/=0) THEN
    KSIZE_FULL_OUT = KSIZE_FULL_IN
  ELSEIF (ALLOCATED(NSIZE_TASK)) THEN
    KSIZE_FULL_OUT = NSIZE_TASK(NRANK)
  ELSE
    KSIZE_FULL_OUT = KDIM_FULL
  END IF
#endif
ENDIF
!
IF (HPROGRAM=='AROME ') THEN
#ifdef SFX_ARO
  CALL AROGET_SIZE_FULL_n(HPROGRAM,KDIM_FULL,KSIZE_FULL_OUT)
#endif
ENDIF
IF (LHOOK) CALL DR_HOOK('GET_SIZE_FULL_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SIZE_FULL_n
