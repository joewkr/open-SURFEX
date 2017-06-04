!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_END
!########################
!
!!****  *SFX_OASIS_END* - end coupling SFX - OASIS and XIOS
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!      S.Sénési    08/2015 : add XIOS_FINALIZE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef CPLOASIS
USE MOD_OASIS
USE MODD_SFX_OASIS, ONLY : LOASIS
#endif
!
#ifdef WXIOS
USE MODD_XIOS , ONLY : LXIOS 
USE XIOS, ONLY : XIOS_FINALIZE
#endif
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: IERR   ! Error info
!
!-------------------------------------------------------------------------------
!
#ifdef WXIOS
IF (LXIOS) THEN 
! XIOS will finalize Oasis if needed
   CALL XIOS_FINALIZE() 
ENDIF
#endif
!
#ifdef CPLOASIS
IF(LOASIS) THEN
#ifdef WXIOS
  IF (.NOT. LXIOS)THEN !Same test than in offline.F90 because use for Arpege-Aladin-Arome
#endif
    CALL OASIS_TERMINATE(IERR)
    IF (IERR/=OASIS_OK) THEN
      WRITE(*,'(A)'   )'Error OASIS terminate'
      WRITE(*,'(A,I4)')'Return code from oasis_terminate : ',IERR
      CALL ABOR1_SFX("SFX_OASIS_END: Error OASIS terminate")
    ENDIF
#ifdef WXIOS
  ENDIF
#endif
ENDIF
#endif
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_END
