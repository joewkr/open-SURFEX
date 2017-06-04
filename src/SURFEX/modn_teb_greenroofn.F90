!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_TEB_GREENROOF_n
!     ##################
!
!!****  *MODN_TEB_GREENROOF_n* - declaration of namelist NAM_TEBn
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify the namelist NAM_TEB_GREENROOFn
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    Based on modn_tebn
!!       
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
CHARACTER(LEN=4)  :: CSCOND_GR
CHARACTER(LEN=4)  :: CRUNOFF_GR
CHARACTER(LEN=3)  :: CKSAT_GR
LOGICAL           :: LSOC_GR
CHARACTER(LEN=3)  :: CHORT_GR         
!
NAMELIST/NAM_TEB_GREENROOFn/CRUNOFF_GR,CSCOND_GR,CKSAT_GR,LSOC_GR,CHORT_GR
!
CONTAINS
!
! subroutine INIT !
SUBROUTINE INIT_NAM_TEB_GREENROOFn (TGRO)
!
  USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
  IMPLICIT NONE

!
  TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: TGRO
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:INIT_NAM_TEB_GREENROOFN',0,ZHOOK_HANDLE)
  CSCOND_GR     = TGRO%CSCOND
  CRUNOFF_GR    = TGRO%CRUNOFF
  CKSAT_GR      = TGRO%CKSAT
  LSOC_GR       = TGRO%LSOC
  CHORT_GR      = TGRO%CHORT
IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:INIT_NAM_TEB_GREENROOFN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_TEB_GREENROOFn

! subroutine UPDATE !
SUBROUTINE UPDATE_NAM_TEB_GREENROOFn (TGRO)
!
  USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
  IMPLICIT NONE

!
  TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: TGRO
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:UPDATE_NAM_TEB_GREENROOFN',0,ZHOOK_HANDLE)
  TGRO%CSCOND   = CSCOND_GR
  TGRO%CRUNOFF   = CRUNOFF_GR
  TGRO%CKSAT    = CKSAT_GR
  TGRO%LSOC     = LSOC_GR
  TGRO%CHORT     = CHORT_GR
IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:UPDATE_NAM_TEB_GREENROOFN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_TEB_GREENROOFn

END MODULE MODN_TEB_GREENROOF_n
