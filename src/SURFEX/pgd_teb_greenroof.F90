!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GREENROOF (DTCO, UG, U, USS, IO, S, K, DTV, KDIM, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_GREENROOF* monitor for averaging and interpolations of TEB GR physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    C.de Munck & A. Lemonsu        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    07/2011
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGD_GRID,             ONLY : NL
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE
!
USE MODI_PGD_TEB_GREENROOF_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB)          :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    1.      ISBA specific fields for green roofs
!             ------------------------------------
!
! for green roofs, CISBA = DIF / CSCOND = 'DEF '
IO%CISBA  = 'DIF'
IO%CSCOND = 'PL98 ' ! CSCOND_GR = 'PL98' !begin test 29092011 ! normalement pas besoin
IO%CHORT  = 'DEF '
IO%CKSAT  = 'DEF '
IO%LSOC   = .FALSE.
IO%LTR_ML = .FALSE.
!
ALLOCATE(K%XRUNOFFB(KDIM))
ALLOCATE(K%XWDRAIN (KDIM))
!
K%XRUNOFFB(:) = 0.5 
K%XWDRAIN (:) = 0.0
!
DTV%NTIME = 12
!
IO%LPAR = .TRUE.
!
CALL PGD_TEB_GREENROOF_PAR(DTCO, DTV, UG, U, USS, IO, S, K, KDIM, HPROGRAM)
!
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_GREENROOF
