!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DEFAULT_TEB_n (CHT, DMTO, DGO, DUT, IO, T, TOP, HPROGRAM)
!     #######################################################
!
!!****  *READ_TEB_CONF* - routine to read the configuration for TEB
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DIAG_MISC_TEB_OPTIONS_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
USE MODI_GET_LUOUT
USE MODI_GET_DEFAULT_NAM_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODN_TEB_n
USE MODN_TEB_GREENROOF_n
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DMTO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DUT
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output listing logical unit
INTEGER           :: ILUDES         ! .des file logical unit
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)

 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_TEBn(T, TOP)
 CALL INIT_NAM_DIAG_SURFn(DGO, TOP)
 CALL INIT_NAM_DIAG_TEBn(DMTO, DGO, DUT)
 CALL INIT_NAM_CH_TEBn(CHT)
 CALL INIT_NAM_TEB_GREENROOFn(IO)
ENDIF
! 
IF (LNAM_READ) THEN
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(ILUDES,'NAM_TEBN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_TEBn)
 CALL POSNAM(ILUDES,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
 CALL POSNAM(ILUDES,'NAM_DIAG_TEBN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_TEBn)
 CALL POSNAM(ILUDES,'NAM_CH_TEBN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_TEBn)
 CALL POSNAM(ILUDES,'NAM_TEB_GREENROOFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_TEB_GREENROOFn)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_TEBn(T, TOP)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_DIAG_TEBn(DMTO, DGO, DUT)
 CALL UPDATE_NAM_CH_TEBn(CHT)
 CALL UPDATE_NAM_TEB_GREENROOFn(IO)
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_TEB_n
