!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DEFAULT_WATFLUX_n (CHW, DGO, W, HPROGRAM)
!     #############################################################
!
!!****  *READ_WATFLUX_CONF* - routine to read the configuration for WATFLUX
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
!
!
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_GET_DEFAULT_NAM_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODN_WATFLUX_n
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
TYPE(CH_WATFLUX_t), INTENT(INOUT) :: CHW
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output listing logical unit
INTEGER           :: ILUDES         ! .des file logical unit
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_WATFLUX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_WATFLUX_N',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_WATFLUXn(W)
 CALL INIT_NAM_DIAG_SURFn(DGO)
 CALL INIT_NAM_CH_WATFLUXn(CHW)
ENDIF
! 
IF (LNAM_READ) THEN
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(ILUDES,'NAM_WATFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_WATFLUXn)
 CALL POSNAM(ILUDES,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_DIAG_SURFn)
 CALL POSNAM(ILUDES,'NAM_CH_WATFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CH_WATFLUXn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CWAT_ALB',CWAT_ALB, 'UNIF','TA96')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CINTERPOL_TS',CINTERPOL_TS,'QUADRA','LINEAR','UNIF  ','NONE  ')
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_WATFLUXn(W)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_CH_WATFLUXn(CHW)
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_WATFLUX_n
