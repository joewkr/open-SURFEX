!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_WATFLUX_CONF_n (CHW, DGO, W, HPROGRAM)
!     #############################################################
!
!!****  *READ_WATFLUX_CONF* - reads the configuration for WATFLUX
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
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
!
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODN_WATFLUX_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
INTEGER           :: IMI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
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
 !* open namelist file
 !
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(INAM,'NAM_WATFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_WATFLUXn)
 CALL POSNAM(INAM,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURFn)
 CALL POSNAM(INAM,'NAM_CH_WATFLUXN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_WATFLUXn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CWAT_ALB',CWAT_ALB,'UNIF','TA96')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCH_DRY_DEP',CCH_DRY_DEP,'      ','WES89 ','NONE  ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CINTERPOL_TS',CINTERPOL_TS,'QUADRA','LINEAR','UNIF  ','NONE  ')
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_WATFLUXn(W)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_CH_WATFLUXn(CHW)
 ENDIF
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
XTSTEP = XUNDEF
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_WATFLUX_CONF_n
