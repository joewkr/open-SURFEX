!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_FLAKE_CONF_n (CHF, DGO, DMF, F, HPROGRAM)
!     #############################################################
!
!!****  *READ_FLAKE_CONF* - reads the configuration for FLAKE
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
!!      Modified    04/2013, P. Le Moigne: FLake chemistry
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
!
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
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
USE MODN_FLAKE_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_FLAKE_t), INTENT(INOUT) :: CHF
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
TYPE(FLAKE_t), INTENT(INOUT) :: F
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
INTEGER           :: ILU
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
!
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_FLAKEn(F)
 CALL INIT_NAM_DIAG_SURFn(DGO)
 CALL INIT_NAM_DIAG_FLAKEn(DMF)
 CALL INIT_NAM_CH_FLAKEn(CHF)
ENDIF
!
IF (LNAM_READ) THEN
        
 !* open namelist file
 !
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
 !
 !* reading of namelist
 !  -------------------
 !
 CALL POSNAM(INAM,'NAM_FLAKEN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_FLAKEn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW_FLK',CSNOW_FLK,'DEF')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CFLK_FLUX',CFLK_FLUX,'DEF  ','FLAKE')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CFLK_ALB',CFLK_ALB,'UNIF','TA96','MK10')
 !
 CALL POSNAM(INAM,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURFn)
 CALL POSNAM(INAM,'NAM_DIAG_FLAKEN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_FLAKEn)
 IF (LWATER_PROFILE .AND. count (XZWAT_PROFILE /= XUNDEF) == 0) &
        CALL ABOR1_SFX("XZWAT_PROFILE MUST BE DEFINED IN NAMELIST NAM_DIAG_FLAKEN IF LWATER_PROFILE=T")
 CALL POSNAM(INAM,'NAM_CH_FLAKEN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_FLAKEn)
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCH_DRY_DEP',CCH_DRY_DEP,'      ','WES89 ','NONE  ')
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_FLAKEn(F)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_DIAG_FLAKEn(DMF)
 CALL UPDATE_NAM_CH_FLAKEn(CHF)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
XTSTEP = XUNDEF
IF (LHOOK) CALL DR_HOOK('READ_FLAKE_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_FLAKE_CONF_n
