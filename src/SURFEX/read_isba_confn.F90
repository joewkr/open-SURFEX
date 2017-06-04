!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_CONF_n (CHI, DE, DGO, DMI, IO, HPROGRAM)
!     #######################################################
!
!!****  *READ_ISBA_CONF* - routine to read the configuration for ISBA
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
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Le Moigne 09/2005 CSNOWRES option
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation
!!      Modified by P. Le Moigne (05/2008): deep soil characteristics
!!      Modified by R. El Khatib 05-Apr-2012 Fix message handling
!!      Modified by C. Carmagnola (01/2013): CSNOWMETAMO = 'B92','C13','F06','T07'
!!      B. Decharme  04/2013 delete CTOPREG
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
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
USE MODN_ISBA_n
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DE
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMI
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
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
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_ISBAn(IO)
 CALL INIT_NAM_ISBA_AGSn(IO)
 CALL INIT_NAM_SGH_ISBAn(IO)
 CALL INIT_NAM_DIAG_ISBAn(DE, DGO, DMI)
 CALL INIT_NAM_DIAG_SURFn(DGO)
 CALL INIT_NAM_CH_CONTROLn(CHI)
 CALL INIT_NAM_CH_ISBAn(CHI)
 CALL INIT_NAM_SPINUP_CARB_ISBAn(IO)
 CALL INIT_NAM_ISBA_SNOWn(IO) 
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
 CALL POSNAM(INAM,'NAM_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_ISBAn)
 CALL POSNAM(INAM,'NAM_ISBA_AGSN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_ISBA_AGSn) 
 CALL POSNAM(INAM,'NAM_SGH_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_SGH_ISBAn)
 CALL POSNAM(INAM,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURFn)
 CALL POSNAM(INAM,'NAM_DIAG_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_ISBAn)
 CALL POSNAM(INAM,'NAM_CH_CONTROLN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_CONTROLn)
 CALL POSNAM(INAM,'NAM_CH_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_ISBAn)
  CALL POSNAM(INAM,'NAM_SPINUP_CARBN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_SPINUP_CARBn)
 CALL POSNAM(INAM,'NAM_ISBA_SNOWN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_ISBA_SNOWn) 
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSCOND',CSCOND,'NP89','PL98')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CC1DRY',CC1DRY,'DEF ','GB93')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSOILFRZ',CSOILFRZ,'DEF','LWT')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CDIFSFCOND',CDIFSFCOND,'DEF ','MLCH')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOWRES',CSNOWRES,'DEF','RIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCPSURF',CCPSURF,'DRY','HUM')
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CRUNOFF',CRUNOFF,'WSAT','DT92','SGH ','TOPD')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CKSAT',CKSAT,'DEF','SGH','EXP')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CRAIN',CRAIN,'DEF','SGH')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHORT',CHORT,'DEF','SGH')
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCH_DRY_DEP',CCH_DRY_DEP,'      ','WES89 ','NONE  ')
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOWMETAMO',CSNOWMETAMO,'B92','C13','F06','T07')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOWRAD',CSNOWRAD,'B92','TAR','TA1','TA2')
 ! 
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_ISBAn(IO)
 CALL UPDATE_NAM_ISBA_AGSn(IO)
 CALL UPDATE_NAM_SGH_ISBAn(IO)
 CALL UPDATE_NAM_DIAG_ISBAn(DE, DGO, DMI)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_CH_CONTROLn(CHI)
 CALL UPDATE_NAM_CH_ISBAn(CHI)
 CALL UPDATE_NAM_SPINUP_CARB_ISBAn(IO)
 CALL UPDATE_NAM_ISBA_SNOWn(IO) 
ENDIF
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
!XTSTEP = XUNDEF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CONF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_CONF_n
