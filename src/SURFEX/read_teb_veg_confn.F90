!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_VEG_CONF_n (CHT, IO, HPROGRAM)
!     #######################################################
!
!!****  *READ_TEB_VEG_CONF* - routine to read the configuration for VEG
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
!!      B. Decharme 06/2013 delete CTOPREG
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
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
USE MODN_TEB_VEG_n
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
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
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
IF (LHOOK) CALL DR_HOOK('READ_TEB_VEG_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_TEB_VEGn(IO)
 CALL INIT_NAM_TEB_VEG_AGSn(IO)
 CALL INIT_NAM_CH_CONTROLn(CHT)
 CALL INIT_NAM_CH_TEB_VEGn(CHT)
 CALL INIT_NAM_SGH_TEB_VEGn(IO)        
ENDIF

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
! for the time being, chemistry is not implemented on gardens
! CALL POSNAM(INAM,'NAM_CH_ISBAN',GFOUND,ILUOUT)
! IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_ISBAn)
 CALL POSNAM(INAM,'NAM_CH_CONTROLN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_CONTROLn)
 CALL POSNAM(INAM,'NAM_SGH_ISBAN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_SGH_ISBAn)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSCOND',CSCOND,'NP89','PL98')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CALBEDO',CALBEDO,'EVOL','DRY ','WET ','MEAN','USER','CM13')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CC1DRY',CC1DRY,'DEF ','GB93')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSOILFRZ',CSOILFRZ,'DEF','LWT')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CDIFSFCOND',CDIFSFCOND,'DEF ','MLCH')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOWRES',CSNOWRES,'DEF','RIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCPSURF',CCPSURF,'DRY','HUM')
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CRUNOFF',CRUNOFF,'WSAT','DT92','SGH ','TOPD')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CKSAT',CKSAT,'DEF','SGH','EXP')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHORT',CHORT,'DEF','SGH')
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_TEB_VEGn(IO)
 CALL UPDATE_NAM_TEB_VEG_AGSn(IO)
 CALL UPDATE_NAM_CH_TEB_VEGn(CHT)
 CALL UPDATE_NAM_CH_CONTROLn(CHT)
 CALL UPDATE_NAM_SGH_TEB_VEGn(IO)        
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_TEB_VEG_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
!XTSTEP = XUNDEF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_VEG_CONF_n
