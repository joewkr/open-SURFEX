!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SURF_ATM_CONF_n (CHU, DGO, USS, HPROGRAM)
!     #######################################################
!
!!****  *READ_SURF_ATM_CONF* - reads the general configuration for surface
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
USE MODD_CH_SURF_n, ONLY : CH_SURF_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODE_POS_SURF
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
USE MODN_SSO_n
USE MODN_SURF_ATM_n
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
TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
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
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) THEN
 CALL INIT_NAM_SSOn(USS)
 CALL INIT_NAM_CH_CONTROLn(CHU)
 CALL INIT_NAM_CH_SURFn(CHU)
 CALL INIT_NAM_DIAG_SURF_ATMn(DGO)
 CALL INIT_NAM_DIAG_SURFn(DGO)
 CALL INIT_NAM_WRITE_DIAG_SURFn(DGO)
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
 !
 CALL POSNAM(INAM,'NAM_SSON',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_SSOn)
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CROUGH',CROUGH,'NONE','Z01D','Z04D','BE04')
 !
 CALL POSNAM(INAM,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURFn)
 !
 CSELECT(:) = '            '
 CALL POSNAM(INAM,'NAM_WRITE_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_WRITE_DIAG_SURFn)
 !
 CALL POSNAM(INAM,'NAM_DIAG_SURF_ATMN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURF_ATMn)
 !
 CALL POSNAM(INAM,'NAM_CH_CONTROLN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_CONTROLn)
 !
 CALL POSNAM(INAM,'NAM_CH_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_CH_SURFn)
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
 !
ENDIF
!
IF (IMI.NE.-1) THEN
 CALL UPDATE_NAM_SSOn(USS)
 CALL UPDATE_NAM_CH_CONTROLn(CHU)
 CALL UPDATE_NAM_CH_SURFn(CHU)
 CALL UPDATE_NAM_DIAG_SURF_ATMn(DGO)
 CALL UPDATE_NAM_DIAG_SURFn(DGO)
 CALL UPDATE_NAM_WRITE_DIAG_SURFn(DGO)
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM_CONF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SURF_ATM_CONF_n
