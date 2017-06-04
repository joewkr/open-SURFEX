!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_IDEAL_CONF_n (DGO, HPROGRAM)
!     #######################################################
!
!!****  *READ_IDEAL_CONF* - routine to read the configuration for IDEAL
!!                        (MESONH universe)
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
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODE_POS_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODN_IDEAL_n
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
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
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
IF (LHOOK) CALL DR_HOOK('READ_IDEAL_CONF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IMI=GET_CURRENT_MODEL_INDEX_SURFEX()
!
IF (IMI.NE.-1 .AND. LNAM_READ) CALL INIT_NAM_DIAG_SURFn(DGO)
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
 CALL POSNAM(INAM,'NAM_DIAG_SURFN',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=INAM,NML=NAM_DIAG_SURFn)
 !
 !* close namelist file
 !
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
!
ENDIF
!
IF (IMI.NE.-1) CALL UPDATE_NAM_DIAG_SURFn(DGO)
IF (LHOOK) CALL DR_HOOK('READ_IDEAL_CONF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_IDEAL_CONF_n
