!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_IDEAL_FLUX_CONF(HPROGRAM)
!     #######################################################
!
!!****  *READ_IDEAL_FLUX_CONF* - routine to read the configuration for ideal flux option
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XDAY, XLVTT
!
USE MODE_POS_SURF
!
USE MODI_ABOR1_SFX
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODN_IDEAL_FLUX
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
!
!*       0.2   Declarations of local variables
!        -------------------------------
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_IDEAL_FLUX_CONF',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!* open namelist file
!
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
!
CSFTQ='kg/m2/s'
!
!* reading of namelist
!  -------------------
!
 CALL POSNAM(INAM,'NAM_IDEAL_FLUX',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_IDEAL_FLUX)
!
IF (NFORCF.GT.NFORC_MAX .OR. NFORCT.GT.NFORC_MAX) &
  CALL ABOR1_SFX("READ_IDEAL_FLUX_CONF: NFORCF AND NFORCT MUST BE LOWER THAN 48")
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CUSTARTYPE',CUSTARTYPE,'Z0   ','USTAR')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSFTQ',CSFTQ,'kg/m2/s','W/m2   ')
!
!* close namelist file
!
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
!
IF (CUSTARTYPE=='Z0   ' .AND. XZ0==XUNDEF) THEN
  CALL ABOR1_SFX(     &
  'FATAL ERROR : XZ0 must be prescribed in namelist NAM_IDEAL_FLUX if CUSTARTYPE="Z0   "')
END IF

IF (CSFTQ=='W/m2   ') THEN
  XSFTQ=XSFTQ/XLVTT
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_IDEAL_FLUX_CONF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_IDEAL_FLUX_CONF
