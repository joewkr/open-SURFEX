!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DEFAULT_SURF_ATM(HPROGRAM)
!     #######################################################
!
!!****  *READ_DEFAULT_SURF_ATM* - routine to read the default general configuration for surface
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
USE MODE_POS_SURF
!
USE MODI_GET_LUOUT
USE MODI_GET_DEFAULT_NAM_n
!
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODN_CHS_ORILAM
USE MODN_SURF_ATM
USE MODN_WRITE_SURF_ATM
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output listing logical unit
INTEGER           :: ILUDES         ! .des file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_SURF_ATM',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL GET_DEFAULT_NAM_n(HPROGRAM,'READ ',ILUDES)
!
IF (ILUDES==0 .AND. LHOOK) CALL DR_HOOK('READ_DEFAULT_SURF_ATM',1,ZHOOK_HANDLE)
IF (ILUDES==0) RETURN
!
!* reading of file with default value
!  ----------------------------------
!
 CALL POSNAM(ILUDES,'NAM_CHS_ORILAM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_CHS_ORILAM)
 CALL POSNAM(ILUDES,'NAM_SURF_ATM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_SURF_ATM)
 CALL POSNAM(ILUDES,'NAM_WRITE_SURF_ATM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_WRITE_SURF_ATM)
IF (LHOOK) CALL DR_HOOK('READ_DEFAULT_SURF_ATM',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DEFAULT_SURF_ATM
