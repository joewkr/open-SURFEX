!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SURF_ATM_CONF(HPROGRAM)
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
!!      B. Decharme 06/2013 CIMPLICIT_WIND is now in NAM_SURF_REPROD_OPER
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODE_POS_SURF
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
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM_CONF',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!* open namelist file
!
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
!
!* reading of namelist
!  -------------------
!
 CALL POSNAM(INAM,'NAM_CHS_ORILAM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_CHS_ORILAM)
!
 CALL POSNAM(INAM,'NAM_SURF_ATM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_SURF_ATM)
!
 CALL POSNAM(INAM,'NAM_WRITE_SURF_ATM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_WRITE_SURF_ATM)
!
!
!* close namelist file
!
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM_CONF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SURF_ATM_CONF
