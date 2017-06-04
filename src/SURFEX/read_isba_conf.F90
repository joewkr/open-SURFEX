!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_CONF(HPROGRAM)
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
!!      Modified by P. Aumond    (07/2011): tree drag replaced by grass
!!      Modified by T. Aspelien  (04/2012): Separated assimilation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_POS_SURF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODN_AGRI
USE MODN_DEEPSOIL
USE MODN_TREEDRAG
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CONF',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!* open namelist file
!
 CALL OPEN_NAMELIST(HPROGRAM,INAM)
!
!* reading of namelist
!  -------------------
!
 CALL POSNAM(INAM,'NAM_AGRI',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_AGRI)
 CALL POSNAM(INAM,'NAM_DEEPSOIL',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_DEEPSOIL)
 CALL POSNAM(INAM,'NAM_TREEDRAG',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_TREEDRAG)
!
!* close namelist file
!
 CALL CLOSE_NAMELIST(HPROGRAM,INAM)
IF (LHOOK) CALL DR_HOOK('READ_ISBA_CONF',1,ZHOOK_HANDLE)
!
! 
!-------------------------------------------------------------------------------
!
!* surface time-step forced by the atmosphere
!
!XTSTEP = XUNDEF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_CONF
