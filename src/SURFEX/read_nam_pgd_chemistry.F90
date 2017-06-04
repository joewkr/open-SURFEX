!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_CHEMISTRY(HPROGRAM, HCH_EMIS )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_CHEMISTRY* reads namelist for CHEMISTRY
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    S. Queguiner        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2011
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_TEST_NAM_VAR_SURF
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!                                   
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM      ! Type of program
 CHARACTER(LEN=4),    INTENT(OUT)   :: HCH_EMIS      ! Option for emissions computations
!                                  
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=4)         :: CCH_EMIS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_CH_EMISSIONS/ CCH_EMIS
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_CHEMISTRY',0,ZHOOK_HANDLE)
CCH_EMIS        = 'NONE'
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_CH_EMISSIONS',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CH_EMISSIONS)
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCH_EMIS',CCH_EMIS,'NONE','AGGR','SNAP')
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
HCH_EMIS   = CCH_EMIS
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_CHEMISTRY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_CHEMISTRY
