!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_ISBA_CARBON(HPROGRAM,HRESPSL)
!     #######################################################
!
!!****  *READ_PREP_ISBA_CARBON* - routine to read the configuration for soil 
!!                              carbon in ISBA fields preparation
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
!!      A.L. Gibelin   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2009 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_ISBA_CARBON
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
 CHARACTER(LEN=3),  INTENT(OUT) :: HRESPSL  ! Soil respiration

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output file logical unit
INTEGER           :: ILUNAM         ! namelist file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.3  Declaration of namelists
!             ------------------------
!
!
NAMELIST/NAM_PREP_ISBA_CARBON/CRESPSL
!-------------------------------------------------------------------------------
!
!* default
!  -------
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_CARBON',0,ZHOOK_HANDLE)
!
IF (LNAM_READ) THEN
 !   
 CRESPSL = 'DEF'
 ! 
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
 !
 !* reading of namelist
 !  -------------------
 !
 !
 CALL POSNAM(ILUNAM,'NAM_PREP_ISBA_CARBON',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PREP_ISBA_CARBON)
 !
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CRESPSL',CRESPSL,'DEF','PRM','CNT')
 !
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
ENDIF
!
HRESPSL = CRESPSL
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_CARBON',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_ISBA_CARBON
