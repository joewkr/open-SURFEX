!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      SUBROUTINE DIAG_ISBA_TO_ROUT (PMESH_SIZE, &
                                    PVARC,PVARCP,PVARROUT)
!     ############################
!
!!****  *DIAG_ISBA_TO_ROUT*  
!!
!!    PURPOSE
!!    -------
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   10/11/2006
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_SURF_PAR,        ONLY: XUNDEF
USE MODD_CSTS,            ONLY: XRHOLW
USE MODD_TOPODYN, ONLY : XTOPD_STEP
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN) :: PMESH_SIZE
!
REAL,DIMENSION(:),INTENT(IN)        :: PVARC       ! Current time step cumulated diagnostic from SurfEx
REAL,DIMENSION(:),INTENT(IN)        :: PVARCP      ! Previous time step cumulated diagnostic from SurfEx 
REAL,DIMENSION(:),INTENT(OUT)       :: PVARROUT    ! Not cumulated diagnostic (m3/s)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_TO_ROUT',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
PVARROUT=XUNDEF
!
IF ( SIZE(PVARC,1)==SIZE(PVARCP,1) ) THEN
  !
  WHERE ( PVARC/=XUNDEF )
    PVARROUT = PVARC - PVARCP
    PVARROUT = PVARROUT / XTOPD_STEP
    PVARROUT = PVARROUT * PMESH_SIZE / XRHOLW
  ENDWHERE
  !
ELSE 
  !
  WRITE(*,*) 'Pb with diagnostic to rout'
  CALL ABOR1_SFX("DIAG_ISBA_TO_ROUT: PB WITH DIAGNOSTIC TO ROUT ")
  !
ENDIF
!
WHERE (PVARROUT<0.) PVARROUT = 0.
!
IF (LHOOK) CALL DR_HOOK('DIAG_ISBA_TO_ROUT',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_ISBA_TO_ROUT
