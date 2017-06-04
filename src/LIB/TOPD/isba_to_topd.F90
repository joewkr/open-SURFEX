!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE ISBA_TO_TOPD(PVARI,PVART)
!     #######################
!
!!****  *ISBA_TO_TOPD*  
!!
!!    PURPOSE
!!    -------
!
!     
!         
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
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * LTHE / Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   12/2003
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC
USE MODD_COUPLING_TOPD, ONLY : NMASKT
USE MODD_SURF_PAR,        ONLY : XUNDEF, NUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)      :: PVARI   ! variable from ISBA grid
REAL, DIMENSION(:,:), INTENT(OUT)   :: PVART   ! variable for TOPODYN grid
!
!*      0.2    declarations of local variables
!
INTEGER            :: JJ, JI             ! loop control 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ISBA_TO_TOPD',0,ZHOOK_HANDLE)
!
!*       1.     ISBA => TOPODYN-LAT
!               -------------------
!
PVART(:,:)=XUNDEF
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    IF (NMASKT(JJ,JI)/=NUNDEF) PVART(JJ,JI) = PVARI(NMASKT(JJ,JI))
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ISBA_TO_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_TO_TOPD
