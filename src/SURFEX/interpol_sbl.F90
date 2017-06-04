!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INTERPOL_SBL( PZ, PIN, PH, POUT)
!     #####################################################################
!
!!
!!    PURPOSE
!!    -------
!     This routine do interpolation of field from canopy levels to a defined
!     height. Interpolation is linear.
!         
!     
!!**  METHOD
!!    ------
!     We search for the levels aroud the specified height of interpolation and
!     then perform a linear interpolation. If height of interpolation isn't
!     between two canopy levels, we send the value XUNDEF.
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
!!      
!!    AUTHOR
!!    ------
!     Sebastien Riette
!!
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    14/01/2010
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
REAL, DIMENSION(:,:), INTENT(IN)       :: PZ     ! Height of canopy levels
REAL, DIMENSION(:,:), INTENT(IN)       :: PIN    ! Filed values on canopy levels
REAL,                 INTENT(IN)       :: PH     ! Height of interpolation
!
REAL, DIMENSION(:)  , INTENT(OUT)      :: POUT   ! Interpolated value
!
!*      0.2    declarations of local variables
!
INTEGER :: ILEVEL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Starting from the bottom, we look for the canopy level just below 10m and
! we interpolate linearly the canopy wind field if we're not on last level. If
! we are on last level, we do nothing and XUNDEF is left in POUT.
IF (LHOOK) CALL DR_HOOK('INTERPOL_SBL',0,ZHOOK_HANDLE)
POUT(:) = XUNDEF
ILEVEL=1

!While there are XUNDEF values and we aren't at canopy's top
DO WHILE(ANY(POUT(:)==XUNDEF) .AND. ILEVEL/=SIZE(PZ,2))

  !Where interpolation is needed and possible
  !(10m is between ILEVEL and ILEVEL+1)
  WHERE(POUT(:)==XUNDEF .AND. PZ(:,ILEVEL+1)>=10.)

    !Interpolation between ILEVEL and ILEVEL+1
    POUT(:)=PIN(:,ILEVEL) + &
              (PIN(:,ILEVEL+1)-PIN(:,ILEVEL)) * &
              (PH-PZ(:,ILEVEL)) / (PZ(:,ILEVEL+1)-PZ(:,ILEVEL))  

  END WHERE
  ILEVEL=ILEVEL+1
END DO
IF (LHOOK) CALL DR_HOOK('INTERPOL_SBL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_SBL
