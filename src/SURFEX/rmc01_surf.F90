!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE RMC01_SURF(PZ, PLMO, PLK, PLEPS, ONEUTRAL)
!     ##############################################################
!
!!****  *RMC01_SURF* -
!!
!!    PURPOSE
!!    -------
!!    This routine modifies the mixing and dissipative length near the SBL.
!!    (Redelsperger, Mahe and Carlotti, 2001)
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson  - Meteo-France -
!!
!!    MODIFICATIONS
!!    -------------
!!     Original     07/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_CSTS,        ONLY : XKARMAN
USE MODD_CANOPY_TURB, ONLY : XALPSBL, XCMFS, XCED
!
USE MODE_SBLS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
REAL, DIMENSION(:,:),   INTENT(IN)  :: PZ   ! altitude of full levels
REAL, DIMENSION(:,:),   INTENT(IN)  :: PLMO ! Monin Obuhkov length
REAL, DIMENSION(:,:),   INTENT(OUT) :: PLK  ! Mixing length
REAL, DIMENSION(:,:),   INTENT(OUT) :: PLEPS! Dissipative length
LOGICAL, OPTIONAL,      INTENT(IN)  :: ONEUTRAL
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
LOGICAL :: GNEUTRAL
!
INTEGER :: JK        ! loop counter
!
REAL, DIMENSION(SIZE(PZ,1),SIZE(PZ,2)) :: ZZ_O_LMO ! height / LMO
REAL, DIMENSION(SIZE(PZ,1),SIZE(PZ,2)) :: ZPHIM    ! MO function
                                                   ! for stress
REAL, DIMENSION(SIZE(PZ,1),SIZE(PZ,2)) :: ZPHIE    ! MO function
                                                   ! for TKE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
GNEUTRAL = .FALSE.
IF (PRESENT(ONEUTRAL)) GNEUTRAL = ONEUTRAL
!
!*     1. MO quantities
!         -------------
!
! z/LMO
IF (LHOOK) CALL DR_HOOK('RMC01_SURF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*     2. Modification of the mixing length
!         ---------------------------------
!
PLK  (:,:) = XKARMAN/SQRT(XALPSBL)/XCMFS * PZ(:,:)  
!
!-------------------------------------------------------------------------------
!
!*     3. Modification of the dissipative length
!         --------------------------------------
!
PLEPS(:,:) = XKARMAN*(XALPSBL**1.5)*XCED * PZ(:,:)
!
!-------------------------------------------------------------------------------
!
IF (GNEUTRAL) THEN
  IF (LHOOK) CALL DR_HOOK('RMC01_SURF',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
WHERE (PLMO(:,:)==XUNDEF)
  ZZ_O_LMO(:,:)=0.
ELSEWHERE
  ZZ_O_LMO(:,:)=PZ(:,:)/PLMO(:,:)
END WHERE
ZZ_O_LMO(:,:) = MAX(ZZ_O_LMO(:,:),-10.)
ZZ_O_LMO(:,:) = MIN(ZZ_O_LMO(:,:), 10.)
!
!
! MO function for stress
ZPHIM(:,:) = BUSINGER_PHIM(ZZ_O_LMO(:,:))
!
! MO function for TKE
ZPHIE(:,:) = BUSINGER_PHIE(ZZ_O_LMO(:,:))
!
!-------------------------------------------------------------------------------
!
!*     2. Modification of the mixing length
!         ---------------------------------
!
PLK  (:,:) = PLK  (:,:) / (ZPHIM(:,:)**2*SQRT(ZPHIE(:,:)))  
!
!-------------------------------------------------------------------------------
!
!*     3. Modification of the dissipative length
!         --------------------------------------
!
PLEPS(:,:) = PLEPS(:,:) / (ZPHIM(:,:)**2*SQRT(ZPHIE(:,:))) 
!
WHERE (ZZ_O_LMO(:,:)<0.)
  PLEPS(:,:) = PLEPS(:,:)/(1.-1.9*ZZ_O_LMO(:,:))
ELSEWHERE
  PLEPS(:,:) = PLEPS(:,:)/(1.-0.3*SQRT(ZZ_O_LMO(:,:)))
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('RMC01_SURF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE RMC01_SURF
