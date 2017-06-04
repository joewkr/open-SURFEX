!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE ISBA_BUDGET_INIT (OWATER_BUDGET, HISBA, PEK, PDG, PDZG, &
                            PWG_INI, PWGI_INI, PWR_INI, PSWE_INI )
!     ###############################################################################
!
!!****  *ISBA_BUDGET_INIT * - initialize water and energy budget for ISBA
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2012
!!
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XRHOLW
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
LOGICAL, INTENT(IN) :: OWATER_BUDGET
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HISBA      ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, DIMENSION(:,:),  INTENT(IN) :: PDG        ! soil layer depth               (m)
REAL, DIMENSION(:,:),  INTENT(IN) :: PDZG       ! soil layer thickness           (m)
!
REAL, DIMENSION(:), INTENT(OUT)   :: PWG_INI    ! total wg at t-1                (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PWGI_INI   ! total wgi at t-1               (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PWR_INI    ! total wr at t-1                (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PSWE_INI   ! total swe at t-1               (kg m-2)
!
!*      0.2    declarations of local variables
!
INTEGER :: INI, INL, INLS
INTEGER :: JI, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET_INIT',0,ZHOOK_HANDLE)
!
INI =SIZE(PEK%XWG,1)
INL =SIZE(PEK%XWG,2)
INLS=SIZE(PEK%TSNOW%WSNOW,2)
!
PWG_INI (:) = XUNDEF
PWGI_INI(:) = XUNDEF
PSWE_INI(:) = XUNDEF
PWR_INI (:) = XUNDEF
!
!
! * Water budget
!   ------------
!
IF(OWATER_BUDGET)THEN
!
! total wr at t-1
  PWR_INI(:)=PEK%XWR(:)
!
! total swe at t-1
  PSWE_INI(:)=0.0
  DO JL=1,INLS
     DO JI=1,INI
        PSWE_INI(JI)=PSWE_INI(JI)+PEK%TSNOW%WSNOW(JI,JL)
     ENDDO
  ENDDO
!
! total wg and wgi at t-1
  PWG_INI (:)= 0.0
  PWGI_INI(:)= 0.0
  IF(HISBA=='DIF')THEN
    DO JL=1,INL
       DO JI=1,INI
          IF(PEK%XWG(JI,JL)/=XUNDEF)THEN
             PWG_INI (JI)=PWG_INI (JI)+PEK%XWG (JI,JL)*PDZG(JI,JL)*XRHOLW
             PWGI_INI(JI)=PWGI_INI(JI)+PEK%XWGI(JI,JL)*PDZG(JI,JL)*XRHOLW
          ENDIF
       ENDDO
    ENDDO
  ELSE
    PWG_INI (:)=PEK%XWG (:,2)*PDG(:,2)*XRHOLW
    PWGI_INI(:)=PEK%XWGI(:,2)*PDG(:,2)*XRHOLW
    IF(HISBA=='3-L')THEN
      PWG_INI(:)=PWG_INI(:)+PEK%XWG(:,3)*(PDG(:,3)-PDG(:,2))*XRHOLW
    ENDIF
  ENDIF
!
ENDIF
!
! * Energy budget
!   -------------
!
! not yet implemented
!
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_BUDGET_INIT
