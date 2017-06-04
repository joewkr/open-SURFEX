!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGET_ISBA (PDIR_SW, PSCA_SW, PLW, K, DK )  
!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_ISBA * - Computes diagnostics over ISBA
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2006
!!      Modified    08/2008 (B. Decharme) LWU diag
!!------------------------------------------------------------------
!
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_ISBA_n, ONLY : ISBA_K_t
!
USE MODD_CSTS,           ONLY : XSTEFAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),INTENT(IN)  :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)  :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN)   :: PLW       ! longwave radiation (on horizontal surf.)
!
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(DIAG_t), INTENT(INOUT) :: DK
!
!*      0.2    declarations of local variables
!
INTEGER                          :: ISWB      ! number of SW bands
INTEGER                          :: JSWB      ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_ISBA',0,ZHOOK_HANDLE)
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  DK%XSWBD(:,JSWB) = PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB)
  DK%XSWBU(:,JSWB) = PDIR_SW(:,JSWB) * K%XDIR_ALB_WITH_SNOW(:,JSWB) + &
                     PSCA_SW(:,JSWB) * K%XSCA_ALB_WITH_SNOW(:,JSWB) 
ENDDO
!
DK%XSWD(:) = 0.
DK%XSWU(:) = 0.
DO JSWB=1,ISWB
   DK%XSWD(:) = DK%XSWD(:) + DK%XSWBD(:,JSWB)
   DK%XSWU(:) = DK%XSWU(:) + DK%XSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
!Wrong old diag : LWU=EMIS*STEFAN*Ts**4 + (1.-EMIS)*LW
!Due to e_budget.f90 linearization, LWU can not be calculated using actual Ts
!
DK%XLWD(:) = PLW(:)
DK%XLWU(:) = DK%XSWD(:) - DK%XSWU(:) + DK%XLWD(:) - DK%XRN(:)
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_ISBA
