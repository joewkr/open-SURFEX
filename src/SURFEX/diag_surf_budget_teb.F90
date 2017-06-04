!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGET_TEB (D, PDIR_SW, PSCA_SW, PDIR_ALB, PSCA_ALB,  &
                                        PLW, PEMIS, PTRAD   )  
!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_TEB * - Computes diagnostics over TEB
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
!!------------------------------------------------------------------
!
USE MODD_DIAG_n, ONLY : DIAG_t
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
TYPE(DIAG_t), INTENT(INOUT) :: D
!
REAL, DIMENSION(:,:),INTENT(IN)  :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)  :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN)   :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)   :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(:,:),INTENT(IN)  :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN)  :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN)   :: PEMIS     ! emissivity                            (-)                                
!
!*      0.2    declarations of local variables
!
INTEGER                          :: ISWB      ! number of SW bands
INTEGER                          :: JSWB      ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_TEB',0,ZHOOK_HANDLE)
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  D%XSWBD(:,JSWB) = PDIR_SW(:,JSWB)                    + PSCA_SW(:,JSWB)
  D%XSWBU(:,JSWB) = PDIR_SW(:,JSWB) * PDIR_ALB(:,JSWB) + PSCA_SW(:,JSWB) * PSCA_ALB(:,JSWB) 
ENDDO
!
D%XSWD(:) = 0.
D%XSWU(:) = 0.
DO JSWB=1,ISWB
   D%XSWD(:)=D%XSWD(:)+D%XSWBD(:,JSWB)
   D%XSWU(:)=D%XSWU(:)+D%XSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
D%XLWD(:)=PLW(:)
D%XLWU(:)=PEMIS(:)*XSTEFAN*PTRAD(:)**4 + (1.-PEMIS(:))*PLW(:)
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_TEB
