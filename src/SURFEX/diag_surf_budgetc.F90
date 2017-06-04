!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC (D, DC, PTSTEP, ONOTICE)  
!     #########################################################################
!
!!****  *DIAG_SURF_BUDGETC * - Computes cumulated diagnostics 
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
!!      Original    08/2009
!!------------------------------------------------------------------
! 
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DC
!
REAL,               INTENT(IN) :: PTSTEP  
!
LOGICAL, INTENT(IN) :: ONOTICE
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!* total incoming and outgoing SW
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_FLAKE',0,ZHOOK_HANDLE)
!
IF (ONOTICE) DC%XSWD(:) = DC%XSWD(:) + D%XSWD(:) * PTSTEP
DC%XSWU(:) = DC%XSWU(:) + D%XSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
IF (ONOTICE) DC%XLWD(:) = DC%XLWD(:) + D%XLWD(:) * PTSTEP
DC%XLWU(:) = DC%XLWU(:) + D%XLWU(:) * PTSTEP
!
!* net radiation
!
DC%XRN(:) = DC%XRN(:) + D%XRN(:) * PTSTEP
!
!* sensible heat flux
!
DC%XH(:) = DC%XH(:) + D%XH(:) * PTSTEP 
!
IF (ONOTICE) THEN
  !
  !* latent heat flux
  !
  DC%XLE (:) = DC%XLE (:) + D%XLE (:) * PTSTEP 
  DC%XLEI(:) = DC%XLEI(:) + D%XLEI(:) * PTSTEP 
  !
  !* evaporation and sublimation (kg/m2)
  !
  DC%XEVAP(:) = DC%XEVAP(:) + D%XEVAP(:) * PTSTEP
  DC%XSUBL(:) = DC%XSUBL(:) + D%XSUBL(:) * PTSTEP
  !
ENDIF
!
!* storage flux
!
DC%XGFLUX(:) = DC%XGFLUX(:) + D%XGFLUX(:) * PTSTEP 
!
!* wind stress
!
DC%XFMU(:) = DC%XFMU(:) + D%XFMU(:) * PTSTEP 
DC%XFMV(:) = DC%XFMV(:) + D%XFMV(:) * PTSTEP
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC
