!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_EMISSION_TO_ATM_n (CHN, SV, &
                                       PSFSV,PRHOA)
!     ######################################################################
!!
!!***  *CH_EMISSION_TO_ATM_n* - 
!!
!!    PURPOSE
!!    -------
!!      
!!
!!**  METHOD
!!    ------
!!
!!    AUTHOR
!!    ------
!!    S. Queguiner
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/2011
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_TYPE_EFUTIL
USE MODD_CHS_AEROSOL
!
USE MODI_CH_AER_EMISSION
USE MODI_ABOR1_SFX
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1  declaration of arguments
!
!
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
TYPE(SV_t), INTENT(INOUT) :: SV
!
REAL,             DIMENSION(:,:),INTENT(INOUT):: PSFSV  ! flux of     atmospheric scalar var.   (Mol/m2/s)
REAL,             DIMENSION(:),  INTENT(IN)   :: PRHOA  ! Air density (kg/m3)
!
!*       0.2  declaration of local variables
!
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: CNAMES
TYPE(PRONOSVAR_T),POINTER :: CURPRONOS !Current pronostic variable
!
INTEGER :: JSPEC ! Loop counter on aggregated emitted chemical species
INTEGER :: JSV   ! Loop counter on atmospheric species
INTEGER :: ISV   ! Number       of atmospheric species
!
REAL,DIMENSION(SIZE(PSFSV,1),SIZE(PSFSV,2)) :: ZEMIS ! interpolated in time emission flux
REAL,DIMENSION(SIZE(PSFSV,1)              ) :: ZFCO  ! CO emission flux

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_TO_ATM_n',0,ZHOOK_HANDLE)
!
!      1.     Agregation : emissions computation
!             ----------------------------------
!
ISV = SIZE(SV%CSV)
!
ZEMIS(:,:) = 0.
!
! Point on head of Pronostic variable list
! to cover the entire list.
CNAMES=>SV%CSV
CURPRONOS=>CHN%TSPRONOSLIST
!
DO WHILE(ASSOCIATED(CURPRONOS))
  IF (CURPRONOS%NAMINDEX > ISV) CALL ABOR1_SFX('CH_EMISSION_FLUXN: FATAL ERROR')
  !  
  ZEMIS(:,CURPRONOS%NAMINDEX) = 0.
  !
  ! Loop on the number of agreg. coeff.
  DO JSPEC=1,CURPRONOS%NBCOEFF
    !   Compute agregated flux    
    ZEMIS(:,CURPRONOS%NAMINDEX) = ZEMIS(:,CURPRONOS%NAMINDEX)+ &
            CURPRONOS%XCOEFF(JSPEC)*CHN%XEMIS_FIELDS(:,CURPRONOS%NEFINDEX(JSPEC))
  END DO
  !
  CURPRONOS=>CURPRONOS%NEXT
  !
END DO
!
!------------------------------------------------------------------------------
!
!      2.     Primary Aerosol emissions
!             -------------------------
!
IF (LCH_AERO_FLUX) THEN
  ZFCO(:) = 0.
  DO JSV=1,ISV
    IF (SV%CSV(JSV)=='CO    ') ZFCO(:) = ZEMIS(:,JSV)
  END DO
  !
  CALL CH_AER_EMISSION(ZEMIS,PRHOA,SV%CSV,1,ZFCO)
END IF
!
!------------------------------------------------------------------------------
!
!      3.     Adds emissions from inventories to chemical species fluxes
!             ----------------------------------------------------------
!
PSFSV(:,:) = PSFSV(:,:) + ZEMIS(:,:)
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_TO_ATM_n',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE CH_EMISSION_TO_ATM_n
