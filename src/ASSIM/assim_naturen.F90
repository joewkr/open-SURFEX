!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_NATURE_n (IM, U, HPROGRAM, KI, &
                          PCON_RAIN, PSTRAT_RAIN, PCON_SNOW, PSTRAT_SNOW, &
                          PCLOUDS,   PLSM,        PEVAPTR,   PEVAP,       & 
                          PSWEC,     PTSC,   PUCLS, PVCLS,                &
                          PTS,       PT2M,        PHU2M,     PSWE,        &
                          HTEST, OD_MASKEXT, PLON, PLAT)

!     ###############################################################################
!
!!****  *ASSIM_NATURE_n * - Chooses the surface assimilation schemes for natural continental parts  
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     T. Aspelien
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!--------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE YOMHOOK,         ONLY : LHOOK,   DR_HOOK
USE PARKIND1,        ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_ASSIM_ISBA_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN) :: KI
REAL, DIMENSION(KI), INTENT(IN) :: PCON_RAIN
REAL, DIMENSION(KI), INTENT(IN) :: PSTRAT_RAIN
REAL, DIMENSION(KI), INTENT(IN) :: PCON_SNOW
REAL, DIMENSION(KI), INTENT(IN) :: PSTRAT_SNOW
REAL, DIMENSION(KI), INTENT(IN) :: PCLOUDS
REAL, DIMENSION(KI), INTENT(IN) :: PLSM
REAL, DIMENSION(KI), INTENT(IN) :: PEVAPTR
REAL, DIMENSION(KI), INTENT(IN) :: PEVAP
REAL, DIMENSION(KI), INTENT(IN) :: PSWEC
REAL, DIMENSION(KI), INTENT(IN) :: PTSC
REAL, DIMENSION(KI), INTENT(IN) :: PUCLS
REAL, DIMENSION(KI), INTENT(IN) :: PVCLS
REAL, DIMENSION(KI), INTENT(IN) :: PTS
REAL, DIMENSION(KI), INTENT(IN) :: PT2M
REAL, DIMENSION(KI), INTENT(IN) :: PHU2M
REAL, DIMENSION(KI), INTENT(IN) :: PSWE
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL,  DIMENSION (KI), INTENT(IN) ::  OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT
!
!*      0.2    declarations of local variables
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_N',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF (U%CNATURE=='ISBA  ') THEN
  !
  CALL ASSIM_ISBA_n(IM, U, HPROGRAM, KI,  &
                    PCON_RAIN, PSTRAT_RAIN, PCON_SNOW, PSTRAT_SNOW, &
                    PCLOUDS,   PLSM,        PEVAPTR,   PEVAP,       &
                    PSWEC,     PTSC,     PUCLS, PVCLS,              &
                    PTS,       PT2M,        PHU2M,     PSWE,        &
                    HTEST, OD_MASKEXT, PLON, PLAT )
ELSE
  WRITE(*,*) 'No assimilation done for scheme: ',TRIM(U%CNATURE)
END IF
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_NATURE_n
