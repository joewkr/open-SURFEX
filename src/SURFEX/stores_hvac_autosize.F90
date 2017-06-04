!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE STORES_HVAC_AUTOSIZE (B, BOP, DTB)
!     #############################################################
!
!!****  *STORES_HVAC_AUTOSIZE* - routine to store the HVAC system
!!                               characteristics for further use
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
!
INTEGER :: IL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!

IF (LHOOK) CALL DR_HOOK('STORES_HVAC_AUTOSIZE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       8.     Building HVAC automatic sizing:
!               -------------------------------  
!* stores the real systems characteristics in physiographic data 
!  for further use
!
IL = SIZE(B%XM_SYS_RAT)
!
DTB%LDATA_M_SYS_RAT = .TRUE.
ALLOCATE(DTB%XPAR_M_SYS_RAT(IL))
DTB%XPAR_M_SYS_RAT = B%XM_SYS_RAT 
!
DTB%LDATA_CAP_SYS_RAT = .TRUE.
ALLOCATE(DTB%XPAR_CAP_SYS_RAT(IL))
DTB%XPAR_CAP_SYS_RAT = B%XCAP_SYS_RAT
!
DTB%LDATA_CAP_SYS_HEAT = .TRUE.
ALLOCATE(DTB%XPAR_CAP_SYS_HEAT(IL))
DTB%XPAR_CAP_SYS_HEAT = B%XCAP_SYS_HEAT
!
BOP%LAUTOSIZE = .FALSE.
DTB%LDATA_T_SIZE_MIN = .FALSE.
DTB%LDATA_T_SIZE_MAX = .FALSE.
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('STORES_HVAC_AUTOSIZE',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE STORES_HVAC_AUTOSIZE
