!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_ISBA_CONF_n (IO, KSNOW_LAYER_IN, &
                                  HISBA, KPATCH,KGROUND_LAYER,KSNOW_LAYER,KNBIOMASS,  &
                                   KNLITTER, KNLITTLEVS, KNSOILCARB)  
!     ########################################
!
!!****  *GET_ISBA_CONF_n* - routine to get some ISBA fields
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
!!      Original    04/2008
!!      A.L. Gibelin 07/2009 : Dimensions for carbon options
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
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
TYPE(ISBA_OPTIONS_t), INTENT(IN) :: IO
INTEGER, INTENT(IN) :: KSNOW_LAYER_IN
!
CHARACTER(LEN=3), INTENT(OUT) :: HISBA
INTEGER, INTENT(OUT) :: KPATCH        ! number of patchs
INTEGER, INTENT(OUT) :: KGROUND_LAYER ! number of ground layers
INTEGER, INTENT(OUT) :: KSNOW_LAYER   ! number of snow layers
INTEGER, INTENT(OUT) :: KNBIOMASS     ! number of biomass pools
INTEGER, INTENT(OUT) :: KNLITTER      ! number of litter pools
INTEGER, INTENT(OUT) :: KNLITTLEVS    ! number of litter levels
INTEGER, INTENT(OUT) :: KNSOILCARB    ! number of soil carbon pools
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_ISBA_CONF_N',0,ZHOOK_HANDLE)
HISBA = IO%CISBA
KPATCH = IO%NPATCH
KGROUND_LAYER = IO%NGROUND_LAYER
KSNOW_LAYER = KSNOW_LAYER_IN
KNBIOMASS = IO%NNBIOMASS
KNLITTER = IO%NNLITTER
KNLITTLEVS = IO%NNLITTLEVS
KNSOILCARB = IO%NNSOILCARB
IF (LHOOK) CALL DR_HOOK('GET_ISBA_CONF_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_ISBA_CONF_n
