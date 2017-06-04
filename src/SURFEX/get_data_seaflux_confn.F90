!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_DATA_SEAFLUX_CONF_n (DTS, &
                                          OSST_DATA, KTIME)  
!     ########################################
!
!!****  *GET_DATA_SEAFLUX_CONF_n* - routine to get some ISBA fields
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
!
!
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
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
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
!
LOGICAL, INTENT(OUT) :: OSST_DATA     ! number of patchs
INTEGER, INTENT(OUT) :: KTIME ! number of ground layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_DATA_SEAFLUX_CONF_N',0,ZHOOK_HANDLE)
OSST_DATA = DTS%LSST_DATA
IF (ASSOCIATED(DTS%TDATA_SST)) THEN
  KTIME = SIZE(DTS%TDATA_SST)
ELSE
  KTIME = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('GET_DATA_SEAFLUX_CONF_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_DATA_SEAFLUX_CONF_n
