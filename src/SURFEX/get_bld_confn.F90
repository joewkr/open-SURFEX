!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_BLD_CONF_n (BDD, DTT, &
                                 ODATA_BLDTYPE, ODATA_BLD_AGE, ODATA_USETYPE, &
                KDESC_ROOF_LAYER, KDESC_ROAD_LAYER, KDESC_WALL_LAYER, &
                KDESC_FLOOR_LAYER, KDESC_CODE, KDESC_USE, KDESC_AGE, KDESC_BLD)  
!     ########################################
!
!!****  *GET_BLD_CONF_n* - routine to get some ISBA fields
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
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
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
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
!
LOGICAL, INTENT(OUT) :: ODATA_BLDTYPE
LOGICAL, INTENT(OUT) :: ODATA_BLD_AGE
LOGICAL, INTENT(OUT) :: ODATA_USETYPE
INTEGER, INTENT(OUT) :: KDESC_ROOF_LAYER
INTEGER, INTENT(OUT) :: KDESC_ROAD_LAYER
INTEGER, INTENT(OUT) :: KDESC_WALL_LAYER
INTEGER, INTENT(OUT) :: KDESC_FLOOR_LAYER
INTEGER, INTENT(OUT) :: KDESC_CODE
INTEGER, INTENT(OUT) :: KDESC_USE
INTEGER, INTENT(OUT) :: KDESC_AGE
INTEGER, INTENT(OUT) :: KDESC_BLD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_BLD_CONF_N',0,ZHOOK_HANDLE)
ODATA_BLDTYPE = DTT%LDATA_BLDTYPE
ODATA_BLD_AGE = DTT%LDATA_BLD_AGE
ODATA_USETYPE = DTT%LDATA_USETYPE
KDESC_ROOF_LAYER = BDD%NDESC_ROOF_LAYER
KDESC_ROAD_LAYER = BDD%NDESC_ROAD_LAYER
KDESC_WALL_LAYER = BDD%NDESC_WALL_LAYER
KDESC_FLOOR_LAYER = BDD%NDESC_FLOOR_LAYER
KDESC_CODE = BDD%NDESC_CODE
KDESC_USE = BDD%NDESC_USE
KDESC_AGE = BDD%NDESC_AGE
KDESC_BLD = BDD%NDESC_BLD
IF (LHOOK) CALL DR_HOOK('GET_BLD_CONF_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_BLD_CONF_n
