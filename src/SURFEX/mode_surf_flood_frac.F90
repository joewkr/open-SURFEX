!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##########################
MODULE MODE_SURF_FLOOD_FRAC
!##########################
!
!!****  *MODE_SURF_FLOOD_FRAC* -  module for routines to compute flood fraction
!!                               for surface schemes
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all routines to compute
!     flood fractions for the ISBA scheme.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/05/08
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!-------------------------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------------------
!
!     ######################################################
      FUNCTION FLOOD_FRAC_GROUND(PPSNG,PFFLOOD) RESULT(PFFG)
!     ######################################################
!
REAL, DIMENSION(:), INTENT(IN)  :: PPSNG   ! Snow fraction over the ground
REAL, DIMENSION(:), INTENT(IN)  :: PFFLOOD ! Effective floodplain fraction
REAL, DIMENSION(SIZE(PPSNG))    :: PFFG    ! Floodplain fraction over the ground
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_GROUND',0,ZHOOK_HANDLE)
PFFG(:) = PFFLOOD(:)
!
WHERE(PFFLOOD(:)>(1.0-PPSNG(:)))PFFG(:)=1.0-PPSNG(:)
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_GROUND',1,ZHOOK_HANDLE)
!
END FUNCTION FLOOD_FRAC_GROUND
!
!-------------------------------------------------------------------------------
!
!     ########################################################
      FUNCTION FLOOD_FRAC_VEG(PLAI,PPSNV,PFFLOOD) RESULT(PFFV)
!     ########################################################
!
USE MODD_FLOOD_PAR, ONLY : XCFFV
!
REAL, DIMENSION(:), INTENT(IN)  :: PLAI    ! leaf area index
REAL, DIMENSION(:), INTENT(IN)  :: PPSNV   ! Snow fraction over the vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PFFLOOD ! Effective floodplain fraction
REAL, DIMENSION(SIZE(PPSNV))    :: PFFV    ! Floodplain fraction over vegetation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_VEG',0,ZHOOK_HANDLE)
PFFV(:)=PFFLOOD(:)*MIN(1.0,XCFFV/MAX(PLAI(:),0.1))  
!
WHERE(PFFV(:)>(1.0-PPSNV(:)))PFFV(:)=1.0-PPSNV(:)
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_VEG',1,ZHOOK_HANDLE)
!
END FUNCTION FLOOD_FRAC_VEG
!
!-------------------------------------------------------------------------------
!
!     ############################################################
      FUNCTION FLOOD_FRAC_NAT(PVEG,PFFG,PFFV,PFFLOOD) RESULT(PFF)
!     ############################################################
!
REAL, DIMENSION(:), INTENT(IN)  :: PVEG    ! Vegetation cover fraction
REAL, DIMENSION(:), INTENT(IN)  :: PFFG    ! Floodplain fraction over the ground
REAL, DIMENSION(:), INTENT(IN)  :: PFFV    ! Floodplain fraction over vegetation
REAL, DIMENSION(:), INTENT(IN)  :: PFFLOOD ! Effective floodplain fraction
REAL, DIMENSION(SIZE(PVEG))     :: PFF     ! Floodplain fraction at the surface
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_NAT',0,ZHOOK_HANDLE)
PFF(:) = PVEG(:)*PFFV(:) + (1-PVEG(:))*PFFG(:)
!
PFF(:) = MIN(PFF(:),PFFLOOD(:))
IF (LHOOK) CALL DR_HOOK('MODE_SURF_FLOOD_FRAC:FLOOD_FRAC_NAT',1,ZHOOK_HANDLE)
!
END FUNCTION FLOOD_FRAC_NAT
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END MODULE MODE_SURF_FLOOD_FRAC
