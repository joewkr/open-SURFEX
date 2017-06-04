!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_VEG (PEK, KLU,KGROUND_LAYER,KNBIOMASS)  
!   ##########################################################################
!
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KGROUND_LAYER
INTEGER, INTENT(IN) :: KNBIOMASS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_VEG',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Averaged Surface radiative parameters:
!
ALLOCATE(PEK%XSNOWFREE_ALB           (KLU))
ALLOCATE(PEK%XSNOWFREE_ALB_VEG       (KLU))
ALLOCATE(PEK%XSNOWFREE_ALB_SOIL      (KLU))
!
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
!
! - Soil and vegetation heat and water:
!
ALLOCATE(PEK%XWR                     (KLU                    )) 
ALLOCATE(PEK%XTG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(PEK%XWG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(PEK%XWGI                    (KLU,KGROUND_LAYER      )) 
ALLOCATE(PEK%XRESA                   (KLU                    )) 
!
! - Vegetation: Ags Prognostic 
!
ALLOCATE(PEK%XAN                     (KLU                    )) 
ALLOCATE(PEK%XANDAY                  (KLU                    )) 
ALLOCATE(PEK%XANFM                   (KLU                    )) 
ALLOCATE(PEK%XLE                     (KLU                    ))
!
! - Vegetation (Ags 'NIT' 'NCB' option):
!
ALLOCATE(PEK%XBIOMASS                (KLU,KNBIOMASS          ))
ALLOCATE(PEK%XRESP_BIOMASS           (KLU,KNBIOMASS          ))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_VEG',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_VEG
