!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_VEG_PGD (PEK, S, K, P, OALLOC, KLU, KVEGTYPE, KGROUND_LAYER)  
!   ##########################################################################
!
!
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_PE_t, ISBA_P_t, ISBA_K_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_K_t), INTENT(INOUT) :: K
!
LOGICAL, INTENT(IN) :: OALLOC ! True if constant PGD fields must be allocated
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KVEGTYPE
INTEGER, INTENT(IN) :: KGROUND_LAYER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_VEG_PGD',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! - Physiographic field that can evolve prognostically
!
ALLOCATE(PEK%XLAI                    (KLU))
ALLOCATE(PEK%XVEG                    (KLU)) 
ALLOCATE(PEK%XEMIS                   (KLU)) 
ALLOCATE(PEK%XZ0                     (KLU)) 
!
! - vegetation: default option (Jarvis) and general parameters:
!
ALLOCATE(PEK%XRSMIN                  (KLU))
ALLOCATE(PEK%XGAMMA                  (KLU)) 
ALLOCATE(PEK%XWRMAX_CF               (KLU))
ALLOCATE(PEK%XRGL                    (KLU))
ALLOCATE(PEK%XCV                     (KLU)) 
!
ALLOCATE(PEK%XLAIMIN                 (KLU)) 
ALLOCATE(PEK%XSEFOLD                 (KLU)) 
ALLOCATE(PEK%XGMES                   (KLU))
ALLOCATE(PEK%XGC                     (KLU))
ALLOCATE(PEK%XF2I                    (KLU)) 
ALLOCATE(PEK%XBSLAI                  (KLU))
!
! - vegetation:
!
ALLOCATE(PEK%XALBNIR_VEG             (KLU)) 
ALLOCATE(PEK%XALBVIS_VEG             (KLU)) 
ALLOCATE(PEK%XALBUV_VEG              (KLU)) 
!
ALLOCATE(PEK%LSTRESS                 (KLU)) 
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT' option)
!
ALLOCATE(PEK%XCE_NITRO               (KLU)) 
ALLOCATE(PEK%XCF_NITRO               (KLU)) 
ALLOCATE(PEK%XCNA_NITRO              (KLU)) 
!
IF (.NOT. OALLOC) THEN
  IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_VEG_PGD',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
! Input Parameters:
!
! - vegetation + bare soil:
!
ALLOCATE(P%XZ0_O_Z0H               (KLU)) 
!
ALLOCATE(P%XROOTFRAC               (KLU,KGROUND_LAYER       ))
ALLOCATE(P%NWG_LAYER               (KLU))
ALLOCATE(P%XDROOT                  (KLU))
ALLOCATE(P%XDG2                    (KLU))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
!
ALLOCATE(P%XH_TREE              (KLU)) 
!
!
ALLOCATE(P%XRE25                (KLU))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT' options)
!
!
ALLOCATE(P%XAH                 (KLU)) 
ALLOCATE(P%XBH                 (KLU)) 
!
ALLOCATE(P%XDMAX               (KLU)) 
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
ALLOCATE(S%XSOC               (KLU,KGROUND_LAYER       ))  
!
ALLOCATE(K%XSAND              (KLU,KGROUND_LAYER       )) 
ALLOCATE(K%XCLAY              (KLU,KGROUND_LAYER       )) 
ALLOCATE(K%XRUNOFFB           (KLU                     )) 
ALLOCATE(K%XWDRAIN            (KLU                     )) 
!
ALLOCATE(P%XTAUICE            (KLU                    ))
!
ALLOCATE(P%XDG                (KLU,KGROUND_LAYER)) 
!
ALLOCATE(P%XRUNOFFD           (KLU)) 
!
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                   
ALLOCATE(P%XD_ICE             (KLU)) 
!
ALLOCATE(K%XGAMMAT            (KLU )) 
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_VEG_PGD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_VEG_PGD
