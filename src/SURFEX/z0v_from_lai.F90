!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_Z0V_FROM_LAI
!#######################
!
INTERFACE Z0V_FROM_LAI
!
    FUNCTION Z0V_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PZ0          ! vegetation roughness
!
END FUNCTION Z0V_FROM_LAI_0D
!
!
    FUNCTION Z0V_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PZ0          ! vegetation roughness
!
END FUNCTION Z0V_FROM_LAI_1D
!
!
    FUNCTION Z0V_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PZ0  ! vegetation roughness
!
END FUNCTION Z0V_FROM_LAI_2D
!
    FUNCTION Z0V_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PZ0)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PZ0  ! vegetation roughness
!
END FUNCTION Z0V_FROM_LAI_VEGTYPE
!
END INTERFACE
!
END MODULE MODI_Z0V_FROM_LAI
!

!   ###########################################################
    FUNCTION Z0V_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation roughness from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      P. Aumond   10/10/2009     Because drag force applied in atmospheric 
!!                                 model, Z0tree -> z0grass
!!      R. Alkama    05/2012   : Extantion from 12 to 19 vegtypes 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PZ0          ! vegetation roughness
!
!*      0.2    declarations of local variables
!
REAL                            :: ZALLEN_H    ! Allen formula for height
REAL                            :: ZLAI        ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PVEGTYPE)) :: ZH_VEG          ! height for each type
REAL                            :: ZAVG_H      ! averaged height
REAL                            :: ZZREF       ! reference height        
!
INTEGER                         :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
ZH_VEG(:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS)
!
ZZREF = 10.
ZAVG_H = 0.
DO JTYPE=1,SIZE(PVEGTYPE)
  ZAVG_H = ZAVG_H + PVEGTYPE(JTYPE) / (LOG(0.13*ZH_VEG(JTYPE)/ZZREF))**2
END DO
ZAVG_H = MAX(ZAVG_H,0.00001)

ZAVG_H = ZZREF / 0.13 * EXP (-1./SQRT(ZAVG_H))
!
PZ0  = MAX(0.001, 0.13*ZAVG_H)
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_0D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION Z0V_FROM_LAI_0D
!
!   ###########################################################
    FUNCTION Z0V_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation roughness from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PZ0          ! vegetation roughness
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI))                  :: ZALLEN_H    ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI))                  :: ZLAI        ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PLAI),SIZE(PVEGTYPE,2)) :: ZH_VEG          ! height for each type
REAL, DIMENSION(SIZE(PLAI))                  :: ZAVG_H      ! averaged height
REAL                                         :: ZZREF       ! reference height        
!
INTEGER                                      :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_1D',0,ZHOOK_HANDLE)
ZH_VEG(:,:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS)
!
ZZREF = 10.
ZAVG_H(:) = 0.
DO JTYPE=1,SIZE(PVEGTYPE,2)
  ZAVG_H(:) = ZAVG_H(:) + PVEGTYPE(:,JTYPE) / (LOG(0.13*ZH_VEG(:,JTYPE)/ZZREF))**2
END DO

ZAVG_H = MAX(ZAVG_H,0.00001)

ZAVG_H(:) = ZZREF / 0.13 * EXP (-1./SQRT(ZAVG_H(:)))
!
PZ0 (:) = MAX(0.001, 0.13*ZAVG_H(:))
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION Z0V_FROM_LAI_1D
!
!   ###########################################################
    FUNCTION Z0V_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PZ0)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation roughness from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PZ0  ! vegetation roughness
!
!*      0.2    declarations of local variables
!

REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZALLEN_H ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZLAI     ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2),SIZE(PVEGTYPE,3)) :: ZH_VEG       ! height for each type
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZAVG_H   ! averaged height
REAL                                                        :: ZZREF    ! reference height        
!
INTEGER                                                     :: JTYPE    ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZH_VEG(:,:,:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS)
!
ZZREF = 10.
ZAVG_H(:,:) = 0.
DO JTYPE=1,SIZE(PVEGTYPE,3)
  ZAVG_H(:,:) = ZAVG_H(:,:) + PVEGTYPE(:,:,JTYPE) / (LOG(0.13*ZH_VEG(:,:,JTYPE)/ZZREF))**2
END DO
ZAVG_H(:,:) = MAX(ZAVG_H(:,:),0.00001)
ZAVG_H(:,:) = ZZREF / 0.13 * EXP (-1./SQRT(ZAVG_H(:,:)))
!
PZ0 (:,:) = MAX(0.001, 0.13*ZAVG_H(:,:))
!
WHERE (PLAI(:,:) == XUNDEF)
  PZ0(:,:) = XUNDEF
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION Z0V_FROM_LAI_2D
!
!
!
!   ###########################################################
    FUNCTION Z0V_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PZ0)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation roughness from leaf
!    area index and type of vegetation for each patch
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!        F.Solmon
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PZ0          ! vegetation roughness
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI)) :: ZALLEN_H    ! Allen formula for height
!
REAL, DIMENSION(SIZE(PLAI)) :: ZH_VEG          ! height for each type
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_VEGTYPE',0,ZHOOK_HANDLE)
ZH_VEG(:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,OAGRI_TO_GRASS)
!
PZ0(:) = XUNDEF
!
WHERE(ZH_VEG(:)/=XUNDEF) PZ0 (:) = MAX(0.001, 0.13*ZH_VEG(:)) ! rugosite pour chaque vegtype
!-----------------------------------------------------------------
!
WHERE (PLAI(:) == XUNDEF)
  PZ0(:) = XUNDEF
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_Z0V_FROM_LAI:Z0V_FROM_LAI_VEGTYPE',1,ZHOOK_HANDLE)
!
!
END FUNCTION Z0V_FROM_LAI_VEGTYPE
!
