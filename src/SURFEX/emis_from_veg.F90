!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_EMIS_FROM_VEG
!#######################
!
INTERFACE EMIS_FROM_VEG
!
    FUNCTION EMIS_FROM_VEG_0D(PVEG,PVEGTYPE) RESULT(PEMIS)
!
REAL,                 INTENT(IN) :: PVEG         ! vegetatino fraction
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL                             :: PEMIS        ! emissivity
!
END FUNCTION EMIS_FROM_VEG_0D
!
!
    FUNCTION EMIS_FROM_VEG_1D(PVEG,PVEGTYPE) RESULT(PEMIS)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PVEG         ! vegetation fraction
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PVEG))      :: PEMIS        ! emissivity
!
END FUNCTION EMIS_FROM_VEG_1D
!
!
    FUNCTION EMIS_FROM_VEG_2D(PVEG,PVEGTYPE) RESULT(PEMIS)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PVEG         ! vegetation fraction
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PVEG,1),SIZE(PVEG,2)) :: PEMIS! emissivity
!
END FUNCTION EMIS_FROM_VEG_2D
!
   FUNCTION EMIS_FROM_VEG_VEGTYPE(PVEG) RESULT(PEMIS)
!
REAL, DIMENSION(:), INTENT (IN)     :: PVEG
!
REAL, DIMENSION(SIZE(PVEG))         :: PEMIS

END FUNCTION EMIS_FROM_VEG_VEGTYPE
!
END INTERFACE
!
END MODULE MODI_EMIS_FROM_VEG
!
!   ####################################################
    FUNCTION EMIS_FROM_VEG_0D(PVEG,PVEGTYPE) RESULT(PEMIS)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates averaged emissivity on natural surfaces
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PVEG         ! vegetation fraction
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL                             :: PEMIS        ! emissivity
!
!*      0.2    declarations of local variables
!
REAL :: ZEMISSOIL                  ! soil emissivity
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_0D',0,ZHOOK_HANDLE)
ZEMISSOIL =   XEMISSN   *     PVEGTYPE(NVT_SNOW) &
              + XEMISSOIL * (1.-PVEGTYPE(NVT_SNOW))  
!
PEMIS   =   XEMISVEG  *     PVEG     &
            + ZEMISSOIL * (1.-PVEG)  
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_0D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION EMIS_FROM_VEG_0D
!
!   ####################################################
    FUNCTION EMIS_FROM_VEG_1D(PVEG,PVEGTYPE) RESULT(PEMIS)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates averaged emissivity on natural surfaces
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PVEG         ! vegetation fraction
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PVEG))      :: PEMIS        ! emissivity
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PVEG))      :: ZEMISSOIL    ! soil emissivity
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_1D',0,ZHOOK_HANDLE)
ZEMISSOIL(:) =   XEMISSN   *     PVEGTYPE(:,NVT_SNOW) &
                + XEMISSOIL * (1.-PVEGTYPE(:,NVT_SNOW))  
!
PEMIS(:)   =   XEMISVEG  *     PVEG(:)     &
               + ZEMISSOIL * (1.-PVEG(:))  
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_1D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION EMIS_FROM_VEG_1D
!
!   ####################################################
    FUNCTION EMIS_FROM_VEG_2D(PVEG,PVEGTYPE) RESULT(PEMIS)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates averaged emissivity on natural surfaces
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PVEG         ! vegetation fraction
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PVEG,1),SIZE(PVEG,2)) :: PEMIS! emissivity
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PVEG,1),SIZE(PVEG,2)) :: ZEMISSOIL ! soil emissivity
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_2D',0,ZHOOK_HANDLE)
ZEMISSOIL(:,:) =   XEMISSN   *     PVEGTYPE(:,:,NVT_SNOW) &
                   + XEMISSOIL * (1.-PVEGTYPE(:,:,NVT_SNOW))  
!
PEMIS(:,:)   =   XEMISVEG  *     PVEG(:,:)     &
                 + ZEMISSOIL * (1.-PVEG(:,:))  
!
WHERE (PVEG(:,:) == XUNDEF)
  PEMIS(:,:) = XUNDEF
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION EMIS_FROM_VEG_2D
!
!   ####################################################
    FUNCTION EMIS_FROM_VEG_VEGTYPE(PVEG) RESULT(PEMIS)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates averaged emissivity on natural surfaces
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_ISBA_PAR,       ONLY : XEMISSOIL, XEMISVEG
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PVEG         ! vegetation fraction
!
REAL,   DIMENSION(SIZE(PVEG))      :: PEMIS        ! emissivity
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PVEG))      :: ZEMISSOIL    ! soil emissivity
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_VEGTYPE',0,ZHOOK_HANDLE)
!
PEMIS(:) = XUNDEF
!
DO JJ = 1,SIZE(PEMIS)
  IF (PVEG(JJ)/=XUNDEF) THEN
    IF (JJ/=NVT_SNOW) THEN
      PEMIS(JJ) = XEMISVEG * PVEG(JJ) + XEMISSOIL * (1.-PVEG(JJ))  
    ELSE
      PEMIS(JJ) = XEMISSN
    ENDIF
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODI_EMIS_FROM_VEG:EMIS_FROM_VEG_VEGTYPE',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION EMIS_FROM_VEG_VEGTYPE
