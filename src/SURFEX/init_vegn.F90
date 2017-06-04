!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_VEG_n(IO, KK, PK, PEK, DTV, &
                      OSURF_DIAG_ALBEDO, PDIR_ALB, PSCA_ALB, PEMIS_OUT, PTSRAD )  
!#############################################################
!
!!****  *INIT_VEG_n* - routine to initialize ISBA
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
!!
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
USE MODD_TYPE_SNOW
USE MODD_SNOW_PAR,       ONLY : XEMISSN
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_INIT_SNOW_LW
USE MODI_Z0V_FROM_LAI
USE MODI_VEG_FROM_LAI
USE MODI_EMIS_FROM_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
!
LOGICAL, INTENT(OUT) :: OSURF_DIAG_ALBEDO
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PDIR_ALB
REAL, DIMENSION(:,:), INTENT(OUT) :: PSCA_ALB
REAL, DIMENSION(:), INTENT(OUT) :: PEMIS_OUT
REAL, DIMENSION(:), INTENT(OUT) :: PTSRAD
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI     ! loop increment
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_n',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*      2.     Radiative fields and snow/flood fracion initialization:
!              -------------------------------------------------------
!
!* snow long-wave properties (not initialized in read_gr_snow)
!
 CALL INIT_SNOW_LW(XEMISSN,PEK%TSNOW)
!
!-------------------------------------------------------------------------------
!
!* z0 and vegetation fraction estimated from LAI if not imposed
IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
  DO JI=1,PK%NSIZE_P    
    IF(PEK%XLAI(JI)/=XUNDEF) THEN
      PEK%XLAI (JI) = MAX(PEK%XLAIMIN(JI),PEK%XLAI(JI))
      IF (.NOT.DTV%LIMP_Z0)   &
         PEK%XZ0  (JI) = Z0V_FROM_LAI(PEK%XLAI(JI),PK%XH_TREE(JI),PK%XVEGTYPE_PATCH(JI,:),IO%LAGRI_TO_GRASS)
      IF (.NOT.DTV%LIMP_VEG)  &
        PEK%XVEG (JI) = VEG_FROM_LAI(PEK%XLAI(JI),PK%XVEGTYPE_PATCH(JI,:),IO%LAGRI_TO_GRASS)
      IF (.NOT.DTV%LIMP_EMIS) &
        PEK%XEMIS(JI) = EMIS_FROM_VEG(PEK%XVEG(JI),PK%XVEGTYPE_PATCH(JI,:))
    END IF  
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
IF (IO%LTR_ML) THEN
  ALLOCATE(PEK%XFAPARC   (PK%NSIZE_P))
  ALLOCATE(PEK%XFAPIRC   (PK%NSIZE_P))
  ALLOCATE(PEK%XLAI_EFFC (PK%NSIZE_P))
  ALLOCATE(PEK%XMUS      (PK%NSIZE_P))
  PEK%XFAPARC   (:) = 0.
  PEK%XFAPIRC   (:) = 0.
  PEK%XLAI_EFFC (:) = 0.
  PEK%XMUS      (:) = 0.
ELSE
  ALLOCATE(PEK%XFAPARC   (0))
  ALLOCATE(PEK%XFAPIRC   (0))
  ALLOCATE(PEK%XLAI_EFFC (0))
  ALLOCATE(PEK%XMUS      (0))
ENDIF        
!
!-------------------------------------------------------------------------------
!
!* albedo per tile and averaged albedo, emissivity and radiative temperature
!
ALLOCATE(PEK%XALBNIR_SOIL(PK%NSIZE_P))
ALLOCATE(PEK%XALBVIS_SOIL(PK%NSIZE_P))
ALLOCATE(PEK%XALBUV_SOIL (PK%NSIZE_P))
ALLOCATE(PEK%XALBNIR     (PK%NSIZE_P))
ALLOCATE(PEK%XALBVIS     (PK%NSIZE_P))
ALLOCATE(PEK%XALBUV      (PK%NSIZE_P))
PEK%XALBNIR_SOIL(:) = XUNDEF
PEK%XALBVIS_SOIL(:) = XUNDEF
PEK%XALBUV_SOIL (:) = XUNDEF
PEK%XALBNIR     (:) = XUNDEF
PEK%XALBVIS     (:) = XUNDEF
PEK%XALBUV      (:) = XUNDEF
!
OSURF_DIAG_ALBEDO = .TRUE.
!
!* Initialization of total albedo, emissivity and snow/flood fractions
!
ALLOCATE(PEK%XPSN (PK%NSIZE_P))
ALLOCATE(PEK%XPSNG(PK%NSIZE_P))
ALLOCATE(PEK%XPSNV(PK%NSIZE_P))
PEK%XPSN  = 0.0
PEK%XPSNG = 0.0
PEK%XPSNV = 0.0
!
IF(PEK%TSNOW%SCHEME=='EBA')THEN
   ALLOCATE(PEK%XPSNV_A(PK%NSIZE_P))
   PEK%XPSNV_A = 0.0
ELSE
   ALLOCATE(PEK%XPSNV_A(0))
ENDIF
!
PDIR_ALB = XUNDEF
PSCA_ALB = XUNDEF
PEMIS_OUT= XUNDEF
PTSRAD   = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VEG_n
