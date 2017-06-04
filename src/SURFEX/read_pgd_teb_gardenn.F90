!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GARDEN_n (OCH_BIO_FLUX, DTCO, DTV, GB, U, &
                                        IO, K, KDIM, TOP, HPROGRAM,KVERSION,KBUGFIX)
!     #########################################
!
!!****  *READ_PGD_TEB_GARDEN_n* - routine to initialise ISBA physiographic variables 
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
!!      Original    01/2003 
!!      P. Le Moigne  12/2004 : add type of photosynthesis
!!      B. Decharme      2008 : add XWDRAIN
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_ISBA_PAR,        ONLY : XOPTIMGRID
!
USE MODI_READ_PGD_TEB_GARDEN_PAR_n
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
LOGICAL, INTENT(IN) :: OCH_BIO_FLUX
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
INTEGER, INTENT(INOUT) :: KDIM
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
INTEGER,           INTENT(IN)  :: KBUGFIX
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER           :: JLAYER         ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',KDIM)
!
!
!* clay fraction : attention, seul un niveau est present dans le fichier
!* on rempli tout les niveaux de  XCLAY avec les valeurs du fichiers
!
YRECFM='TWN_CLAY'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_CLAY'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XCLAY(:,1),IRESP)
DO JLAYER=2,IO%NGROUND_LAYER
  K%XCLAY(:,JLAYER)=K%XCLAY(:,1)
END DO
!
!* sand fraction
!
YRECFM='TWN_SAND'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_SAND'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XSAND(:,1),IRESP)
DO JLAYER=2,IO%NGROUND_LAYER
  K%XSAND(:,JLAYER)=K%XSAND(:,1)
END DO
!
!* orographic runoff coefficient
!
YRECFM='TWN_RUNOFFB'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_RUNOFFB'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
IF (KVERSION<=3) THEN
  K%XWDRAIN = 0.
ELSE
  YRECFM='TWN_WDRAIN'
  IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,K%XWDRAIN,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* biogenic chemical emissions
!
IF (OCH_BIO_FLUX) THEN
  ALLOCATE(GB%XISOPOT(KDIM))
  YRECFM='E_ISOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,GB%XISOPOT,IRESP)
  !
  ALLOCATE(GB%XMONOPOT(KDIM))
  YRECFM='E_MONOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,GB%XMONOPOT,IRESP)
ELSE
  ALLOCATE(GB%XISOPOT (0))
  ALLOCATE(GB%XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
IF (KVERSION>=7) THEN
  YRECFM='PAR_GARDEN'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%LPAR,IRESP)
ELSEIF (.NOT.TOP%LECOCLIMAP) THEN
  IO%LPAR = .TRUE.
ELSE
  IO%LPAR = .FALSE.
ENDIF
!
IO%LECOCLIMAP = (.NOT. IO%LPAR)
!
ALLOCATE(DTV%LDATA_LAI        (1))
ALLOCATE(DTV%LDATA_VEG        (1))
ALLOCATE(DTV%LDATA_Z0         (1))
ALLOCATE(DTV%LDATA_EMIS       (1))
ALLOCATE(DTV%LDATA_ALBNIR_VEG (1))
ALLOCATE(DTV%LDATA_ALBVIS_VEG (1))
ALLOCATE(DTV%LDATA_ALBUV_VEG  (1))
ALLOCATE(DTV%LDATA_ALBNIR_SOIL(1))
ALLOCATE(DTV%LDATA_ALBVIS_SOIL(1))
ALLOCATE(DTV%LDATA_ALBUV_SOIL (1))
!
IF (.NOT.IO%LPAR) THEN
  DTV%LDATA_LAI        = .FALSE.
  DTV%LDATA_VEG        = .FALSE.
  DTV%LDATA_Z0         = .FALSE.
  DTV%LDATA_EMIS       = .FALSE.
  DTV%LDATA_ALBNIR_VEG = .FALSE.
  DTV%LDATA_ALBVIS_VEG = .FALSE.
  DTV%LDATA_ALBUV_VEG  = .FALSE.
  DTV%LDATA_ALBNIR_SOIL= .FALSE.
  DTV%LDATA_ALBVIS_SOIL= .FALSE.
  DTV%LDATA_ALBUV_SOIL = .FALSE.
ELSE
  DTV%LDATA_LAI        = .TRUE.
  DTV%LDATA_VEG        = .TRUE.
  DTV%LDATA_Z0         = .TRUE.
  DTV%LDATA_EMIS       = .TRUE.
  DTV%LDATA_ALBNIR_VEG = .TRUE.
  DTV%LDATA_ALBVIS_VEG = .TRUE.
  DTV%LDATA_ALBUV_VEG  = .TRUE.
  DTV%LDATA_ALBNIR_SOIL= .TRUE.
  DTV%LDATA_ALBVIS_SOIL= .TRUE.
  DTV%LDATA_ALBUV_SOIL = .TRUE.        
ENDIF
!
IF (IO%LPAR) CALL READ_PGD_TEB_GARDEN_PAR_n(DTV, IO, KDIM, HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GARDEN_n
