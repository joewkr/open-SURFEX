!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_n (OCH_BIO_FLUX, DTCO, DTV, GB, U, &
                                           IO, S, K, KDIM, HPROGRAM,KVERSION)
!     #########################################
!
!!****  *READ_PGD_TEB_GREENROOF_n* - routine to initialise ISBA physiographic variables 
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
!!     based on read_pgd_teb_gardenn
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_S_t
!
USE MODD_ISBA_PAR,        ONLY : XOPTIMGRID
!
USE MODI_READ_SURF
USE MODI_READ_PGD_TEB_GREENROOF_PAR_n
!
!
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
!
LOGICAL, INTENT(IN) :: OCH_BIO_FLUX
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_S_t), INTENT(INOUT) :: S
INTEGER, INTENT(INOUT) :: KDIM
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
!
INTEGER           :: JLAYER         ! loop counter on layers ! not used
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',KDIM)
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!* orographic runoff coefficient
!
YRECFM='GR_RUNOFFB' 
 CALL READ_SURF(HPROGRAM,YRECFM,K%XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
IF (KVERSION<=6) THEN
  K%XWDRAIN = 0.
ELSE
  YRECFM='GR_WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,K%XWDRAIN,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
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
!
!LPAR_GREENROOF = .FALSE.
!IF (KVERSION>=7) THEN
!  YRECFM='PAR_GREENROOF'
!  CALL READ_SURF(HPROGRAM,YRECFM,LPAR_GREENROOF,IRESP)
!END IF
!
!IF (LPAR_GREENROOF) CALL READ_PGD_TEB_GREENROOF_PAR_n(HPROGRAM)
!
IO%LPAR = .TRUE. 
!
IO%LECOCLIMAP = (.NOT. IO%LPAR)
!
 CALL READ_PGD_TEB_GREENROOF_PAR_n(DTV, IO, S, K, KDIM, HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_n
