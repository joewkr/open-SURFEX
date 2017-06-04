!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_TOWN_n (DTCO, OREAD_BUDGETC, UG, U, GCP, TM, GDM, GRM, DGO, DL, DLC,  &                        
                              HPROGRAM,HINIT,KI,KSV,KSW, HSV,PCO2,PRHOA,       &
                              PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                              PEMIS,PTSRAD,PTSURF,KYEAR,KMONTH,KDAY,PTIME,     &
                              HATMFILE,HATMFILETYPE,HTEST                      )  
!     #############################################################
!
!!****  *INIT_TOWN_n* - chooses initialization routine for towns
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
!!      Original    27/09/96 
!!       V.Masson   18/08/97 call to fmread directly with dates and strings
!!       V.Masson   15/03/99 new PGD treatment with COVER types
!        F.Solmon  06/00   adaptation for patch approach
!!       B.Decharme 04/2013 new coupling variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t, DIAG_t
!
USE MODD_CSTS,       ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_INIT_IDEAL_FLUX
!
USE MODI_INIT_TEB_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: DL
TYPE(DIAG_t), INTENT(INOUT) :: DLC
!
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KI),  INTENT(IN)  :: PAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
INTEGER,                          INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,                          INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,                          INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                             INTENT(IN)  :: PTIME     ! current time since
                                                          !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TOWN_N',0,ZHOOK_HANDLE)
IF (U%CTOWN=='NONE  ') THEN
  PDIR_ALB=0.
  PSCA_ALB=0.
  PEMIS   =1.
  PTSRAD  =XTT
  PTSURF  =XTT
ELSE IF (U%CTOWN=='FLUX  ') THEN
  CALL INIT_IDEAL_FLUX(DGO, DL, DLC, OREAD_BUDGETC, &
                       HPROGRAM,HINIT,KI,KSV,KSW,HSV,PDIR_ALB,PSCA_ALB,  &
                       PEMIS,PTSRAD,PTSURF,'OK'                    )  
ELSE IF (U%CTOWN=='TEB   ') THEN
  CALL INIT_TEB_n(DTCO, UG, U, GCP, TM%CHT, TM%DTT, TM%SB, TM%G, TM%TOP,        &
                  TM%TPN, TM%TIR, TM%NT, TM%TD, TM%BDD, TM%BOP, TM%DTB, TM%NB,  &
                  GDM, GRM, HPROGRAM, HINIT, KI, KSV, KSW, HSV, PCO2,           &
                  PRHOA, PZENITH, PAZIM, PSW_BANDS, PDIR_ALB,                   &
                  PSCA_ALB, PEMIS, PTSRAD, PTSURF, KYEAR, KMONTH,               &
                  KDAY, PTIME, HATMFILE, HATMFILETYPE, 'OK'                     )
END IF
IF (LHOOK) CALL DR_HOOK('INIT_TOWN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TOWN_n
