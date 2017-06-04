!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_NATURE_n (DTCO, OREAD_BUDGETC, UG, U, USS, GCP, IM, &
                                DTZ, DGO, DL, DLC, NDST, SLT, SV,         &
                                HPROGRAM,HINIT,OLAND_USE, KI,KSV,KSW,     &
                                HSV,PCO2,PRHOA,PZENITH,PAZIM,PSW_BANDS,   &
                                PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD,PTSURF,    &
                                KYEAR, KMONTH,KDAY, PTIME, TPDATE_END,    &
                                HATMFILE,HATMFILETYPE,HTEST              )  
!     #############################################################
!
!!****  *INIT_NATURE_n* - routine to choose initialization of vegetation scheme
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
!!      B. Decharme  04/2013 new coupling variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
USE MODD_DST_n, ONLY : DST_NP_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_CSTS,       ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_INIT_IDEAL_FLUX
!
USE MODI_INIT_ISBA_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
LOGICAL, INTENT(IN) :: OREAD_BUDGETC
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: DL
TYPE(DIAG_t), INTENT(INOUT) :: DLC
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SV_t), INTENT(INOUT) :: SV
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE ! 
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
TYPE(DATE), INTENT(INOUT) :: TPDATE_END
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
REAL(KIND=JPRB) :: ZHOOK_HANDLE
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
IF (LHOOK) CALL DR_HOOK('INIT_NATURE_N',0,ZHOOK_HANDLE)
IF (U%CNATURE=='NONE  ') THEN
  PDIR_ALB=0.
  PSCA_ALB=0.
  PEMIS   =1.
  PTSRAD  =XTT
  PTSURF  =XTT
ELSE IF (U%CNATURE=='FLUX  ') THEN
  CALL INIT_IDEAL_FLUX(DGO, DL, DLC, OREAD_BUDGETC, HPROGRAM, HINIT, &
                       KI, KSV, KSW, HSV, PDIR_ALB, PSCA_ALB, PEMIS,   &
                       PTSRAD, PTSURF, 'OK'    )  
ELSE IF (U%CNATURE=='ISBA  ' .OR. U%CNATURE=='TSZ0') THEN
  CALL INIT_ISBA_n(DTCO, OREAD_BUDGETC, UG, U, USS, GCP, &
                   IM, DTZ, NDST, SLT, SV, &
                   HPROGRAM, HINIT, OLAND_USE, KI, KSV, KSW, HSV, &
                   PCO2, PRHOA, PZENITH, PAZIM, PSW_BANDS,        &
                   PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD, PTSURF,     &
                   KYEAR, KMONTH, KDAY, PTIME, TPDATE_END,        &
                   HATMFILE, HATMFILETYPE, 'OK'     )  
END IF
IF (LHOOK) CALL DR_HOOK('INIT_NATURE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_NATURE_n
