!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_ASSIM(OASSIM,HASSIM,HASSIM_ISBA,KPRINTLEV,         &
                               OAROME,OECSST,OAESST,OAESNM,                 &
                               OALADSURF,OREAD_SST_FROM_FILE,               &
                               HFILE_FORMAT_SST,OEXTRAP_SEA,                &
                               OEXTRAP_WATER,OEXTRAP_NATURE,                &
                               OWATERTG2,KBOUTPUT,KECHGU,PRCLIMCA,          &
                               PRCLISST,PSIGH2MO,PSIGT2MO,PSIGWGO,          &
                               PSIGWGB,PSIGW2B,OOBSWG,OOBS2M,OIMVEG,        &
                               PSPRECIP2,PRTHR_QC,PSIGWGO_MAX,              &
                               PRSCAL_JAC,OPRT,OSIM,OBEV,OBFIXED,           &
                               KOBSTYPE,OOBSHEADER,HFILE_FORMAT_OBS,OOBSNAT,&
                               HFILE_FORMAT_FG,HFILE_FORMAT_LSM,            &
                               HFILE_FORMAT_CLIM,HOBS_M,PERROBS_M,PQCOBS_M, &
                               KNCO,KIVAR,KVAR,HVAR_M,HPREFIX_M,            &
                               PSIGMA_M,PTPRT_M,KNCV,PSCALE_Q,              &
                               PSCALE_QLAI,HBIO,HPREFIX_BIO,PALPH,          &
                               KENS,KIE,PINFL_M,PADDINFL_M, PASSIM_WINH,    &
                               PADDTIMECORR_M,OENS_GEN,OPB_CORRELATIONS,    &
                               OPERTURBATION_RUN,OBIAS_CORRECTION,          &
                               OENKF,ODENKF,HTEST)
!     ########################################################################
!
!!****  *DEFAULT_ISBA* - routine to set default values for the configuration for ISBA assimilation scheme
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
!!      L. Jarlan  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2005
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_ASSIM, ONLY : NOBSMAX, NVARMAX
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
LOGICAL,           INTENT(OUT) :: OASSIM        ! assimilation or not
CHARACTER(LEN=5),  INTENT(OUT) :: HASSIM        ! type of corrections PLUS/2DVAR
CHARACTER(LEN=5),  INTENT(OUT) :: HASSIM_ISBA
INTEGER,           INTENT(OUT) :: KPRINTLEV
LOGICAL,           INTENT(OUT) :: OAROME
LOGICAL,           INTENT(OUT) :: OECSST
LOGICAL,           INTENT(OUT) :: OAESST
LOGICAL,           INTENT(OUT) :: OAESNM
LOGICAL,           INTENT(OUT) :: OALADSURF
LOGICAL,           INTENT(OUT) :: OREAD_SST_FROM_FILE
CHARACTER(LEN=6),  INTENT(OUT) :: HFILE_FORMAT_SST
LOGICAL,           INTENT(OUT) :: OEXTRAP_SEA
LOGICAL,           INTENT(OUT) :: OEXTRAP_WATER
LOGICAL,           INTENT(OUT) :: OEXTRAP_NATURE
LOGICAL,           INTENT(OUT) :: OWATERTG2
INTEGER,           INTENT(OUT) :: KBOUTPUT
!
INTEGER,           INTENT(OUT) :: KECHGU
REAL,              INTENT(OUT) :: PRCLIMCA
REAL,              INTENT(OUT) :: PRCLISST
REAL,              INTENT(OUT) :: PSIGH2MO
REAL,              INTENT(OUT) :: PSIGT2MO
REAL,              INTENT(OUT) :: PSIGWGO
REAL,              INTENT(OUT) :: PSIGWGB
REAL,              INTENT(OUT) :: PSIGW2B
LOGICAL,           INTENT(OUT) :: OOBSWG
LOGICAL,           INTENT(OUT) :: OOBS2M
LOGICAL,           INTENT(OUT) :: OIMVEG
REAL,              INTENT(OUT) :: PSPRECIP2 
REAL,              INTENT(OUT) :: PRTHR_QC
REAL,              INTENT(OUT) :: PSIGWGO_MAX
REAL,              INTENT(OUT) :: PRSCAL_JAC
!
LOGICAL,           INTENT(OUT) :: OPRT
LOGICAL,           INTENT(OUT) :: OSIM
LOGICAL,           INTENT(OUT) :: OBEV
LOGICAL,           INTENT(OUT) :: OBFIXED
!
INTEGER,             INTENT(OUT) :: KOBSTYPE
LOGICAL,             INTENT(OUT) :: OOBSHEADER
CHARACTER(LEN=6),    INTENT(OUT) :: HFILE_FORMAT_OBS
CHARACTER(LEN=6),    INTENT(OUT) :: HFILE_FORMAT_FG
CHARACTER(LEN=6),    INTENT(OUT) :: HFILE_FORMAT_LSM
CHARACTER(LEN=6),    INTENT(OUT) :: HFILE_FORMAT_CLIM
CHARACTER(LEN=10),  DIMENSION(NOBSMAX), INTENT(OUT) :: HOBS_M
REAL, DIMENSION(NOBSMAX),    INTENT(OUT) :: PERROBS_M
REAL, DIMENSION(NOBSMAX),    INTENT(OUT) :: PQCOBS_M
INTEGER, DIMENSION(NOBSMAX), INTENT(OUT) :: KNCO
LOGICAL, INTENT(OUT) :: OOBSNAT
!
INTEGER,           INTENT(OUT) :: KIVAR
INTEGER,           INTENT(OUT) :: KVAR
CHARACTER(LEN=3),  DIMENSION(NVARMAX), INTENT(OUT) :: HVAR_M
CHARACTER(LEN=100),  DIMENSION(NVARMAX), INTENT(OUT) :: HPREFIX_M
REAL, DIMENSION(NVARMAX), INTENT(OUT) :: PSIGMA_M
REAL, DIMENSION(NVARMAX), INTENT(OUT) :: PTPRT_M
INTEGER, DIMENSION(NVARMAX), INTENT(OUT) :: KNCV
REAL,                INTENT(OUT) :: PSCALE_Q
REAL,                INTENT(OUT) :: PSCALE_QLAI
CHARACTER(LEN=12),   INTENT(OUT) :: HBIO
CHARACTER(LEN=100),  INTENT(OUT) :: HPREFIX_BIO
REAL, DIMENSION(12), INTENT(OUT) :: PALPH
!
INTEGER, INTENT(OUT) :: KENS
INTEGER, INTENT(OUT) :: KIE
REAL, INTENT(OUT) :: PASSIM_WINH
REAL, DIMENSION(NVARMAX),INTENT(OUT) :: PINFL_M
REAL, DIMENSION(NVARMAX),INTENT(OUT) :: PADDINFL_M
REAL, DIMENSION(NVARMAX),INTENT(OUT) :: PADDTIMECORR_M
LOGICAL, INTENT(OUT) :: OENKF
LOGICAL, INTENT(OUT) :: ODENKF
LOGICAL, INTENT(OUT) :: OENS_GEN
LOGICAL, INTENT(OUT) :: OPB_CORRELATIONS
LOGICAL, INTENT(OUT) :: OPERTURBATION_RUN
LOGICAL, INTENT(OUT) :: OBIAS_CORRECTION
CHARACTER(LEN=2),   INTENT(IN) :: HTEST
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DEFAULT_ASSIM',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('default_assim: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

!
OASSIM    = .FALSE.
HASSIM    = "PLUS "
HASSIM_ISBA = "OI" 
KPRINTLEV = 0
OAROME    = .TRUE.
OECSST    = .FALSE.
OAESST    = .FALSE.
OAESNM    = .FALSE.
OALADSURF = .TRUE.
OREAD_SST_FROM_FILE=.FALSE.
HFILE_FORMAT_SST = "FA    "
OEXTRAP_SEA    = .TRUE.
OEXTRAP_WATER  = .TRUE.
OEXTRAP_NATURE = .FALSE.
OWATERTG2      = .FALSE.

KBOUTPUT = 1
!
KECHGU = 6
!  RCLIMCA : coef. de rappel vers la climatologie des champs de surface
!  RCLISST : coef. de rappel vers la climatologie de SST
!PRCLIMCA=0.045
PRCLIMCA = 0. ! no climatology relaxation
!PRCLISST=0.05 ! as in the original cacsts
PRCLISST = 0.05 
!***  SIGT2MO : ecart-type d'erreur "d'observation" sur T2m
!***  SIGH2MO : ecart-type d'erreur "d'observation" sur Hu2m
PSIGH2MO = 0.1 ! observation error for HU2m
PSIGT2MO = 1.0 ! observation error for T2m
PSIGWGO = 0.06 ! observation error for WG
PSIGWGB = 0.06 ! background error for WG
PSIGW2B = 0.03 ! background error for W2
OOBSWG = .TRUE. ! assimilation of WG
OOBS2M = .FALSE. ! assimilation of T2M + RH2M (with WG)
!     LIMVEG : activation de la limitation a wp > veg*wwilt
!***  LIMVEG  : si wp >= veg*wwilt
OIMVEG = .TRUE.
PSPRECIP2 = 4.0
PRTHR_QC = 3.0
PSIGWGO_MAX = 6.0 ! maximum acceptable WG obs error (%) 
PRSCAL_JAC = 4.0  ! to modify the "effective" assimilation window
!
! Initialization of EKF
OPRT = .FALSE.
OSIM = .FALSE.
OBEV = .TRUE.
OBFIXED = .FALSE.
!
KOBSTYPE = 2
OOBSHEADER = .FALSE.
HFILE_FORMAT_OBS = "FA    "
HFILE_FORMAT_FG = "FA    "
HFILE_FORMAT_LSM = "FA    "
HFILE_FORMAT_CLIM = "FA    "
HOBS_M = (/"T2M ","HU2M","WG2 ","LAI ","SWE "/)
PERROBS_M = (/1.0,0.1,0.4,0.2,0.1/)
PQCOBS_M = (/999.,999.,999.,999.,999./)
KNCO = (/0,0,0,0,0/)
OOBSNAT = .FALSE.
!
KIVAR = 1
KVAR = 4
HVAR_M = (/"WG2","WG1","TG2","TG1","LAI","WG3","WG4","WG5","WG6"/)
HPREFIX_M = (/"","","","","","","","",""/)
PSIGMA_M = (/0.15,0.1,2.0,2.0,0.2,0.2,0.2,0.2,0.2/)
PTPRT_M = (/0.0001,0.0001,0.00001,0.00001,0.001,0.00001,0.0001,0.00001,0.00001/)
KNCV = (/0,0,0,0,0,0,0,0,0/)
PSCALE_Q = 0.125
PSCALE_QLAI = 0.5
HBIO = "BIOMA1"
HPREFIX_BIO = ""
PALPH = (/0., 0., 0., 0.08203445, 0.07496252, 0.06846970, 0.06771856, 0.09744689, &
          0.09744689, 0.07164350, 0.17686594, 0.07164350/)
!
KENS = 1
KIE = 0
PASSIM_WINH = 24
PINFL_M = (/0.,0.,0.,0.,0.,0.,0.,0.,0./)
PADDINFL_M = (/0.,0.,0.,0.,0.,0.,0.,0.,0./)
PADDTIMECORR_M = (/0.,0.,0.,0.,0.,0.,0.,0.,0./)
OENKF = .FALSE.
ODENKF = .FALSE.
OENS_GEN = .TRUE.
OPB_CORRELATIONS = .FALSE.
OPERTURBATION_RUN = .FALSE.
OBIAS_CORRECTION = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_ASSIM',1,ZHOOK_HANDLE)
!
END SUBROUTINE DEFAULT_ASSIM
