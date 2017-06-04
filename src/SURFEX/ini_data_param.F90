!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE INI_DATA_PARAM(PLAI, PH_TREE,        &
                                PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, PRSMIN,       &
                                PRGL, PCV, PGAMMA, PGMES, PGC, PBSLAI, PSEFOLD,     &
                                PLAIMIN_IN, PLAIMIN_OUT, PDMAX, PSTRESS, PF2I,      &
                                PVEG_IN, PVEG_OUT,                                  &
                                PGREEN, PZ0, PZ0_O_Z0H, PEMIS_ECO, PWRMAX_CF,       &
                                PROOT_LIN, PROOT_EXTINCTION, PSOILRC_SO2,           &
                                PSOILRC_O3, PRE25, PCE_NITRO, PCF_NITRO, PCNA_NITRO,&
                                PGMES_ST, PGC_ST, PBSLAI_ST, PSEFOLD_ST, PDMAX_ST  ,&
                                PGNDLITTER, PH_VEG, PZ0LITTER, OAGRI_TO_GRASS       )
!     #########################
!
!!**** *INI_DATA_PARAM* initializes secondary cover-field correspondance arrays
!!                      from VEGTYPE and LAI
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/01/2000
!!    F.solmon    01/06/2000 adaptation for patch approach: calculation of parameters 
!!                for each vegtypes of basic covers
!!    V Masson    03/04/2002 set RSMIN value to 120 for NVT_TROG and NVT_C4
!!    L Jarlan    15/10/2004 modify xdata_gmes following Gibelin
!!    P Le Moigne 09/2005 AGS modifs of L. Jarlan (duplicate arrays for ast, lst or nit options)
!!    S. Lafont      03/09 : change unit of RE25
!!    S. Faroux      03/09 : irrigated crops are assumed C4 crops
!!    S. Lafont      09/11 : Reco bare soil is 0; corrected comments
!!    B. Decharme    07/12 : Ponderation coefficient for cumulative root fraction of evergreen forest
!!    R. Alkama      05/12 : Add 7 new vegtype (19 rather than 12)
!!    B. Decharme    05/13 : new param for equatorial forest
!!    P. Samuelsson  10/14 : Multi-energy balance (MEB)
!!Seferian & Delire  06/15 : Updating Nitrogen content and coef (PCF,PCNA) and 
!                            mesophyl conductance based on TRY database (Kattge et al., GCB 2011) and Jacobs Thesis
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS,           ONLY : XDAY
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,   & 
                                NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                NVT_IRR, NVT_GRAS, NVT_TROG,NVT_PARK,   &
                                NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                NVT_C3S, NVT_FLTR, NVT_FLGR,            &
                                NVEGTYPE, JPCOVER
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_RSMIN
!
USE MODI_VEG_FROM_LAI
USE MODI_GREEN_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_EMIS_FROM_VEG
USE MODI_ABOR1_SFX
USE MODI_VEG_HEIGHT_FROM_LAI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLAI
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PH_TREE
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBNIR_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBVIS_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBUV_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRSMIN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRGL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGAMMA
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGMES
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGC
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PBSLAI
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSEFOLD
REAL, DIMENSION(:,:), INTENT(IN),  OPTIONAL :: PLAIMIN_IN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PLAIMIN_OUT
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PDMAX
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSTRESS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PF2I
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PVEG_IN
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PVEG_OUT
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PGREEN
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PZ0
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PZ0_O_Z0H
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PEMIS_ECO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PWRMAX_CF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PROOT_LIN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PROOT_EXTINCTION
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSOILRC_SO2
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSOILRC_O3
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRE25
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCE_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCF_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCNA_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGMES_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGC_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PBSLAI_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSEFOLD_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PDMAX_ST
!
LOGICAL, OPTIONAL, INTENT(IN) :: OAGRI_TO_GRASS
!
!            MEB parameters
!            --------------
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PH_VEG
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PZ0LITTER
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PGNDLITTER
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
LOGICAL            :: GAGRI_TO_GRASS
INTEGER            :: IVT_C3, IVT_C3W, IVT_C3S, IVT_PARK, IVT_FLTR, IVT_FLGR
INTEGER            :: JC                     ! class loop counter
!
INTEGER            :: JM                     ! month loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    7.     Secondary variables on natural covers
!            -------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM',0,ZHOOK_HANDLE)
!
GAGRI_TO_GRASS = .FALSE.
!
!When set, C3 and C4 crop values are replaced by value for C3 grass
IF(PRESENT(OAGRI_TO_GRASS))GAGRI_TO_GRASS=OAGRI_TO_GRASS
!
!-------------------------------------------------------------------------------
!*    7.5    albnir (veg only)
!            ------
IF (PRESENT(PALBNIR_VEG)) THEN
  PALBNIR_VEG(:,:)= 0.30
  PALBNIR_VEG(:,NVT_TEBD)= 0.25
  PALBNIR_VEG(:,NVT_TRBD)= 0.25
  PALBNIR_VEG(:,NVT_TEBE)= 0.25
  PALBNIR_VEG(:,NVT_BOBD)= 0.25
  PALBNIR_VEG(:,NVT_SHRB)= 0.25
  PALBNIR_VEG(:,NVT_BONE)= 0.15
  PALBNIR_VEG(:,NVT_TENE)= 0.15
  PALBNIR_VEG(:,NVT_BOND)= 0.15
  PALBNIR_VEG(:,NVT_TRBE)= 0.21
  IF (NVT_FLTR>0) THEN
     PALBNIR_VEG(:,NVT_FLTR) = 0.25
  ENDIF
ENDIF
!-------------------------------------------------------------------------------
!*    7.6    albvis (veg only)
!            ------
IF (PRESENT(PALBVIS_VEG)) THEN
  PALBVIS_VEG(:,:)= 0.10
  PALBVIS_VEG(:,NVT_TEBD)= 0.05
  PALBVIS_VEG(:,NVT_TRBD)= 0.05
  PALBVIS_VEG(:,NVT_TEBE)= 0.05
  PALBVIS_VEG(:,NVT_BOBD)= 0.05
  PALBVIS_VEG(:,NVT_SHRB)= 0.05
  PALBVIS_VEG(:,NVT_BONE)= 0.05
  PALBVIS_VEG(:,NVT_TENE)= 0.05
  PALBVIS_VEG(:,NVT_BOND)= 0.05
  PALBVIS_VEG(:,NVT_TRBE)= 0.05
  IF (NVT_FLTR>0) THEN
     PALBVIS_VEG(:,NVT_FLTR) = 0.05
  ENDIF
ENDIF        
!-------------------------------------------------------------------------------
!*    7.6    albUV (veg only)
!            -----
IF (PRESENT(PALBUV_VEG)) THEN
  !
  PALBUV_VEG(:,:)= 0.06  
  PALBUV_VEG(:,NVT_TEBD)= 0.0525
  PALBUV_VEG(:,NVT_TRBD)= 0.0525
  PALBUV_VEG(:,NVT_TEBE)= 0.0525
  PALBUV_VEG(:,NVT_BOBD)= 0.0525
  PALBUV_VEG(:,NVT_SHRB)= 0.0525
  PALBUV_VEG(:,NVT_BONE)= 0.0425
  PALBUV_VEG(:,NVT_TENE)= 0.0425
  PALBUV_VEG(:,NVT_BOND)= 0.0425
  PALBUV_VEG(:,NVT_TRBE)= 0.038 
  IF (NVT_FLTR>0) THEN
     PALBUV_VEG(:,NVT_FLTR) = 0.0525
  ENDIF
  !
  PALBUV_VEG(:,NVT_GRAS)= 0.08
  PALBUV_VEG(:,NVT_BOGR)= 0.08
  PALBUV_VEG(:,NVT_TROG)= 0.125
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PALBUV_VEG(:,NVT_C3  )= 0.08
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PALBUV_VEG(:,NVT_C3W )= 0.08
      PALBUV_VEG(:,NVT_C3S )= 0.08
    ENDIF
    PALBUV_VEG(:,NVT_C4  )= 0.08
    IF (NVT_IRR/=0) THEN
      PALBUV_VEG(:,NVT_IRR )= 0.08
    ENDIF
    !
  ELSE
    !
    IF (NVT_IRR/=0) THEN
      PALBUV_VEG(:,NVT_IRR )= 0.045
    ENDIF
    !
  ENDIF
  !
  IF (NVT_PARK/=0) THEN
    PALBUV_VEG(:,NVT_PARK)= 0.08
  ELSEIF (NVT_FLGR/=0) THEN
    PALBUV_VEG(:,NVT_FLGR)= 0.08
  ENDIF
  !
ENDIF        
!------------------------------------------------------------------------------
!*    7.7    Rsmin
!            -----
IF (PRESENT(PRSMIN)) THEN
  !
  PRSMIN(:,:)= 40.
  PRSMIN(:,NVT_TEBD)= 150.
  PRSMIN(:,NVT_TRBD)= 150.
  PRSMIN(:,NVT_TEBE)= 150.
  PRSMIN(:,NVT_BOBD)= 150.
  PRSMIN(:,NVT_SHRB)= 150.
  PRSMIN(:,NVT_BONE)= 150.
  PRSMIN(:,NVT_TENE)= 150.
  PRSMIN(:,NVT_BOND)= 150.
  !
  IF (NVT_FLTR>0) THEN
     PRSMIN(:,NVT_FLTR) = 150.
  ENDIF
  !
  PRSMIN(:,NVT_TRBE)= XEVERG_RSMIN
  PRSMIN(:,NVT_TROG)= 120.
  !
  IF(GAGRI_TO_GRASS)THEN
    PRSMIN(:,NVT_C4  )= 40.
    IF (NVT_IRR/=0) THEN
      PRSMIN(:,NVT_IRR )= 40.
    ENDIF
  ELSE
    PRSMIN(:,NVT_C4  )= 120.
    IF (NVT_IRR/=0) THEN
      PRSMIN(:,NVT_IRR )= 120.
    ENDIF
  ENDIF
  !
ENDIF
!-------------------------------------------------------------------------------
!*    7.8    Gamma
!            -----
IF (PRESENT(PGAMMA)) THEN
  PGAMMA(:,:)= 0. 
  PGAMMA(:,NVT_TEBD)= 0.04
  PGAMMA(:,NVT_TRBD)= 0.04
  PGAMMA(:,NVT_TEBE)= 0.04
  PGAMMA(:,NVT_BOBD)= 0.04
  PGAMMA(:,NVT_SHRB)= 0.04
  PGAMMA(:,NVT_BONE)= 0.04
  PGAMMA(:,NVT_TENE)= 0.04
  PGAMMA(:,NVT_BOND)= 0.04
  PGAMMA(:,NVT_TRBE)= 0.04
  IF (NVT_FLTR>0) THEN
     PGAMMA(:,NVT_FLTR) = 0.04
  ENDIF
ENDIF
!-------------------------------------------------------------------------------
!*    7.8    Wrmax_cf
!            --------
IF (PRESENT(PWRMAX_CF)) THEN
  PWRMAX_CF(:,:)= 0.2
  PWRMAX_CF(:,NVT_TEBD)= 0.1
  PWRMAX_CF(:,NVT_TRBD)= 0.1
  PWRMAX_CF(:,NVT_TEBE)= 0.1
  PWRMAX_CF(:,NVT_BOBD)= 0.1
  PWRMAX_CF(:,NVT_SHRB)= 0.1
  PWRMAX_CF(:,NVT_BONE)= 0.1
  PWRMAX_CF(:,NVT_TENE)= 0.1
  PWRMAX_CF(:,NVT_BOND)= 0.1
  PWRMAX_CF(:,NVT_TRBE)= 0.1
  IF (NVT_FLTR>0) THEN
     PWRMAX_CF(:,NVT_FLTR) = 0.1
  ENDIF
ENDIF
!-------------------------------------------------------------------------------
!*    7.9    Rgl
!            ---
IF (PRESENT(PRGL)) THEN
  PRGL(:,:)= 100.
  PRGL(:,NVT_TEBD)= 30.
  PRGL(:,NVT_TRBD)= 30.
  PRGL(:,NVT_TEBE)= 30.
  PRGL(:,NVT_BOBD)= 30.
  PRGL(:,NVT_SHRB)= 30.
  PRGL(:,NVT_BONE)= 30.
  PRGL(:,NVT_TENE)= 30.
  PRGL(:,NVT_BOND)= 30.
  PRGL(:,NVT_TRBE)= 30.
  IF (NVT_FLTR>0) THEN
     PRGL(:,NVT_FLTR) = 30.
  ENDIF
ENDIF    
!-------------------------------------------------------------------------------
!*    7.10   Cv
!            --
IF (PRESENT(PCV)) THEN
  PCV(:,:)=2.E-5
  PCV(:,NVT_TEBD)= 1.E-5
  PCV(:,NVT_TRBD)= 1.E-5
  PCV(:,NVT_TEBE)= 1.E-5
  PCV(:,NVT_BOBD)= 1.E-5
  PCV(:,NVT_SHRB)= 1.E-5
  PCV(:,NVT_BONE)= 1.E-5
  PCV(:,NVT_TENE)= 1.E-5
  PCV(:,NVT_BOND)= 1.E-5
  PCV(:,NVT_TRBE)= 1.E-5 
  IF (NVT_FLTR>0) THEN
     PCV(:,NVT_FLTR) = 1.E-5
  ENDIF
ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   mesophyll conductance (m s-1)
!            -----------------------------
!            Uptdated values using Kattge et al. 2009 median values of Vcmax at 25C
!            (For TRBE, used median + 1 standard deviation)
!            For C3 tree PFTs : 
!              gmes = Vcmax / (gamma + Kc*(1 + O2/Ko))    
!              from Jacobs eq [A8.5] and Farquhar, 1980 eq 42 : gm = dA/dC estimated at Ci=Gamma 
!            For grasses (C3 and C4): used V7 value
!                crops :  used N. Canal's PhD thesis 
!            --------------------------------------------------------------------
IF (PRESENT(PGMES)) THEN
  PGMES(:,:)=0.020
  PGMES(:,NVT_TEBD)= 0.001
  PGMES(:,NVT_TRBD)= 0.001
  PGMES(:,NVT_TEBE)= 0.001
  PGMES(:,NVT_BOBD)= 0.001
  PGMES(:,NVT_SHRB)= 0.001
  PGMES(:,NVT_BONE)= 0.001
  PGMES(:,NVT_TENE)= 0.001
  PGMES(:,NVT_BOND)= 0.001
  PGMES(:,NVT_TRBE)= 0.001 
  IF (NVT_FLTR>0) THEN
     PGMES(:,NVT_FLTR) = 0.001
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PGMES(:,NVT_C3  )= 0.020
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PGMES(:,NVT_C3W )= 0.020
      PGMES(:,NVT_C3S )= 0.020
    ENDIF
    PGMES(:,NVT_C4  )= 0.020
    IF (NVT_IRR/=0) THEN
      PGMES(:,NVT_IRR )= 0.020
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PGMES(:,NVT_C3  )= 0.003
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PGMES(:,NVT_C3W )= 0.003
      PGMES(:,NVT_C3S )= 0.003
    ENDIF
    PGMES(:,NVT_C4  )= 0.003
    IF (NVT_IRR/=0) THEN
      PGMES(:,NVT_IRR )= 0.003
    ENDIF
    !
  ENDIF
ENDIF    
!
IF (PRESENT(PGMES_ST)) THEN
  !
  PGMES_ST(:,:)=0.003
  PGMES_ST(:,NVT_TEBD)= 0.0018
  PGMES_ST(:,NVT_TRBD)= 0.0012
  PGMES_ST(:,NVT_TEBE)= 0.0019
  PGMES_ST(:,NVT_BOBD)= 0.0018
  PGMES_ST(:,NVT_SHRB)= 0.0016
  PGMES_ST(:,NVT_BONE)= 0.0019
  PGMES_ST(:,NVT_TENE)= 0.0019
  PGMES_ST(:,NVT_BOND)= 0.0012
  PGMES_ST(:,NVT_TRBE)= 0.0012
  IF (NVT_FLTR>0) THEN
     PGMES_ST(:,NVT_FLTR) = 0.0018
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PGMES_ST(:,NVT_C3  )= 0.001
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PGMES_ST(:,NVT_C3W )= 0.001
      PGMES_ST(:,NVT_C3S )= 0.001
    ENDIF
    PGMES_ST(:,NVT_C4  )= 0.006
    IF (NVT_IRR/=0) THEN
      PGMES_ST(:,NVT_IRR )= 0.006
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PGMES_ST(:,NVT_C3  )= 0.00175
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PGMES_ST(:,NVT_C3W )= 0.00175
      PGMES_ST(:,NVT_C3S )= 0.00175
    ENDIF
    PGMES_ST(:,NVT_C4  )= 0.0098
    IF (NVT_IRR/=0) THEN
      PGMES_ST(:,NVT_IRR )= 0.0098
    ENDIF
    !
  ENDIF
  !
  PGMES_ST(:,NVT_GRAS )= 0.001
  PGMES_ST(:,NVT_BOGR )= 0.001
  PGMES_ST(:,NVT_TROG )= 0.006
  !
  IF (NVT_PARK/=0) THEN
    PGMES_ST(:,NVT_PARK)= 0.001
  ELSEIF (NVT_FLGR/=0) THEN
    PGMES_ST(:,NVT_FLGR)= 0.001
  ENDIF
  !
ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   Ecosystem Respiration (kg m-2 s-1)
!            -----------------------------------
IF (PRESENT(PRE25)) THEN
  !
  PRE25(:,:)= 3.6E-7
  PRE25(:,NVT_BONE)= 1.8E-7
  PRE25(:,NVT_TENE)= 1.8E-7
  PRE25(:,NVT_BOND)= 1.8E-7
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PRE25(:,NVT_C3  )= 3.6E-7
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PRE25(:,NVT_C3W )= 3.6E-7
      PRE25(:,NVT_C3S )= 3.6E-7
    ENDIF
    PRE25(:,NVT_C4  )= 3.6E-7
    IF (NVT_IRR/=0) THEN
      PRE25(:,NVT_IRR )= 3.6E-7    
    ENDIF
    !        
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PRE25(:,NVT_C3  )= 3.6E-7
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PRE25(:,NVT_C3W )= 3.6E-7
      PRE25(:,NVT_C3S )= 3.6E-7
    ENDIF
    PRE25(:,NVT_C4  )= 3.0E-7
    IF (NVT_IRR/=0) THEN
      PRE25(:,NVT_IRR )= 3.0E-7
    ENDIF
    !
  ENDIF
  !
  !ecosystem respiration only if vegetetation is present
  PRE25(:,NVT_NO  )= 0.
  PRE25(:,NVT_ROCK)= 0.
  PRE25(:,NVT_SNOW)= 0.
  !
ENDIF
!-------------------------------------------------------------------------------
!*    7.11   cuticular conductance (m s-1)
!            -----------------------------
IF (PRESENT(PGC)) THEN
  PGC(:,:)=0.00025
  PGC(:,NVT_TEBD)= 0.00015
  PGC(:,NVT_TRBD)= 0.00015
  PGC(:,NVT_TEBE)= 0.00015
  PGC(:,NVT_BOBD)= 0.00015
  PGC(:,NVT_SHRB)= 0.00015
  PGC(:,NVT_BONE)= 0.
  PGC(:,NVT_TENE)= 0.
  PGC(:,NVT_BOND)= 0.
  PGC(:,NVT_TRBE)= 0.00015    
  IF (NVT_FLTR>0) THEN
     PGC(:,NVT_FLTR) = 0.00015
  ENDIF  
ENDIF
!
IF (PRESENT(PGC_ST)) THEN
  PGC_ST(:,:)=0.00015
  PGC_ST(:,NVT_BONE)= 0.
  PGC_ST(:,NVT_TENE)= 0.
  PGC_ST(:,NVT_BOND)= 0.
  !
  IF (NVT_C3/=0) THEN
    PGC_ST(:,NVT_C3  )= 0.00025
  ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
    PGC_ST(:,NVT_C3W )= 0.00025
    PGC_ST(:,NVT_C3S )= 0.00025
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    PGC_ST(:,NVT_C4  )= 0.00025
    IF (NVT_IRR/=0) THEN
      PGC_ST(:,NVT_IRR )= 0.00025
    ENDIF
  ENDIF
  ! 
  PGC_ST(:,NVT_GRAS)= 0.00025
  PGC_ST(:,NVT_BOGR)= 0.00025  
  !    
  IF (NVT_PARK/=0) THEN
    PGC_ST(:,NVT_PARK)= 0.001
  ELSEIF (NVT_FLGR/=0) THEN
    PGC_ST(:,NVT_FLGR)= 0.001
  ENDIF 
  !
ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   critical normilized soil water content for stress parameterisation
!            ------------------------------------------------------------------
IF (PRESENT(PF2I)) PF2I(:,:)=0.3
!-------------------------------------------------------------------------------
!*    7.12   ratio d(biomass)/d(lai) (kg/m2)
!            -----------------------
IF (PRESENT(PBSLAI)) THEN
  PBSLAI(:,:)=0.36
  PBSLAI(:,NVT_TEBD)= 0.25
  PBSLAI(:,NVT_TRBD)= 0.25
  PBSLAI(:,NVT_TEBE)= 0.25
  PBSLAI(:,NVT_BOBD)= 0.25
  PBSLAI(:,NVT_SHRB)= 0.25
  PBSLAI(:,NVT_BONE)= 0.25
  PBSLAI(:,NVT_TENE)= 0.25
  PBSLAI(:,NVT_BOND)= 0.25
  PBSLAI(:,NVT_TRBE)= 0.25 
  IF (NVT_FLTR>0) THEN
     PBSLAI(:,NVT_FLTR) = 0.25
  ENDIF 
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PBSLAI(:,NVT_C3  )= 0.36
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PBSLAI(:,NVT_C3W )= 0.36
      PBSLAI(:,NVT_C3S )= 0.36
    ENDIF
    PBSLAI(:,NVT_C4  )= 0.36
    IF (NVT_IRR/=0) THEN
      PBSLAI(:,NVT_IRR )= 0.36  
    ENDIF 
    !        
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PBSLAI(:,NVT_C3  )= 0.06
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PBSLAI(:,NVT_C3W )= 0.06
      PBSLAI(:,NVT_C3S )= 0.06
    ENDIF
    PBSLAI(:,NVT_C4  )= 0.06
    IF (NVT_IRR/=0) THEN
      PBSLAI(:,NVT_IRR )= 0.06 
    ENDIF
    !
  ENDIF
  !
ENDIF
!    
IF (PRESENT(PBSLAI_ST)) THEN
  !
  PBSLAI_ST(:,:)=0.08 
  PBSLAI_ST(:,NVT_TEBD)= 0.125
  PBSLAI_ST(:,NVT_TRBD)= 0.125
  PBSLAI_ST(:,NVT_TEBE)= 0.125
  PBSLAI_ST(:,NVT_BOBD)= 0.125
  PBSLAI_ST(:,NVT_SHRB)= 0.125
  PBSLAI_ST(:,NVT_BONE)= 0.50
  PBSLAI_ST(:,NVT_TENE)= 0.50
  PBSLAI_ST(:,NVT_BOND)= 0.50
  PBSLAI_ST(:,NVT_TRBE)= 0.25 
  IF (NVT_FLTR>0) THEN
     PBSLAI_ST(:,NVT_FLTR) = 0.125
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PBSLAI_ST(:,NVT_C3  )= 0.08
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PBSLAI_ST(:,NVT_C3W )= 0.08
      PBSLAI_ST(:,NVT_C3S )= 0.08
    ENDIF
    PBSLAI_ST(:,NVT_C4  )= 0.08
    IF (NVT_IRR/=0) THEN
      PBSLAI_ST(:,NVT_IRR )= 0.08
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PBSLAI_ST(:,NVT_C3  )= 0.06
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PBSLAI_ST(:,NVT_C3W )= 0.06
      PBSLAI_ST(:,NVT_C3S )= 0.06
    ENDIF
    PBSLAI_ST(:,NVT_C4  )= 0.06
    IF (NVT_IRR/=0) THEN
      PBSLAI_ST(:,NVT_IRR )= 0.06
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!*    7.12   maximum air saturation deficit tolerate by vegetation (kg/kg)
!            -----------------------------------------------------
IF (PRESENT(PDMAX)) THEN
  PDMAX(:,:) = 0.1
  PDMAX(:,NVT_TEBD)= 0.01
  PDMAX(:,NVT_TRBD)= 0.01
  PDMAX(:,NVT_TEBE)= 0.01
  PDMAX(:,NVT_BOBD)= 0.01
  PDMAX(:,NVT_SHRB)= 0.01
  PDMAX(:,NVT_BONE)= 0.01
  PDMAX(:,NVT_TENE)= 0.01
  PDMAX(:,NVT_BOND)= 0.01
  PDMAX(:,NVT_TRBE)= 0.01 
  IF (NVT_FLTR>0) THEN
     PDMAX(:,NVT_FLTR) = 0.01
  ENDIF
ENDIF    
!
IF (PRESENT(PDMAX_ST)) THEN
  !
  PDMAX_ST(:,:) = 0.05
  PDMAX_ST(:,NVT_TEBD)= 0.109
  PDMAX_ST(:,NVT_TRBD)= 0.109
  PDMAX_ST(:,NVT_TEBE)= 0.109
  PDMAX_ST(:,NVT_BOBD)= 0.109
  PDMAX_ST(:,NVT_SHRB)= 0.109
  PDMAX_ST(:,NVT_BONE)= 0.124
  PDMAX_ST(:,NVT_TENE)= 0.124
  PDMAX_ST(:,NVT_BOND)= 0.124
  PDMAX_ST(:,NVT_TRBE)= 0.124 
  IF (NVT_FLTR>0) THEN
     PDMAX_ST(:,NVT_FLTR) = 0.109
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    PDMAX_ST(:,NVT_C4  )= 0.05
    IF (NVT_IRR/=0) THEN
      PDMAX_ST(:,NVT_IRR )= 0.05
    ENDIF
    !
  ELSE
    !
    PDMAX_ST(:,NVT_C4  )= 0.033
    IF (NVT_IRR/=0) THEN
      PDMAX_ST(:,NVT_IRR )= 0.033
    ENDIF
    !
  ENDIF  
  !  
  PDMAX_ST(:,NVT_TROG)= 0.052
  !
ENDIF    
!-------------------------------------------------------------------------------
!*    7.12   Defensive/offensive strategy (1/0)
!            ----------------------------
IF (PRESENT(PSTRESS)) THEN
  !
  PSTRESS(:,:) = 1. 
  PSTRESS(:,NVT_TEBD)= 0.
  PSTRESS(:,NVT_TRBD)= 0.
  PSTRESS(:,NVT_TEBE)= 0.
  PSTRESS(:,NVT_BOBD)= 0.
  PSTRESS(:,NVT_SHRB)= 0.
  PSTRESS(:,NVT_TRBE)= 0.
  IF (NVT_FLTR>0) THEN
     PSTRESS(:,NVT_FLTR) = 0.
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PSTRESS(:,NVT_C3  )= 0.
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSTRESS(:,NVT_C3W )= 0.
      PSTRESS(:,NVT_C3S )= 0.
    ENDIF
    IF (NVT_IRR/=0) THEN
      PSTRESS(:,NVT_IRR )= 0.
    ENDIF
    !
  ENDIF    
  !
  PSTRESS(:,NVT_C4  )= 0.
  !
  PSTRESS(:,NVT_GRAS)= 0.
  PSTRESS(:,NVT_BOGR)= 0.      
  PSTRESS(:,NVT_TROG)= 0.
  !
  IF (NVT_PARK/=0) THEN
    PSTRESS(:,NVT_PARK)= 0.
  ELSEIF (NVT_FLGR/=0) THEN
    PSTRESS(:,NVT_FLGR)= 0.
  ENDIF 
  !
ENDIF    
!-------------------------------------------------------------------------------
!*    7.13   e-folding time for senescence (days)
!            ------------------------------------
! parameters use in case HPHOTO == 'NONE'
IF (PRESENT(PSEFOLD)) THEN
  !
  PSEFOLD(:,:)=90. * XDAY
  PSEFOLD(:,NVT_TEBD)= 365.* XDAY
  PSEFOLD(:,NVT_TRBD)= 365.* XDAY
  PSEFOLD(:,NVT_TEBE)= 365.* XDAY
  PSEFOLD(:,NVT_BOBD)= 365.* XDAY
  PSEFOLD(:,NVT_SHRB)= 365.* XDAY
  PSEFOLD(:,NVT_BONE)= 365.* XDAY
  PSEFOLD(:,NVT_TENE)= 365.* XDAY
  PSEFOLD(:,NVT_BOND)= 365.* XDAY
  PSEFOLD(:,NVT_TRBE)= 365.* XDAY
  IF (NVT_FLTR>0) THEN
     PSEFOLD(:,NVT_FLTR) = 365.* XDAY
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PSEFOLD(:,NVT_C3  )= 90.* XDAY
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSEFOLD(:,NVT_C3W )= 90.* XDAY
      PSEFOLD(:,NVT_C3S )= 90.* XDAY
    ENDIF
    !
    PSEFOLD(:,NVT_C4  )= 90.* XDAY
    IF (NVT_IRR/=0) THEN
      PSEFOLD(:,NVT_IRR )= 90.* XDAY
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PSEFOLD(:,NVT_C3  )= 60.* XDAY
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSEFOLD(:,NVT_C3W )= 60.* XDAY
      PSEFOLD(:,NVT_C3S )= 60.* XDAY
    ENDIF
    !
    PSEFOLD(:,NVT_C4  )= 60.* XDAY
    IF (NVT_IRR/=0) THEN
      PSEFOLD(:,NVT_IRR )= 60.* XDAY
    ENDIF
    !
  ENDIF  
  !  
ENDIF    
!
! parameters use in case HPHOTO == 'AST', 'NIT', 'NCB'

IF (PRESENT(PSEFOLD_ST)) THEN
  PSEFOLD_ST(:,:)=150. * XDAY
  PSEFOLD_ST(:,NVT_TEBD)= 230.* XDAY
  PSEFOLD_ST(:,NVT_TRBD)= 230.* XDAY
  PSEFOLD_ST(:,NVT_TEBE)= 230.* XDAY
  PSEFOLD_ST(:,NVT_BOBD)= 230.* XDAY
  PSEFOLD_ST(:,NVT_SHRB)= 230.* XDAY
  PSEFOLD_ST(:,NVT_BONE)= 365.* XDAY
  PSEFOLD_ST(:,NVT_TENE)= 365.* XDAY
  PSEFOLD_ST(:,NVT_BOND)= 365.* XDAY
  PSEFOLD_ST(:,NVT_TRBE)= 365.* XDAY
  IF (NVT_FLTR>0) THEN
     PSEFOLD_ST(:,NVT_FLTR) = 365.* XDAY
  ENDIF
ENDIF    
!-------------------------------------------------------------------------------
!*    7.14   Minimum LAI (m2/m2)
!            -------------------
! Modi lai/patch defined
IF (PRESENT(PLAIMIN_OUT)) THEN
  PLAIMIN_OUT (:,:) = 0.3
  PLAIMIN_OUT(:,NVT_BONE)= 1.0
  PLAIMIN_OUT(:,NVT_TENE)= 1.0
  PLAIMIN_OUT(:,NVT_BOND)= 1.0
  PLAIMIN_OUT(:,NVT_TRBE)= 1.0
ENDIF 
!--------------------------------------------------------------------
!
!*    7.16   Fraction of ground litter
!            -------------------------
! 	 
IF (PRESENT(PGNDLITTER)) THEN
  PGNDLITTER (:,:,:) = 0.
  PGNDLITTER (:,:,NVT_TEBD) = 0.03
  PGNDLITTER (:,:,NVT_BONE) = 0.03
  PGNDLITTER (:,:,NVT_TRBE) = 0.03
  PGNDLITTER (:,:,NVT_TRBD) = 0.03
  PGNDLITTER (:,:,NVT_TEBE) = 0.03
  PGNDLITTER (:,:,NVT_TENE) = 0.03
  PGNDLITTER (:,:,NVT_BOBD) = 0.03
  PGNDLITTER (:,:,NVT_BOND) = 0.03
  PGNDLITTER (:,:,NVT_SHRB) = 0.03
ENDIF 
! 
!------------------------------------------------------------------------
!*    2.20   specific leaf area sensitivity to nitrogen concentration
!            -----------------------------
!            corresponds to "e" in (eq 1) from Gibelin et al, 2006 
!            SLA = f + e * Nm   with SLA = specific leaf area
!            kept values from Gibelin et al 2006 
!            -----------------------------------------------------
!
IF (PRESENT(PCE_NITRO)) THEN
  !
  PCE_NITRO(:,:)=7.68
  PCE_NITRO(:,NVT_TEBD)= 4.83
  PCE_NITRO(:,NVT_TRBD)= 4.83
  PCE_NITRO(:,NVT_TEBE)= 4.83
  PCE_NITRO(:,NVT_BOBD)= 4.83
  PCE_NITRO(:,NVT_SHRB)= 4.83
  PCE_NITRO(:,NVT_BONE)= 4.85
  PCE_NITRO(:,NVT_TENE)= 4.85
  PCE_NITRO(:,NVT_BOND)= 4.85
  PCE_NITRO(:,NVT_TRBE)= 4.83
  IF (NVT_FLTR>0) THEN
     PCE_NITRO(:,NVT_FLTR) = 4.83
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PCE_NITRO(:,NVT_C3  )= 5.56
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PCE_NITRO(:,NVT_C3W )= 5.56
      PCE_NITRO(:,NVT_C3S )= 5.56
    ENDIF
    PCE_NITRO(:,NVT_C4  )= 5.56
    IF (NVT_IRR/=0) THEN
      PCE_NITRO(:,NVT_IRR )= 5.56
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PCE_NITRO(:,NVT_C3  )= 3.79
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PCE_NITRO(:,NVT_C3W )= 3.79
      PCE_NITRO(:,NVT_C3S )= 3.79
    ENDIF
    PCE_NITRO(:,NVT_C4  )= 7.68
    IF (NVT_IRR/=0) THEN
      PCE_NITRO(:,NVT_IRR )= 7.68
    ENDIF
    !
  ENDIF
  !
  PCE_NITRO(:,NVT_GRAS)= 5.56
  PCE_NITRO(:,NVT_BOGR)= 5.56 
  !  
  IF (NVT_PARK/=0) THEN
    PCE_NITRO(:,NVT_PARK)= 5.56
  ELSEIF (NVT_FLGR/=0) THEN
    PCE_NITRO(:,NVT_FLGR)= 5.56
  ENDIF   
  ! 
ENDIF
!
!-------------------------------------------------------------------------------
!*    2.21   lethal minimum value of leaf area ratio
!            ----------
!            intercept of SLA = f + e * Nm  from Gibelin et al, 2006 (eq 1)
!            kept Gibelin et al values for grasses and crops
!            used TRY database (Kattge et al., 2011) median values for trees                    
!            with SLA and Nm from TRY and "e" (PCE_NITRO) from Gibelin et al 2006 
!            used Domingues 2011 for TRBE SLA.
!            ------------------------------------------------------
IF (PRESENT(PCF_NITRO)) THEN
  !
  PCF_NITRO(:,:)=-4.33
  PCF_NITRO(:,NVT_TEBD)= 5.11
  PCF_NITRO(:,NVT_TRBD)= 5.11
  PCF_NITRO(:,NVT_TEBE)= 0.17
  PCF_NITRO(:,NVT_BOBD)= 5.11
  PCF_NITRO(:,NVT_SHRB)= 4.98
  PCF_NITRO(:,NVT_BONE)= -0.87
  PCF_NITRO(:,NVT_TENE)= -0.87
  PCF_NITRO(:,NVT_BOND)= 0.68
  PCF_NITRO(:,NVT_TRBE)= 0.12 ! obtained using f = SLA - e*Nm 
                                                                 ! with SLA = 8.33 m2/kg_DM (Domingues 2011), Nm=1.7% (TRY)
  IF (NVT_FLTR>0) THEN
     PCF_NITRO(:,NVT_FLTR) = 5.11
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PCF_NITRO(:,NVT_C3  )= 6.73
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PCF_NITRO(:,NVT_C3W )= 6.73
      PCF_NITRO(:,NVT_C3S )= 6.73
    ENDIF
    PCF_NITRO(:,NVT_C4  )= 6.73
    IF (NVT_IRR/=0) THEN
      PCF_NITRO(:,NVT_IRR )= 6.73
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PCF_NITRO(:,NVT_C3  )= 9.84
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PCF_NITRO(:,NVT_C3W )= 9.84
      PCF_NITRO(:,NVT_C3S )= 9.84
    ENDIF
    PCF_NITRO(:,NVT_C4  )= -4.33
    IF (NVT_IRR/=0) THEN
      PCF_NITRO(:,NVT_IRR )= -4.33
    ENDIF
    !
  ENDIF
  !
  PCF_NITRO(:,NVT_GRAS)= 6.73
  PCF_NITRO(:,NVT_BOGR)= 6.73    
  !
  IF (NVT_PARK/=0) THEN
    PCF_NITRO(:,NVT_PARK)= 6.73
  ELSEIF (NVT_FLGR/=0) THEN
    PCF_NITRO(:,NVT_FLGR)= 6.73
  ENDIF  
  ! 
ENDIF    
!-------------------------------------------------------------------------------
!*    2.22   nitrogen concentration of leaf biomass
!            ----------
!            kept Gibelin et al 2006 values for grasses and crops
!            Nm from TRY database (Kattge et al. GCB 2011) median values for tree PFTs
!            Nm in mg_N/g_DM and PCNA_NITRO in % --> PCNA_NITRO = Nm * 0.1 
!            --------------------------------------------------
IF (PRESENT(PCNA_NITRO)) THEN
  !
  PCNA_NITRO(:,:)=1.3
  PCNA_NITRO(:,NVT_TEBD)= 2.13
  PCNA_NITRO(:,NVT_TRBD)= 2.13
  PCNA_NITRO(:,NVT_TEBE)= 1.69
  PCNA_NITRO(:,NVT_BOBD)= 2.13
  PCNA_NITRO(:,NVT_SHRB)= 2.15
  PCNA_NITRO(:,NVT_BONE)= 1.21
  PCNA_NITRO(:,NVT_TENE)= 1.21
  PCNA_NITRO(:,NVT_BOND)= 1.94
  PCNA_NITRO(:,NVT_TRBE)= 1.7 
  IF (NVT_FLTR>0) THEN
     PCNA_NITRO(:,NVT_FLTR) = 2.13
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    PCNA_NITRO(:,NVT_C4  )= 1.3
    IF (NVT_IRR/=0) THEN
      PCNA_NITRO(:,NVT_IRR) = 1.3
    ENDIF
  ELSE
    PCNA_NITRO(:,NVT_C4  )= 1.9
    IF (NVT_IRR/=0) THEN
      PCNA_NITRO(:,NVT_IRR) = 1.9
    ENDIF
  ENDIF
  !
  IF (NVT_C3/=0) THEN
    PCNA_NITRO(:,NVT_C3  )= 1.3
  ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
    PCNA_NITRO(:,NVT_C3W )= 1.3
    PCNA_NITRO(:,NVT_C3S )= 1.3
  ENDIF
  !
  PCNA_NITRO(:,NVT_GRAS)= 1.3 
  PCNA_NITRO(:,NVT_BOGR)= 1.3
  !
  IF (NVT_PARK/=0) THEN
    PCNA_NITRO(:,NVT_PARK)= 1.3
  ELSEIF (NVT_FLGR/=0) THEN
    PCNA_NITRO(:,NVT_FLGR)= 1.3
  ENDIF  
  !  
ENDIF    
!-------------------------------------------------------------------------------
!*    7.15   Jackson (1996) coefficient for cumulative root fraction
!            -------------------------------------------------------
IF (PRESENT(PROOT_EXTINCTION)) THEN
  !
  PROOT_EXTINCTION(:,:)= 0.943 ! (default value)
  PROOT_EXTINCTION(:,NVT_TEBD)= 0.966
  PROOT_EXTINCTION(:,NVT_TRBD)= 0.961
  PROOT_EXTINCTION(:,NVT_TEBE)= 0.966
  PROOT_EXTINCTION(:,NVT_SHRB)= 0.964
  PROOT_EXTINCTION(:,NVT_TENE)= 0.976
  PROOT_EXTINCTION(:,NVT_TRBE)= 0.962
  IF (NVT_FLTR>0) THEN
     PROOT_EXTINCTION(:,NVT_FLTR) = 0.966
  ENDIF
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PROOT_EXTINCTION(:,NVT_C3  )= 0.943
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PROOT_EXTINCTION(:,NVT_C3W )= 0.943
      PROOT_EXTINCTION(:,NVT_C3S )= 0.943
    ENDIF
    PROOT_EXTINCTION(:,NVT_C4  )= 0.943
    IF (NVT_IRR/=0) THEN
      PROOT_EXTINCTION(:,NVT_IRR )= 0.943
    ENDIF  
    !        
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PROOT_EXTINCTION(:,NVT_C3  )= 0.961
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PROOT_EXTINCTION(:,NVT_C3W )= 0.961
      PROOT_EXTINCTION(:,NVT_C3S )= 0.961
    ENDIF
    PROOT_EXTINCTION(:,NVT_C4  )= 0.972
    IF (NVT_IRR/=0) THEN
      PROOT_EXTINCTION(:,NVT_IRR )= 0.972
    ENDIF
    !
  ENDIF
  !
  PROOT_EXTINCTION(:,NVT_BOGR)= 0.914      
  PROOT_EXTINCTION(:,NVT_TROG)= 0.972
  !
ENDIF
!
!-------------------------------------------------------------------------------
!*    7.16   Ponderation coefficient between formulations for cumulative root fraction
!            -------------------------------------------------------------------------
!
IF (PRESENT(PROOT_LIN)) THEN
  PROOT_LIN(:,:)= 0.05
  PROOT_LIN(:,NVT_TRBE)= 0.25
ENDIF
!
!-------------------------------------------------------------------------------
!*    7.17   Coefficient for chemistry deposition of SO2
!            -------------------------------------------
IF (PRESENT(PSOILRC_SO2)) THEN
  !
  PSOILRC_SO2(:,:)= 9999.
  PSOILRC_SO2(:,NVT_TEBD)= 500.
  PSOILRC_SO2(:,NVT_TRBD)= 500.
  PSOILRC_SO2(:,NVT_TEBE)= 500.
  PSOILRC_SO2(:,NVT_BOBD)= 500.
  PSOILRC_SO2(:,NVT_SHRB)= 500.
  PSOILRC_SO2(:,NVT_BONE)= 500.
  PSOILRC_SO2(:,NVT_TENE)= 500.
  PSOILRC_SO2(:,NVT_BOND)= 500.
  PSOILRC_SO2(:,NVT_TRBE)= 200. 
  IF (NVT_FLTR>0) THEN
     PSOILRC_SO2(:,NVT_FLTR) = 500.
  ENDIF  
  ! 
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PSOILRC_SO2(:,NVT_C3  )= 350.
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSOILRC_SO2(:,NVT_C3W )= 350.
      PSOILRC_SO2(:,NVT_C3S )= 350.
    ENDIF
    PSOILRC_SO2(:,NVT_C4  )= 350.
    IF (NVT_IRR/=0) THEN
      PSOILRC_SO2(:,NVT_IRR )= 350.
    ENDIF
    !
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PSOILRC_SO2(:,NVT_C3  )= 150.
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSOILRC_SO2(:,NVT_C3W )= 150.
      PSOILRC_SO2(:,NVT_C3S )= 150.
    ENDIF
    PSOILRC_SO2(:,NVT_C4  )= 150.
    IF (NVT_IRR/=0) THEN
      PSOILRC_SO2(:,NVT_IRR )= 0.001
    ENDIF
    !
  ENDIF
  !
  PSOILRC_SO2(:,NVT_GRAS)= 350.
  PSOILRC_SO2(:,NVT_BOGR)= 350. 
  !    
  IF (NVT_PARK/=0) THEN
    PSOILRC_SO2(:,NVT_PARK)= 350.
  ELSEIF (NVT_FLGR/=0) THEN
    PSOILRC_SO2(:,NVT_FLGR)= 350.
  ENDIF  
  !
  PSOILRC_SO2(:,NVT_TROG)= 350.
  PSOILRC_SO2(:,NVT_NO  )=1000.
  PSOILRC_SO2(:,NVT_ROCK)= 400.
  PSOILRC_SO2(:,NVT_SNOW)= 100.
  !
ENDIF
!
!------------------------------------------------------------------------------
!*    7.18   Coefficient for chemistry deposition of O3
!            ------------------------------------------
IF (PRESENT(PSOILRC_O3)) THEN
  !
  PSOILRC_O3(:,:)= 9999.
  PSOILRC_O3(:,NVT_TEBD)= 200.
  PSOILRC_O3(:,NVT_TRBD)= 200.
  PSOILRC_O3(:,NVT_TEBE)= 200.
  PSOILRC_O3(:,NVT_BOBD)= 200.
  PSOILRC_O3(:,NVT_SHRB)= 200.
  PSOILRC_O3(:,NVT_BONE)= 200.
  PSOILRC_O3(:,NVT_TENE)= 200.
  PSOILRC_O3(:,NVT_BOND)= 200.
  PSOILRC_O3(:,NVT_TRBE)= 500.
  IF (NVT_FLTR>0) THEN
     PSOILRC_O3(:,NVT_FLTR) = 200.
  ENDIF 
  !
  IF(GAGRI_TO_GRASS)THEN
    !
    IF (NVT_C3/=0) THEN
      PSOILRC_O3(:,NVT_C3  )= 200.
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSOILRC_O3(:,NVT_C3W )= 200.
      PSOILRC_O3(:,NVT_C3S )= 200.
    ENDIF
    PSOILRC_O3(:,NVT_C4  )= 200.
    IF (NVT_IRR/=0) THEN
      PSOILRC_O3(:,NVT_IRR )= 200.  
    ENDIF  
    !        
  ELSE
    !
    IF (NVT_C3/=0) THEN
      PSOILRC_O3(:,NVT_C3  )= 150.
    ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
      PSOILRC_O3(:,NVT_C3W )= 150.
      PSOILRC_O3(:,NVT_C3S )= 150.
    ENDIF
    PSOILRC_O3(:,NVT_C4  )= 150.
    IF (NVT_IRR/=0) THEN
      PSOILRC_O3(:,NVT_IRR )=1000.
    ENDIF
    !
  ENDIF
  !
  PSOILRC_O3(:,NVT_GRAS)= 200.
  PSOILRC_O3(:,NVT_BOGR)= 200.  
  !  
  IF (NVT_PARK/=0) THEN
    PSOILRC_O3(:,NVT_PARK)= 200.
  ELSEIF (NVT_FLGR/=0) THEN
    PSOILRC_O3(:,NVT_FLGR)= 200.
  ENDIF   
  !
  PSOILRC_O3(:,NVT_TROG)= 200.
  PSOILRC_O3(:,NVT_NO  )= 400.
  PSOILRC_O3(:,NVT_ROCK)= 200.
  PSOILRC_O3(:,NVT_SNOW)=3500.
  !
ENDIF
!-------------------------------------------------------------------------------
!*    7.15   vegetation and greeness fractions
!            ---------------------------------
IF (PRESENT(PVEG_OUT) .AND. PRESENT(PLAI)) THEN
  DO JM=1,SIZE(PVEG_OUT,2)
    DO JC = 1,SIZE(PVEG_OUT,1)
      PVEG_OUT(JC,JM,:) = VEG_FROM_LAI(PLAI(JC,JM,:),GAGRI_TO_GRASS)        
    ENDDO 
  ENDDO
ELSEIF (PRESENT(PVEG_OUT) .AND. .NOT. PRESENT(PLAI)) THEN
 CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PVEG_OUT, PLAI MUST BE IN ARGUMENTS TOO") 
ENDIF
! 
     
IF (PRESENT(PGREEN) .AND. PRESENT(PLAI)) THEN
  DO JM=1,SIZE(PGREEN,2)
    DO JC = 1,SIZE(PGREEN,1)
      PGREEN(JC,JM,:) = GREEN_FROM_LAI(PLAI(JC,JM,:),GAGRI_TO_GRASS)  
    ENDDO
  ENDDO
ELSEIF (PRESENT(PGREEN) .AND. .NOT. PRESENT(PLAI)) THEN
 CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PGREEN, PLAI MUST BE IN ARGUMENTS TOO")
ENDIF     
!-------------------------------------------------------------------------------
!*    7.16   z0
!            --
IF (PRESENT(PZ0) .AND. PRESENT(PLAI) .AND. PRESENT(PH_TREE)) THEN
  DO JM=1,SIZE(PZ0,2)
    DO JC = 1,SIZE(PZ0,1)
      PZ0(JC,JM,:) = Z0V_FROM_LAI(PLAI(JC,JM,:),PH_TREE(JC,:),GAGRI_TO_GRASS)  
    ENDDO
  ENDDO
ELSEIF (PRESENT(PZ0) .AND. (.NOT. PRESENT(PLAI) .OR. .NOT. PRESENT(PH_TREE))) THEN
  CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PZ0, PLAI AND PH_TREE MUST BE IN ARGUMENTS TOO")
ENDIF   
!-------------------------------------------------------------------------------
!*    7.17   z0/z0h
!            ------
IF (PRESENT(PZ0_O_Z0H)) PZ0_O_Z0H (:,:) = 10.
!-------------------------------------------------------------------------------
!*    7.18   emissivity
!            ----------
IF (PRESENT(PEMIS_ECO) .AND. (PRESENT(PVEG_IN).OR.PRESENT(PVEG_OUT))) THEN
  DO JM=1,SIZE(PEMIS_ECO,2)
    DO JC = 1,SIZE(PEMIS_ECO,1)
      IF (PRESENT(PVEG_OUT)) THEN
        PEMIS_ECO(JC,JM,:) = EMIS_FROM_VEG(PVEG_OUT(JC,JM,:))  
      ELSEIF (PRESENT(PVEG_IN)) THEN
        PEMIS_ECO(JC,JM,:) = EMIS_FROM_VEG(PVEG_IN(JC,JM,:))  
      ENDIF
    ENDDO
  ENDDO
ELSEIF (PRESENT(PEMIS_ECO) .AND. .NOT.PRESENT(PVEG_IN) .AND. .NOT.PRESENT(PVEG_OUT)) THEN
  CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PEMIS_ECO, PVEG_IN OR PVEG_OUT MUST BE IN ARGUMENTS TOO")
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    7.19   vegetation height
!            -----------------
!
IF (PRESENT(PH_VEG) .AND. PRESENT(PLAI) .AND. PRESENT(PH_TREE)) THEN
  DO JM=1,SIZE(PH_VEG,2)
    DO JC = 1,SIZE(PH_VEG,1)
      PH_VEG(JC,JM,:) = VEG_HEIGHT_FROM_LAI(PLAI(JC,JM,:),PH_TREE(JC,:),GAGRI_TO_GRASS )  
    ENDDO
  ENDDO
ELSEIF (PRESENT(PH_VEG) .AND. (.NOT. PRESENT(PLAI) .OR. .NOT. PRESENT(PH_TREE))) THEN
  CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PH_VEG, PLAI AND PH_TREE MUST BE IN ARGUMENTS TOO")
ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.21   z0 understory litter
!            --------------------
! 
IF (PRESENT(PZ0LITTER)) THEN
  PZ0LITTER(:,:,:)  = 0.013 ! Roughness for bare soil
ENDIF   
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_PARAM
