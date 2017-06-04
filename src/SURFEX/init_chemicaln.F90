!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_CHEMICAL_n(KLUOUT, KSV, HSV, SV, HCH_NAMES, HAER_NAMES, &
                           HDSTNAMES, HSLTNAMES     )  
!#############################################################
!
!!****  *INIT_CHEMICAL_n* - routine to initialize CHEMICAL SPECIES
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
!!      Original    08/2011
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_CHS_AEROSOL,    ONLY: LVARSIGI, LVARSIGJ
USE MODD_DST_SURF,       ONLY: LVARSIG_DST, NDSTMDE,  NDST_MDEBEG, LRGFIX_DST, JPMODE_DST
USE MODD_SLT_SURF,       ONLY: LVARSIG_SLT, NSLTMDE,  NSLT_MDEBEG, LRGFIX_SLT, JPMODE_SLT
!
USE MODI_CH_INIT_NAMES
USE MODI_DSLT_INIT_NAMES
USE MODI_DSLT_INIT_MODES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                          INTENT(IN) :: KLUOUT
INTEGER,                          INTENT(IN) :: KSV      ! number of scalars
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN) :: HSV      ! name of all scalar variables
 TYPE(SV_t), INTENT(INOUT) :: SV
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HCH_NAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HAER_NAMES     

 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HDSTNAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HSLTNAMES
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
!
IF (LHOOK) CALL DR_HOOK('INIT_CHEMICAL_n',0,ZHOOK_HANDLE)
!
IF (KSV /= 0) THEN
  !
  ALLOCATE(SV%CSV(KSV))
  CALL CH_INIT_NAMES(KLUOUT, HSV, SV, LVARSIGI, LVARSIGJ    )  

  IF (SV%NBEQ > 0 ) THEN
    ALLOCATE(HCH_NAMES(SV%NBEQ))
    HCH_NAMES(:) = SV%CSV(SV%NSV_CHSBEG:SV%NSV_CHSEND)
  ELSE
    ALLOCATE(HCH_NAMES(0))
  END IF

  IF (SV%NAEREQ > 0 ) THEN
    ALLOCATE(HAER_NAMES(SV%NAEREQ))
    HAER_NAMES(:) = SV%CSV(SV%NSV_AERBEG:SV%NSV_AEREND)
  ELSE
    ALLOCATE(HAER_NAMES(0))
  END IF
  !
  CALL DSLT_INIT_NAMES(         &
         KLUOUT,                &!I [idx] index of writing unit
         'DSTM',                &
         HSV,                   &!I [char] list of scalar variables
         JPMODE_DST,            &
         SV%NDSTEQ,                &!O [nbr] number of dust related tracers
         SV%NSV_DSTBEG,            &!O [idx] first dust related scalar variable
         SV%NSV_DSTEND,            &!O [idx] last dust related scalar variable
         LVARSIG_DST,           &!O type of standard deviation (fixed or variable)
         LRGFIX_DST             &!O type of mean radius (fixed or variable)        
         )  

  IF (PRESENT(HDSTNAMES)) THEN
    IF (SV%NDSTEQ >=1) THEN
      CALL DSLT_INIT_MODES(       &
            SV%NDSTEQ,               &!I [nbr] number of dust related variables in scalar list
            SV%NSV_DSTBEG,           &!I [idx] index of first dust related variable in scalar list
            SV%NSV_DSTEND,           &!I [idx] index of last dust related variable in scalar list
            LVARSIG_DST,          &!I type of standard deviation (fixed or variable)
            LRGFIX_DST,           &!O type of mean radius (fixed or variable)        
            NDST_MDEBEG,          &!O [idx] index of mass for first mode in scalar list
            NDSTMDE               &!O [nbr] number of modes to be transported
            )

      IF(.NOT. ASSOCIATED(HDSTNAMES)) ALLOCATE (HDSTNAMES(SV%NDSTEQ))
      HDSTNAMES(:) = SV%CSV(SV%NSV_DSTBEG:SV%NSV_DSTEND)
    ENDIF
  ENDIF


  CALL DSLT_INIT_NAMES(         &
          KLUOUT,               &!I [idx] index of writing unit
         'SLTM',                &          
          HSV,                  &!I [char] list of scalar variables
          JPMODE_SLT,           &          
          SV%NSLTEQ,               &!O [nbr] number of sea salt related tracers
          SV%NSV_SLTBEG,           &!O [idx] first sea salt related scalar variable
          SV%NSV_SLTEND,           &!O [idx] last sea salt related scalar variable
          LVARSIG_SLT,          &!O type of standard deviation (fixed or variable)
          LRGFIX_SLT            &!O type of mean radius (fixed or variable)        
          )  

  IF (PRESENT(HSLTNAMES)) THEN
    IF (SV%NSLTEQ >=1) THEN
      CALL DSLT_INIT_MODES(       &
            SV%NSLTEQ,               &!I [nbr] number of sea salt related variables in scalar list
            SV%NSV_SLTBEG,           &!I [idx] index of first sea salt related variable in scalar list
            SV%NSV_SLTEND,           &!I [idx] index of last sea salt related variable in scalar list
            LVARSIG_SLT,          &!I type of standard deviation (fixed or variable)
            LRGFIX_SLT,           &!O type of mean radius (fixed or variable)
            NSLT_MDEBEG,          &!O [idx] index of mass for first mode in scalar list
            NSLTMDE               &!O [nbr] number of modes to be transported
            )  
      IF(.NOT. ASSOCIATED(HSLTNAMES)) ALLOCATE (HSLTNAMES(SV%NSLTEQ))
      HSLTNAMES(:) = SV%CSV(SV%NSV_SLTBEG:SV%NSV_SLTEND)
    ENDIF
  END IF

ELSE
  ALLOCATE(SV%CSV     (0))
  IF (PRESENT(HDSTNAMES)) ALLOCATE(HDSTNAMES(0))
  IF (PRESENT(HSLTNAMES)) ALLOCATE(HSLTNAMES(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_CHEMICAL_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_CHEMICAL_n
