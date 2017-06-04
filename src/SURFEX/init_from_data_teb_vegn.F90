!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_FROM_DATA_TEB_VEG_n (DTV, K, P, PEK, KDECADE, OUPDATE, OFIX, OTIME, OALB)    
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary 
!!                     physiographic variables for ISBA
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    Original   01/2004
!     
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODI_SOIL_ALBEDO
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
!
INTEGER,                INTENT(IN)    :: KDECADE
!
LOGICAL, INTENT(IN) :: OUPDATE
LOGICAL, INTENT(IN) :: OFIX
LOGICAL, INTENT(IN) :: OTIME
LOGICAL, INTENT(IN) :: OALB
!
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWG1
REAL, DIMENSION(:), ALLOCATABLE   :: ZWGSAT
!
INTEGER :: ITIME
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      TIME INITIALIZATION
!             -------------------
!
! data every month
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_TEB_VEG_N',0,ZHOOK_HANDLE)
IF (DTV%NTIME==12) THEN
  ITIME = (KDECADE+2)/3    
ELSEIF (DTV%NTIME==1) THEN
  ITIME = 1
ENDIF
!
!
IF (OFIX) THEN
!

  IF (SIZE(P%XH_TREE)>0) P%XH_TREE(:) = DTV%XPAR_H_TREE(:,1)
!
  P%XZ0_O_Z0H(:) = DTV%XPAR_Z0_O_Z0H(:,1)
!
!---------------------------------------------------------------------------------
!
!* soil layers
!  -----------
!
  P%XDG(:,:) = DTV%XPAR_DG(:,:,1)
!
!* cumulative root fraction
!
  IF (SIZE(P%XROOTFRAC)>0) P%XROOTFRAC(:,:) = DTV%XPAR_ROOTFRAC(:,:,1)
!
!* soil ice for runoff
!
 P%XD_ICE(:) = DTV%XPAR_DICE(:,1)
!
  IF (SIZE(P%XDMAX)>0) P%XDMAX(:) = DTV%XPAR_DMAX(:,1)

  IF (SIZE(P%XRE25)>0) P%XRE25(:) = DTV%XPAR_RE25(:,1)


ENDIF
!
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
!*    2.1     fields on natural surfaces only, taking into account patches/ 
!             -------------------------------
!
IF (OTIME) THEN
!
! vegetation fraction
! -------------------
!
 PEK%XVEG(:) =  DTV%XPAR_VEG (:,ITIME,1)
!
! Leaf Aera Index
! ---------------
!
 PEK%XLAI(:) = DTV%XPAR_LAI (:,ITIME,1)
!
! roughness length
! ----------------
!
  PEK%XZ0(:) =  DTV%XPAR_Z0 (:,ITIME,1)
!
!emis-eco
!--------
!
  PEK%XEMIS(:) =  DTV%XPAR_EMIS (:,ITIME,1)
!
 IF (.NOT.OUPDATE) THEN
!---------------------------------------------------------------------------------
! 
!* 1/Rsmin
!
  PEK%XRSMIN(:) = DTV%XPAR_RSMIN(:,1)
!
!* other vegetation parameters
!
  PEK%XGAMMA(:) = DTV%XPAR_GAMMA(:,1)
  PEK%XWRMAX_CF(:) = DTV%XPAR_WRMAX_CF(:,1)
!
!
  PEK%XRGL(:) = DTV%XPAR_RGL(:,1)
  PEK%XCV(:) = DTV%XPAR_CV(:,1)
!
!---------------------------------------------------------------------------------
  PEK%XALBNIR_VEG(:) = DTV%XPAR_ALBNIR_VEG(:,1,1)
  PEK%XALBVIS_VEG(:) = DTV%XPAR_ALBVIS_VEG(:,1,1)
  PEK%XALBUV_VEG(:)  = DTV%XPAR_ALBUV_VEG(:,1,1)
!
  IF (SIZE(PEK%XGMES)>0) PEK%XGMES(:) = DTV%XPAR_GMES(:,1)

  IF (SIZE(PEK%XBSLAI)>0) PEK%XBSLAI(:) = DTV%XPAR_BSLAI(:,1)

  IF (SIZE(PEK%XSEFOLD)>0) PEK%XSEFOLD(:) = DTV%XPAR_SEFOLD(:,1)

  IF (SIZE(PEK%XGC)>0) PEK%XGC(:) = DTV%XPAR_GC(:,1)
!
  IF (SIZE(PEK%XLAIMIN)>0) PEK%XLAIMIN(:) = DTV%XPAR_LAIMIN(:,1)
!
  IF (SIZE(PEK%XCE_NITRO)>0) PEK%XCE_NITRO(:) = DTV%XPAR_CE_NITRO(:,1)
!
  IF (SIZE(PEK%XCF_NITRO)>0) PEK%XCF_NITRO(:) = DTV%XPAR_CF_NITRO(:,1)
!
  IF (SIZE(PEK%XCNA_NITRO)>0) PEK%XCNA_NITRO(:) = DTV%XPAR_CNA_NITRO(:,1)
!
  IF (SIZE(PEK%XF2I)>0) PEK%XF2I(:) = DTV%XPAR_F2I(:,1)
!
  IF (SIZE(PEK%LSTRESS)>0) PEK%LSTRESS(:) = DTV%LPAR_STRESS(:,1)
!
 ENDIF
ENDIF
!
IF (OALB) THEN
  !
  ALLOCATE(ZWGSAT(SIZE(K%XALBVIS_DRY)))
  ALLOCATE(ZWG1(SIZE(K%XALBVIS_DRY)))
  ZWGSAT(:) = 0.
  ZWG1  (:) = 0.
  CALL SOIL_ALBEDO('DRY',ZWGSAT, ZWG1, K, PEK, "ALL" ) 
  DEALLOCATE(ZWGSAT,ZWG1)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_TEB_VEG_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_FROM_DATA_TEB_VEG_n
