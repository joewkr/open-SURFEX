!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE MOD1D_n (DGO, O, OR, PLAT, S, &
                        HPROGRAM, PTIME, PEMIS, PDIR_ALB, PSCA_ALB, &
                        PLW, PSCA_SW, PDIR_SW, PSFTH, PSFTQ, PSFU, PSFV, PRAIN )           
!     #######################################################################
!
!!****  *MOD1D_n*  
!!
!!    PURPOSE
!!    -------
!     Source that exchanges Fluxes and SST between SURFEX (coupling_seaflux) 
!     and the oceanic 1D model in TKE eqations (mixtl_n) 
!     
!!**  METHOD
!!    ------
!     Change turbulent fluxes in solar, non solar, and fresh water fluxes 
!     with the oceanic convention for orientation of fluxes
!     The stress of wind is exchange between SURFEX and TKE model
!
!!    EXTERNAL
!!    --------
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
!!     C. Lebeaupin  *Météo-France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     02/2008
!!      Modified     07/2012, P. Le Moigne : CMO1D phasing
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS
USE MODD_OCEAN_CSTS
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_MIXTL_n
USE MODI_DIAG_INLINE_OCEAN_n
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DGO
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(OCEAN_REL_t), INTENT(INOUT) :: OR
REAL, DIMENSION(:), INTENT(IN) :: PLAT
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),    INTENT(IN)       :: HPROGRAM  ! program calling surf. schemes
REAL                ,INTENT(IN)       :: PTIME   ! current time since midnight in second
REAL, DIMENSION(:)  ,INTENT(IN)       :: PEMIS    ! emissivity
REAL, DIMENSION(:,:),INTENT(IN)       :: PDIR_ALB ! direct albedo
REAL, DIMENSION(:,:),INTENT(IN)       :: PSCA_ALB ! scattered albedo
REAL, DIMENSION(:)  ,INTENT(IN)       :: PLW     ! longwave radiation on horizontal surface (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)       :: PSCA_SW ! diffuse solar radiation on horizontal surface (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)       :: PDIR_SW ! direct solar radiation on horizontal surface (W/m2)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFTH   ! flux of heat (W/m2)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFTQ   ! flux of water vapor (kg/m2/s)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFU    ! zonal stress (Pa)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFV    ! meridian stress (Pa)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PRAIN   ! liquid precipitation (kg/s/m2)
!
!*      0.2    declarations of local variables
!
INTEGER :: JPT
INTEGER :: ITIME,NOCEAN_STEP
REAL, DIMENSION(SIZE(PSFTH)) :: ZFSOL,ZFNSOL !total solar and non-solar fluxes (W/m2)
REAL, DIMENSION(SIZE(PSFTH)) :: ZSFTEAU      !fresh water flux(kg/m2/s)
REAL, DIMENSION(SIZE(PSFTH)) :: ZLV          !latent heat
!
REAL, DIMENSION(SIZE(PSFTH)) :: ZLWU         !long waves upward fluxes (W/m2)
REAL, DIMENSION(SIZE(PDIR_ALB,1),SIZE(PDIR_ALB,2)) :: ZSWU
                                             !shortwave upward fluxes (W/m2)
!
REAL, DIMENSION(SIZE(PSFTH)) :: ZSEATEMP     !surface temperature (K)
!
LOGICAL         :: GCALLMIXT, GTIMEOK
INTEGER         :: ILUOUT              ! output listing logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MOD1D_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
ITIME=INT(PTIME)
NOCEAN_STEP=INT(O%XOCEAN_TSTEP)
!
GTIMEOK=(MOD(ITIME,NOCEAN_STEP)==0)
GCALLMIXT=((MOD(ITIME,NOCEAN_STEP)==0).AND.(O%NOCTCOUNT>0))
!
!Call 1D model if ptime proportional to the oceanic model time step
!
IF (GCALLMIXT) THEN
!
!       1.     Initializations
!________________________________________________________________________
!Computation of solar, non solar and fresh water fluxes
  DO JPT=1,SIZE(PSFTH)
 !SW Flux up
    ZSWU(JPT,:)= PDIR_SW(JPT,:) * PDIR_ALB(JPT,:) + PSCA_SW(JPT,:)*PSCA_ALB(JPT,:)
 !Net solar flux  
    ZFSOL(JPT)=(SUM(PDIR_SW(JPT,:))+SUM(PSCA_SW(JPT,:))-SUM(ZSWU(JPT,:)))/(XRHOSW*XCPSW)
 !Calcul flux LW UP
    ZLWU(JPT)= PEMIS(JPT)*XSTEFAN*S%XSST(JPT)**4 + (1-PEMIS(JPT))*PLW(JPT)
   
    IF (S%XSST(JPT)<=(XTT-2)) THEN
      ZFNSOL(JPT)=(PLW(JPT)-ZLWU(JPT)-PSFTH(JPT)-(XLSTT*PSFTQ(JPT)))/(XRHOSW*XCPSW)
      ZSFTEAU(JPT)=PSFTQ(JPT)/XRHOSWREF
    ELSE
      ZLV(JPT)=XLVTT+(XCPV-XCL)*(S%XSST(JPT)-XTT)
      ZFNSOL(JPT)=(PLW(JPT)-ZLWU(JPT)-PSFTH(JPT)-(ZLV(JPT)*PSFTQ(JPT)))/(XRHOSW*XCPSW)
      ZSFTEAU(JPT)=(PSFTQ(JPT)-PRAIN(JPT))/XRHOSWREF
    ENDIF  
  ENDDO
!__________________________________________________________________________
!
!        2. Call oceanic TKE model
!           ----------------------
!
  IF (OR%LFLUX_NULL) THEN
     WRITE(ILUOUT,*) 'Caution : SURFACE FLUX ARE SET TO 0 '
     ZFSOL(:)   = 0.
     ZFNSOL(:)  = 0.
     ZSFTEAU(:) = 0.
  END IF

  CALL MIXTL_n(O, OR, PLAT, ZFSOL,ZFNSOL,ZSFTEAU,PSFU,PSFV,ZSEATEMP)
!
!---------------------------------------------------------------------------
!        3. Coupling with SURFEX by SST (and relative wind) evolution
!
  IF (O%LPROGSST) THEN 
    S%XSST(:)=ZSEATEMP(:)
    !WRITE(ILUOUT,*) '**SST CHANGED FOR THE ',NOCTCOUNT,'TIME BY FIRST LEVEL OCEANIC MODEL TEMPERATURE AT ', ITIME,' s **'
  ENDIF
  !
ENDIF
!
IF (GTIMEOK) THEN
  CALL DIAG_INLINE_OCEAN_n(DGO, O, S%XSEABATHY)
  O%NOCTCOUNT = O%NOCTCOUNT+1
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MOD1D_N',1,ZHOOK_HANDLE)
!!-------------------------------------------------------------------------------
!!-----------------------------------------------------------------------------
END SUBROUTINE MOD1D_n
