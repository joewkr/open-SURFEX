!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ICE_SOILDIF(KK, PK, PEK, PTSTEP, PKSFC_IVEG, PLEGI, PSOILHCAPZ, PWGI_EXCESS)  
!     ##########################################################################
!
!!****  *ICE_SOILDIF*  
!
!!    PURPOSE
!!    -------
!     This subroutine calculates soil water phase changes using the
!     available/excess energy approach. Soil temperature and volumetric
!     ice content are adjusted due to phase changes. See the references
!     Boone et al., 39, JAM, 2000 and Giard and Bazile, 128, MWR, 2000.
!     NOTE that more recently a modification was made: freeze/thaw follows
!     a relationship between liquid water and temperature derriving from
!     the Clausius Clapeyron Eq. This results in little to no freezing for
!     sufficiently dry but cold (below freezing) soils. Scatter about this
!     curve results due to 'phase change efficiencies' and the surface insolation
!     coefficient.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone (2000)
!!    Boone et al., (2000)
!!      
!!    AUTHOR
!!    ------
!!      A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/02/00   Boonei
!!      Modified    24/11/09   Boone
!!                             Limit energy available for phase change by
!                              local amount owing to diffusion. Has almost
!                              no impact except under rare circumstances
!                              (avoids rare but possible oscillatory behavior)
!                              Also, add minimum (numerical) melt/freeze efficieny to prevent
!                              prolonged periods of small ice amounts approaching zero.
!
!!      Modified    01/06/11   Boone
!                              Use apparent heat capacity linearization for freezing
!                              (when temperature depenence on ice change is direct)
!                              Do away with efficiency coefficients as they acted to provide
!                              numerical stability: not needed as apparent heat capacity increases
!                              the effective heat capacity thus increasing stability.
!                              NOTE: for now considers Brooks & Corey type water retention.
!
!      Modified    08/2011     Decharme
!                              Optimization
!      Modified    10/2013     Boone
!                              Slight edit to phase computation to improve enthalpy conservation
!                              
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_CSTS,     ONLY : XLMTT, XTT, XG, XCI, XRHOLI, XRHOLW
USE MODD_ISBA_PAR, ONLY : XWGMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, INTENT(IN)                   :: PTSTEP  ! Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PKSFC_IVEG, PLEGI
!                                      PKSFC_IVEG = effect of surface layer insolation on phase changes
!                                                    Giard and Bazile (2000): non-dimensional
!                                      PLEGI      = ice sublimation (m s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILHCAPZ
!                                      PSOILHCAPZ = soil heat capacity [J/(m3 K)]
!
REAL, DIMENSION(:), INTENT(OUT)     :: PWGI_EXCESS
!                                      PWGI_EXCESS = Soil ice excess water content
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JL   ! loop control
!
INTEGER                             :: INI    ! Number of point
INTEGER                             :: INL    ! Number of explicit soil layers
INTEGER                             :: IDEPTH ! Total moisture soil depth
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)) :: ZK, ZEXCESSFC
!
REAL, DIMENSION(SIZE(PEK%XTG,1))             :: ZEXCESS
!
REAL                                     :: ZWGMAX, ZPSIMAX, ZPSI, ZDELTAT,  &
                                            ZPHASE, ZTGM, ZWGM, ZWGIM, ZLOG, &
                                            ZEFFIC, ZPHASEM, ZPHASEF, ZWORK, &
                                            ZAPPHEATCAP
!                                            
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
! Initialization:
! ---------------
!
IF (LHOOK) CALL DR_HOOK('ICE_SOILDIF',0,ZHOOK_HANDLE)
!
INI = SIZE(PEK%XTG,1)
INL = MAXVAL(PK%NWG_LAYER)
!
ZEXCESSFC  (:,:)=0.0
ZEXCESS    (:  )=0.0
PWGI_EXCESS(:  )=0.0
!
!-------------------------------------------------------------------------------
!
! 1. Surface layer vegetation insulation coefficient (-)
!    ---------------------------------------------------
!
ZK(:,:) = 1.0
ZK(:,1) = PKSFC_IVEG(:)
!
! 2. Soil ice evolution computation:
!    -------------------------------
!
DO JL=1,INL
  DO JJ=1,INI                 
    IDEPTH=PK%NWG_LAYER(JJ)
    IF(JL<=IDEPTH)THEN
!
      ZWGIM = PEK%XWGI(JJ,JL)
      ZWGM  = PEK%XWG (JJ,JL)
      ZTGM  = PEK%XTG (JJ,JL)

!     The maximum liquid water content as
!     as function of temperature (sub-freezing)
!     based on Gibbs free energy (Fuchs et al., 1978):
!
      ZPSIMAX = MIN(KK%XMPOTSAT(JJ,JL),XLMTT*(PEK%XTG(JJ,JL)-XTT)/(XG*PEK%XTG(JJ,JL)))
!        
      ZWORK  = ZPSIMAX/KK%XMPOTSAT(JJ,JL)
      ZLOG   = LOG(ZWORK)/KK%XBCOEF(JJ,JL)
      ZWGMAX = KK%XWSAT(JJ,JL)*EXP(-ZLOG)
!
!     Calculate maximum temperature for ice based on Gibbs free energy: first
!     compute soil water potential using Brook and Corey (1966) model:
!     psi=mpotsat*(w/wsat)**(-bcoef)
!
      ZWORK = PEK%XWG(JJ,JL)/KK%XWSAT(JJ,JL)
      ZLOG  = KK%XBCOEF(JJ,JL)*LOG(ZWORK)
      ZPSI  = KK%XMPOTSAT(JJ,JL)*EXP(-ZLOG)
!
      ZDELTAT = PEK%XTG(JJ,JL) - XLMTT*XTT/(XLMTT-XG*ZPSI)
!
!     Compute apparent heat capacity. This is considered
!     only when there is available energy (cold) and liquid water
!     available...freezing front.
!     This also has the secondary effect of increasing numerical stability
!     during freezing (as there is a strong temperature dependence) by
!     i) potentially significantly increasing the "apparent" heat capacity and
!     ii) this part is also treated implicitly herein.
!
      ZWORK = (XCI*XRHOLI/(XLMTT*XRHOLW))*ZK(JJ,JL)*MAX(0.0,-ZDELTAT)
!        
      ZAPPHEATCAP=0.0
      IF(ZDELTAT<0.0.AND.ZWGM>=ZWGMAX.AND.ZWORK>=MAX(0.0,ZWGM-ZWGMAX))THEN
        ZAPPHEATCAP = -(XTT*XRHOLW*XLMTT*XLMTT/XG)*ZWGMAX/(ZPSIMAX*KK%XBCOEF(JJ,JL)*ZTGM*ZTGM)
      ENDIF
!
!     *Melt* ice if energy and ice available:
      ZPHASEM  = (PTSTEP/PK%XTAUICE(JJ))*MIN(ZK(JJ,JL)*XCI*XRHOLI*MAX(0.0,ZDELTAT),ZWGIM*XLMTT*XRHOLW)
!
!     *Freeze* liquid water if energy and water available:
      ZPHASEF  = (PTSTEP/PK%XTAUICE(JJ))*MIN(ZK(JJ,JL)*XCI*XRHOLI*MAX(0.0,-ZDELTAT),MAX(0.0,ZWGM-ZWGMAX)*XLMTT*XRHOLW)
!
!     Update heat content if melting or freezing
      PEK%XTG(JJ,JL) = ZTGM + (ZPHASEF - ZPHASEM)/(PSOILHCAPZ(JJ,JL)+ZAPPHEATCAP)
!
!     Get estimate of actual total phase change (J/m3) for equivalent soil water changes:
      ZPHASE = (PSOILHCAPZ(JJ,JL)+ZAPPHEATCAP)*(PEK%XTG(JJ,JL)-ZTGM)
!
!     Adjust ice and liquid water conents (m3/m3) accordingly :
      PEK%XWGI(JJ,JL) = ZWGIM + ZPHASE/(XLMTT*XRHOLW)     
      PEK%XWG(JJ,JL) = ZWGM  - ZPHASE/(XLMTT*XRHOLW) 
!
    ENDIF
  ENDDO
ENDDO
!
! 3. Adjust surface soil ice content for sublimation
!    -----------------------------------------------
!
PEK%XWGI(:,1) = PEK%XWGI(:,1) - PLEGI(:)*PTSTEP/PK%XDZG(:,1)
!
! The remaining code in this block are merely constraints to ensure a highly
! accurate water budget: most of the time this code will not have any
! effect on the soil water profile.
! If sublimation causes all of the remaining ice to be extracted, remove
! some of the liquid (a correction): NOTE that latent heating already accounted
! for in sublimation term, so no need to alter soil temperature.
!
ZEXCESS(:)  = MAX(0.0,  - PEK%XWGI(:,1))
PEK%XWG(:,1)   = PEK%XWG  (:,1) - ZEXCESS(:)
PEK%XWGI(:,1)  = PEK%XWGI (:,1) + ZEXCESS(:)
ZEXCESSFC(:,1) = ZEXCESSFC(:,1) - ZEXCESS(:)
!
! 4. Prevent some possible problems
!    ------------------------------
!
! If sublimation is negative (condensation), make sure ice does not
! exceed maximum possible. If it does, then put excess ice into layer below:
! This correction should rarely if ever cause any ice accumulation in the
! sub-surface layer: this is especially true of deeper layers but it is
! accounted for none-the-less.
!
DO JL=1,INL
  DO JJ=1,INI
    IDEPTH=PK%NWG_LAYER(JJ)
    IF(JL<=IDEPTH)THEN
      ZEXCESS(JJ)       = MAX(0.0, PEK%XWGI(JJ,JL) - (KK%XWSAT(JJ,JL)-XWGMIN) )
      PEK%XWGI(JJ,JL)   = PEK%XWGI(JJ,JL)  - ZEXCESS(JJ)
      ZEXCESSFC(JJ,JL)  = ZEXCESSFC(JJ,JL) + ZEXCESS(JJ)
      IF(JL<IDEPTH)THEN
        PEK%XWGI(JJ,JL+1)  = PEK%XWGI(JJ,JL+1)  + ZEXCESS(JJ)*(PK%XDZG(JJ,JL)/PK%XDZG(JJ,JL+1))
        ZEXCESSFC(JJ,JL+1) = ZEXCESSFC(JJ,JL+1) - ZEXCESS(JJ)*(PK%XDZG(JJ,JL)/PK%XDZG(JJ,JL+1))
      ELSE
        PWGI_EXCESS(JJ)    = ZEXCESS(JJ)*PK%XDZG(JJ,IDEPTH)*XRHOLW/PTSTEP
      ENDIF
    ENDIF
  ENDDO
ENDDO
!   
! Prevent keeping track of very small numbers for ice content: (melt it)
! and conserve energy:
!
DO JL=1,INL
  DO JJ=1,INI 
    IDEPTH=PK%NWG_LAYER(JJ)  
    IF(JL<=IDEPTH.AND.PEK%XWGI(JJ,JL)>0.0.AND.PEK%XWGI(JJ,JL)<1.0E-6)THEN
      PEK%XWG   (JJ,JL)  = PEK%XWG(JJ,JL) + PEK%XWGI(JJ,JL)
      ZEXCESSFC(JJ,JL) = ZEXCESSFC(JJ,JL) + PEK%XWGI(JJ,JL)
      PEK%XWGI(JJ,JL) = 0.0
    ENDIF
    PEK%XTG(JJ,JL) = PEK%XTG(JJ,JL) - ZEXCESSFC(JJ,JL)*XLMTT*XRHOLW/PSOILHCAPZ(JJ,JL)           
  ENDDO
ENDDO
!
!
IF (LHOOK) CALL DR_HOOK('ICE_SOILDIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ICE_SOILDIF
