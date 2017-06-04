!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SGH(IO, KK, PK, PEK, DEK, DMK, PTSTEP, PPG, PPG_MELT, PDUNNE )  
!
!     #####################################################################
!
!!****  *HYDRO_SGH*  
!!
!!    PURPOSE
!!    =======
!
!     1. Determine the Horton runoff that take account of a spatial subgri
!        exponential distribution of the precipitation and of the surface ksat.
!     1. Determine the surface saturated fraction (dt92 or Topmodel).
!     3. Determine the Dunne runoff (dt92 or Topmodel).
!     4. Determine the infiltration rate.
!     5. Determine the flooplains interception and infiltration rate.
!
!!    MODIFICATIONS
!!    -------------
!!
!!         03/16    (B. Decharme) Limit flood infiltration
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!        ===================
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,      ONLY : XRHOLW, XDAY, XCL, XCI, XRHOLI
USE MODD_ISBA_PAR,  ONLY : XWGMIN, XSPHSOIL, XDRYWGHT
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SGH_PAR,   ONLY : XHORT_DEPTH
!
#ifdef TOPD
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, XAS_NATURE, XATOP, NMASKT_PATCH
#endif
!
USE MODI_HYDRO_DT92
!
USE MODE_HYDRO_DIF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
REAL, INTENT(IN)                 :: PTSTEP
!                                   timestep of the integration
!
REAL, DIMENSION(:), INTENT(INOUT):: PPG
REAL, DIMENSION(:), INTENT(IN)   :: PPG_MELT
!                                   PPG      = water reaching the ground
!                                   PPG_MELT = snowmelt reaching the ground
!
REAL, DIMENSION(:), INTENT(OUT)  :: PDUNNE
!                                   PDUNNE  = Dunne runoff
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                            :: ZEICE = 6.0  ! Ice vertical diffusion impedence factor 
!
REAL, DIMENSION(SIZE(PPG))                 :: ZPG_INI, ZFROZEN, ZIMAX_ICE, ZIMAX, &
                                              ZHORT_R, ZHORT_M, ZSOILMAX, ZIF_MAX,&
                                              ZPIFLDMAX
!                                             ZFROZEN  = frozen soil fraction for runoff
!                                             ZIMAX_ICE    = maximum infiltration rate for frozen soil
!                                             ZIMAX     = maximum infiltration rate for unfrozen soil
!                                             ZPIFLDMAX    = maximum floodplains infiltration during 1 day (kg/m2/s)
REAL, DIMENSION(SIZE(PPG))                 :: ZWG2_AVG, ZWGI2_AVG, ZWSAT_AVG, ZWWILT_AVG
!                                             Average water and ice content
!                                             values over the soil depth D2 (for calculating surface runoff)
!
REAL, DIMENSION(SIZE(PK%XDG,1),SIZE(PK%XDG,2)) :: ZWSAT, ZWFC, ZFRZ
!
REAL, DIMENSION(SIZE(PPG))                 :: ZPG_WORK, ZRUISDT, ZNL_HORT, ZDEPTH
!
REAL, DIMENSION(SIZE(PPG))                 :: ZRUNOFF_TOPD
!
REAL                                       :: ZEFFICE, ZLOG10, ZLOG, ZS, ZD_H
!
REAL                                       :: ZTDIURN, ZSOILHEATCAP
!                                             ZTDIURN      = thermal penetration depth for restore (m)
!                                             ZSOILHEATCAP = Total soil volumetric heat capacity [J/(m3 K)]
!
INTEGER                                    :: INJ, INL, JJ, JL, IDEPTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!allocate
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SGH',0,ZHOOK_HANDLE)
!
!initialize
!
ZFROZEN  (:)  = 0.0
ZIMAX_ICE(:)  = 0.0
ZIMAX    (:)  = 0.0
!
ZWSAT  (:,:)  = 0.0
ZWFC   (:,:)  = 0.0
!
ZLOG10 = LOG(10.0)
!
!IO%CRUNOFF = DT92 ZFSAT calculation
ZWG2_AVG(:)   = 0.0
ZWGI2_AVG(:)  = 0.0
ZWSAT_AVG(:)  = 0.0
ZWWILT_AVG(:) = 0.0
!
!IO%CHORT=SGH
ZHORT_R(:) = 0.0
ZHORT_M(:) = 0.0
!
!DEK%XIFLOOD calculation
ZSOILMAX(:)  = 0.0
ZIF_MAX(:)   = 0.0
ZPIFLDMAX(:) = 0.0
!
!IO%CRUNOFF = DT92 DUNNE calculation
ZPG_WORK(:)   = 0.0
ZRUISDT(:)    = 0.0
!
!IO%CRUNOFF = TOPD DUNNE calculation
ZRUNOFF_TOPD(:) = 0.0
!
!to limit numerical artifacts
ZPG_INI(:) = PPG(:) + PPG_MELT(:)
!
!
INJ=SIZE(PK%XDG,1)
INL=MAXVAL(PK%NWG_LAYER)
!
!-------------------------------------------------------------------------------
!
!*           1. Surface saturated fraction
!            -----------------------------
!
IF( IO%CRUNOFF=='DT92' .OR. IO%CRUNOFF == 'TOPD' )THEN
!
! Calculate the layer average water content for the sub-grid
! surface runoff computation: use PK%XRUNOFFD(:,1) as the depth over which
! runoff is calculated.
!
! First, determine a weight for each layer's contribution
! to thickness averaged water content and soil properties for runoff.
!
  IF (IO%CISBA == 'DIF') THEN
!
! Vertically averaged soil properties and moisture for surface runoff computation:
!
    DO JL=1,IO%NLAYER_DUN
      DO JJ=1,INJ
        IDEPTH=PK%NWG_LAYER(JJ)
        IF(JL<=IDEPTH)THEN
          ZWG2_AVG  (JJ) = ZWG2_AVG  (JJ) + PK%XSOILWGHT(JJ,JL)*PEK%XWG (JJ,JL) /MAX(1.E-6,PK%XRUNOFFD(JJ))
          ZWGI2_AVG (JJ) = ZWGI2_AVG (JJ) + PK%XSOILWGHT(JJ,JL)*PEK%XWGI(JJ,JL) /MAX(1.E-6,PK%XRUNOFFD(JJ))
          ZWSAT_AVG (JJ) = ZWSAT_AVG (JJ) + PK%XSOILWGHT(JJ,JL)*KK%XWSAT (JJ,JL)/MAX(1.E-6,PK%XRUNOFFD(JJ))
          ZWWILT_AVG(JJ) = ZWWILT_AVG(JJ) + PK%XSOILWGHT(JJ,JL)*KK%XWWILT(JJ,JL)/MAX(1.E-6,PK%XRUNOFFD(JJ))
        ENDIF
      ENDDO
    ENDDO
!
  ELSE
!           
    ZWG2_AVG(:)   = PEK%XWG (:,2)
    ZWGI2_AVG(:)  = PEK%XWGI(:,2)
    ZWSAT_AVG(:)  = KK%XWSAT (:,1)
    ZWWILT_AVG(:) = KK%XWWILT(:,1)
!      
  ENDIF
!
  IF(IO%CHORT=='SGH')THEN
    !runoff over frozen soil explicitly calculated
    ZWGI2_AVG(:)=0.0
  ENDIF
!
   DO JJ=1,INJ
     ZS = MIN(1.0,(ZWG2_AVG(JJ)+ZWGI2_AVG(JJ)-ZWWILT_AVG(JJ))/(ZWSAT_AVG(JJ)-ZWWILT_AVG(JJ)))
     KK%XFSAT(JJ) = 1.0-(1.0-MAX(0.0,ZS))**(KK%XRUNOFFB(JJ)/(KK%XRUNOFFB(JJ)+1.))
   ENDDO        
!
ENDIF
!
!*           2. Horton runoff
!            ----------------
!
IF(IO%CHORT=='SGH'.OR.IO%LFLOOD)THEN  
!
  IF(IO%CISBA == 'DIF')THEN
!
!   no subgrid frozen soil fraction of the grid cells
    ZFROZEN(:) = 0.0
!    
    DO JL=1,IO%NLAYER_HORT
      DO JJ=1,INJ   
!              
!       Modify soil porosity as ice assumed to become part
!       of solid soil matrix (with respect to liquid flow):                
        ZWSAT(JJ,JL) = MAX(XWGMIN, KK%XWSAT(JJ,JL)-PEK%XWGI(JJ,JL))
        ZWFC (JJ,JL) = KK%XWFC(JJ,JL)*ZWSAT(JJ,JL)/KK%XWSAT(JJ,JL) 
!        
!       Impedance Factor from (Johnsson and Lundin 1991).
        ZFRZ(JJ,JL) = EXP(ZLOG10*(-ZEICE*(PEK%XWGI(JJ,JL)/(PEK%XWGI(JJ,JL)+PEK%XWG(JJ,JL)))))
!
      ENDDO
    ENDDO    
!
!   Calculate infiltration MAX using green-ampt approximation (derived form)
    ZIMAX(:) = INFMAX_FUNC(PEK%XWG, ZWSAT, ZFRZ, PK%XCONDSAT, KK%XMPOTSAT, KK%XBCOEF, &
                           PK%XDZG, PK%XDG, IO%NLAYER_HORT)
!  
  ELSE
!
    DO JJ=1,INJ
!
!    Total soil volumetric heat capacity [J/(m3 K)]:
!
      ZSOILHEATCAP = XCL*XRHOLW*PEK%XWG (JJ,2) +                           &
                     XCI*XRHOLI*PEK%XWGI(JJ,2) +                           &
                     XSPHSOIL*XDRYWGHT*(1.0-KK%XWSAT(JJ,1))
!                     
!     Soil thickness which corresponds to the diurnal surface temperature
!     wave penetration depth as T2 is the average temperature for this layer:
!
      ZTDIURN   = MIN(PK%XDG(JJ,2), 4./(ZSOILHEATCAP*DMK%XCG(JJ)))
!    
!     Effective frozen depth penetration 
!
      ZEFFICE = PK%XDG(JJ,2)*PEK%XWGI(JJ,2)/(PEK%XWGI(JJ,2)+PEK%XWG(JJ,2))
!
!     Modify soil porosity as ice assumed to become part
!     of solid soil matrix (with respect to liquid flow):
!
      ZWSAT(JJ,1) = MAX(XWGMIN, KK%XWSAT(JJ,1)-PEK%XWGI(JJ,2)) 
!
!     calculate the subgrid frozen soil fraction of the grid cells
!
      ZFROZEN (JJ) = MIN(1.,ZEFFICE/MAX(PK%XD_ICE(JJ),ZTDIURN))
!
!     Impedance Factor from (Johnsson and Lundin 1991).
!
      ZFRZ(JJ,1) = EXP(ZLOG10*(-ZEICE*MIN(1.,ZEFFICE/ZTDIURN)))
!
!     Calculate infiltration MAX on frozen soil as Johnsson and Lundin (1991).
!     The max infiltration is equal ,1to the unsaturated conductivity function at a
!     water content c,1orresponding to the total porosity less the ice-filled volume.
!
      ZS =MIN(1.,ZWSAT(JJ,1)/KK%XWSAT(JJ,1))
      ZIMAX_ICE(JJ)=ZFRZ(JJ,1)*PK%XKSAT_ICE(JJ)*(ZS**(2*KK%XBCOEF(JJ,1)+3.))
!
!     Calculate infiltration MAX on unfrozen soil using green-ampt approximation
!    
      ZS   =MIN(1.,PEK%XWG(JJ,2)/ZWSAT(JJ,1))
      ZD_H =MIN(0.10,PK%XDG(JJ,2))
      ZIMAX(JJ)=PK%XCONDSAT(JJ,1)*(KK%XBCOEF(JJ,1)*KK%XMPOTSAT(JJ,1)*(ZS-1.0)/ZD_H+1.0)
!
    ENDDO
!
  ENDIF
!
ENDIF
!
IF(IO%CHORT=='SGH')THEN
!
! calculate the Horton runoff generated by the rainfall rate
!
  IF(IO%CRAIN=='SGH')THEN
!
    WHERE(PPG(:)>0.)
       ZHORT_R(:) = (1.- ZFROZEN(:))* PPG(:)/((ZIMAX    (:)*XRHOLW*KK%XMUF(:)/PPG(:)) + 1.) & !unfrozen soil
                  +      ZFROZEN(:) * PPG(:)/((ZIMAX_ICE(:)*XRHOLW*KK%XMUF(:)/PPG(:)) + 1.)   !frozen soil
    END WHERE        
!
  ELSE
!
    ZHORT_R(:) = (1.- ZFROZEN(:))* MAX(0.,PPG(:)-ZIMAX    (:)*XRHOLW)          & !unfrozen soil
               +      ZFROZEN(:) * MAX(0.,PPG(:)-ZIMAX_ICE(:)*XRHOLW)            !frozen soil
!
  ENDIF
!
! calculate the Horton runoff generated by the snow melt
!        
  ZHORT_M(:) = (1.- ZFROZEN(:))* MAX(0.,PPG_MELT(:)-ZIMAX    (:)*XRHOLW)          & !unfrozen soil
             +      ZFROZEN(:) * MAX(0.,PPG_MELT(:)-ZIMAX_ICE(:)*XRHOLW)            !frozen soil
!
! calculate the  total Horton runoff 
!
  WHERE(KK%XFFLOOD(:)<=KK%XFSAT(:))
        DEK%XHORT(:) = (1. - KK%XFSAT(:))   * (ZHORT_R(:) + ZHORT_M(:))
  ELSEWHERE
        DEK%XHORT(:) = (1. - KK%XFFLOOD(:)) * (ZHORT_R(:) + ZHORT_M(:))
  ENDWHERE
!
ELSE
!
  DEK%XHORT(:) = 0.0
!
ENDIF
!
! calculate all water reaching the ground
!
PPG  (:) = PPG(:) + PPG_MELT(:)        
!
!
!*           3. Dunne runoff and flood interception
!            --------------------------------------
!
! Interception by the flooplains
!
IF(IO%LFLOOD)THEN
  DEK%XPFLOOD(:)=KK%XFFLOOD(:)*MAX(0.0,PPG(:))
ELSE
  DEK%XPFLOOD(:)=0.0
ENDIF
!
IF(IO%CRUNOFF=='SGH ')THEN
!        
! calculate the Dunne runoff with TOPMODEL
!
  PDUNNE(:) = MAX(PPG(:),0.0) * MAX(KK%XFSAT(:)-KK%XFFLOOD(:),0.0)
!
ELSEIF (IO%CRUNOFF=='DT92' .OR. IO%CRUNOFF=='TOPD')THEN
!
!*       Dumenil et Todini (1992)  RUNOFF SCHEME
!        ---------------------------------------         
!
! surface runoff done only on the Fsat-Fflood fraction
!
  ZPG_WORK(:) = PPG(:) - DEK%XHORT(:) - DEK%XPFLOOD(:)
!
#ifdef TOPD
  IF ( LCOUPL_TOPD.AND.IO%CRUNOFF == 'TOPD' )THEN
    !
    DO JJ=1,SIZE(NMASKT_PATCH)
      IF (NMASKT_PATCH(JJ)/=0) THEN
        IF ( XATOP(NMASKT_PATCH(JJ))/=0. .AND. XAS_NATURE(NMASKT_PATCH(JJ))/=XUNDEF ) THEN
          ZRUNOFF_TOPD(JJ) = MAX(PPG(JJ),0.0) * MAX(XAS_NATURE(NMASKT_PATCH(JJ)),0.0)
        ENDIF
      ENDIF 
    ENDDO
    !
  ENDIF
#endif
  !
  CALL HYDRO_DT92(PTSTEP, KK%XRUNOFFB, ZWWILT_AVG,  PK%XRUNOFFD, ZWSAT_AVG, &
                  ZWG2_AVG, ZWGI2_AVG, ZPG_WORK, ZRUISDT           )
!
  PDUNNE(:) = ZRUISDT(:)*PK%XRUNOFFD(:)*XRHOLW/PTSTEP
  !
#ifdef TOPD
  IF (LCOUPL_TOPD.AND.IO%CRUNOFF == 'TOPD') THEN
    PDUNNE(:) = ZRUNOFF_TOPD(:) +  PDUNNE(:)*(1-XATOP(NMASKT_PATCH(:)))
  ENDIF
#endif
  !
  IF(IO%LFLOOD)THEN
    WHERE(KK%XFFLOOD(:)>=KK%XFSAT(:).AND.KK%XFFLOOD(:)>0.0)PDUNNE(:) = 0.0
  ENDIF   
  !
ELSE
! 
! Default case (no subgrid runoff)
!
  KK%XFSAT (:) = 0.0
  PDUNNE(:) = 0.0
!
ENDIF
!
! calculate the infiltration rate after runoff
!
PPG  (:) = PPG(:) - PDUNNE(:) - DEK%XHORT(:) - DEK%XPFLOOD(:)
!
! Supress numerical artifacts:
!
WHERE (ZPG_INI(:)<0.0)
  PPG(:)     = ZPG_INI(:)
  DEK%XHORT(:) = 0.0
  PDUNNE   (:) = 0.0
  DEK%XPFLOOD(:) = 0.0
ENDWHERE
!
!*           4. infiltration rate from floodplains (à revoir pour DF !!!)
!            -------------------------------------
!
IF(IO%LFLOOD)THEN
!
! calculate the maximum flood infiltration
!
  ZPIFLDMAX(:) = MIN(KK%XPIFLOOD(:),XRHOLW/XDAY) ! no more than 1 meter of water per days
!
  ZIF_MAX(:) = MAX(0.,(1.- ZFROZEN(:))) * ZIMAX    (:)*XRHOLW &   !unfrozen soil
             +             ZFROZEN(:)   * ZIMAX_ICE(:)*XRHOLW     !frozen soil
!
  IF(IO%CISBA == 'DIF')THEN
    ZDEPTH(:)=0.0
    DO JL=1,IO%NLAYER_HORT
      DO JJ=1,INJ
        IF(ZDEPTH(JJ)<XHORT_DEPTH)THEN
          ZSOILMAX(JJ) = ZSOILMAX(JJ)+MAX(0.0,ZWFC(JJ,JL)-PEK%XWG(JJ,JL))*PK%XDZG(JJ,JL)*XRHOLW/PTSTEP
          ZDEPTH  (JJ) = PK%XDG(JJ,JL)
        ENDIF
      ENDDO
    ENDDO
  ELSE
    DO JJ=1,INJ
      ZWSAT(JJ,1)  = MAX(XWGMIN, KK%XWSAT(JJ,1)-PEK%XWGI(JJ,2)) 
      ZSOILMAX(JJ) = MAX(0.0,ZWSAT(JJ,1)-PEK%XWG(JJ,2))*PK%XDG(JJ,2)*XRHOLW/PTSTEP
    ENDDO
  ENDIF
!
  ZSOILMAX(:) = MIN(ZSOILMAX(:),ZIF_MAX(:))
!
  DEK%XIFLOOD(:) = MAX(0.0,(KK%XFFLOOD(:)-KK%XFSAT(:))) * MIN(ZPIFLDMAX(:),ZSOILMAX(:))
!
ELSE
!
  DEK%XIFLOOD(:)=0.0
!
ENDIF
!
!calculate the infiltration rate
!
PPG  (:) = PPG(:) + DEK%XIFLOOD(:)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SGH',1,ZHOOK_HANDLE)
!
END SUBROUTINE HYDRO_SGH
