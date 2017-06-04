!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
      SUBROUTINE ICE_SOILFR(IO, KK, PK, PEK, DMK, PTSTEP, PKSFC_IVEG, PDWGI1, PDWGI2 )   
!!     ##########################################################################
!
!!****  *ICE_SOILFR*  
!!
!!    PURPOSE
!!    -------
!
!     In ISBA-FR: calculates the 
!     1.) evolution of the surface and deep-soil temperature(s)
!         (i.e., Ts and T2 if Force-Restore, TN if DIFfusion) due to soil water 
!         phase changes
!     
!!**  METHOD
!!    ------
!
!     - latent heating from soil ice phase changes
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      14/03/95 
!!      (A.Boone)     08/11/00 soil ice phase changes herein
!!      (A.Boone)     06/05/02 Updates, ordering. Addition of 'IO%CSOILFRZ' option
!!      (B. Decharme) 03/2009  BUG : effect of insolation due to vegetation cover
!!                                  at 1 for bare soil
!!      (B. Decharme) 07/2012  Time spliting for soil ice
!!      (A.Boone)     02/2013  Split from isba_fluxes.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,       ONLY : XCL, XTT, XPI, XDAY, XCI, XRHOLI,     &
                            XLMTT, XRHOLW, XG, XCONDI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,   ONLY : XWGMIN, XSPHSOIL, XDRYWGHT
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
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
REAL, INTENT (IN)                   :: PTSTEP     ! model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PKSFC_IVEG
!                                      PKSFC_IVEG= non-dimensional vegetation insolation coefficient
!
REAL, DIMENSION(:), INTENT(OUT)     :: PDWGI1, PDWGI2
!                                      PDWGI1   = near-surface liquid water equivalent
!                                                 volumetric ice content tendency
!                                      PDWGI2   = deep ground liquid water equivalent
!                                                 volumetric ice content tendency
!
!*      0.2    declarations of local variables
!
REAL                        ::   ZKSOIL     ! coefficient for soil freeze/thaw
!
REAL, DIMENSION(SIZE(DMK%XCG)) ::   ZKSFC_FRZ, ZFREEZING, ZICE_MELT, ZWIM,       &
                                 ZWIT, ZWGI1, ZWGI2, ZWM, ZSOILHEATCAP,       &
                                 ZICEEFF, ZEFFIC, ZTAUICE,                    &
                                 ZWGMIN, ZTGMAX, ZMATPOT, ZDELTAT
!                                ZKSFC_FRZ    = surface insolation coefficient (kg m-2 K-1)
!                                ZFREEZING    = rate for freezing soil water (kg m-2)
!                                ZICE_MELT    = rate for melting soil ice (kg m-2)
!                                ZWIM,ZWIT    = available ice content (m3 m-3)
!                                ZWGI1,ZWGI2  = volumetric ice contents (m3 m-3)
!                                ZWM          = available liquid water content (m3 m-3)
!                                ZSOILHEATCAP = soil heat capacity (J  m-3 K-1)
!                                ZICEEFF      = effective soil ice penetration depth (m)
!                                ZEFFIC       = phase change efficiency
!                                ZMATPOT      = soil matric potential (m)
!                                ZWGMIN       = volumetric water content above which soil water can
!                                               be unfrozen (if energy and mass available)(m3 m-3)
!                                ZTGMAX       = temperature below which liquid water 
!                                               can be frozen (if energy and mass available)(K)
!                                ZDELTAT      = Freezing or melting temperature depression (K) after 
!                                               possible flux correction
!
REAL, DIMENSION(SIZE(DMK%XCG)) ::  ZWSAT_AVGZ
!                               ZWSAT_AVGZ = soil column average porosity (m3 m-3)
!
REAL, DIMENSION(SIZE(DMK%XCG)) :: ZPSNG
!                               ZPSNG = snow fractions corresponding to
!                                       dummy argument PEK%XPSNG(:)
!                                       if PEK%TSNOW%SCHEME = 'DEF' (composite
!                                       or Force-Restore snow scheme), else
!                                       they are zero for explicit snow case
!                                       as snow fluxes calculated outside of
!                                       this routine using the 
!                                       PEK%TSNOW%SCHEME = '3-L' or 'CRO' option.
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZINSOLFRZ_VEG = 0.20  ! (-)       Vegetation insolation coefficient
!
REAL, PARAMETER             :: ZINSOLFRZ_LAI = 30.0  ! (m2 m-2)  Vegetation insolation coefficient
!
REAL, PARAMETER             :: ZEFFIC_MIN    = 0.01  ! (-)   (0 <= ZEFFIC_MIN << 1)
!                                                                This parameter ensures
!                                                                a small minimum melt or freeze efficiency...
!                                                                It is numerical. When it is small, it has
!                                                                a only small impact on results, except
!                                                                that it keeps very small values of ice from persisting
!                                                                over long periods of time as they approach zero.
!                                                                If it is zero, then this effect off.
!
!
INTEGER         :: INJ, JJ
!
REAL, DIMENSION(SIZE(DMK%XCG))          :: ZWORK1, ZWORK2, ZTDIURN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization
!               --------------
IF (LHOOK) CALL DR_HOOK('ICE_SOILFR',0,ZHOOK_HANDLE)
!
ZFREEZING(:)    = 0.0
ZKSFC_FRZ(:)    = 0.0
ZEFFIC(:)       = 0.0
ZICE_MELT(:)    = 0.0
ZWGI1(:)        = 0.0
ZWIM(:)         = 0.0
ZSOILHEATCAP(:) = 0.0
ZWIT(:)         = 0.0
ZWGI2(:)        = 0.0
ZTGMAX(:)       = 0.0
ZWGMIN(:)       = 0.0
ZMATPOT(:)      = 0.0
ZWSAT_AVGZ(:)   = XUNDEF
ZDELTAT(:)      = 0.0
ZTDIURN(:)      = 0.0
!
INJ = SIZE(PEK%XTG,1)
!
!-------------------------------------------------------------------------------
!
! If ISBA-ES option in use, then snow covered surface
! fluxes calculated outside of this routine, so set
! the local snow fractions here to zero:
! 
IF(PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME == 'CRO')THEN
  ZPSNG(:)     = 0.0
ELSE
  ZPSNG(:)     = PEK%XPSNG(:)+KK%XFFG(:)
ENDIF
!
!*       1.    Melting/freezing normalized coefficient
!               ---------------------------------------
!
ZKSOIL      = (0.5 * SQRT(XCONDI*XCI*XRHOLI*XDAY/XPI))/XLMTT
!
ZTAUICE (:) = MAX(PTSTEP,PK%XTAUICE(:))
!
DO JJ=1,INJ
!-------------------------------------------------------------------------------
!*       2.     EFFECT OF THE MELTING/FREEZING 
!               ON THE SURFACE-SOIL HEAT AND ICE CONTENTS
!               ('DEF' or Force-Restore soil option)
!               -----------------------------------------
!
!        2.0    Average soil-column porosity
!               ----------------------------
!               if Force-Restore option in use, then vertical
!               profiles of soil hydrological parameters are constant,
!               so use the values in uppermost element (arbitrary)
!
  ZWSAT_AVGZ(JJ) = KK%XWSAT(JJ,1)
!
!               Influence of vegetation insolation on surface:
!
  ZKSFC_FRZ(JJ) = ZKSOIL * PKSFC_IVEG(JJ)
!
ENDDO
!*       2.2    Water freezing
!               --------------
!
IF(IO%CSOILFRZ == 'LWT')THEN
!
! use option to control phase changes based on a relationship
! between the unfrozen liquid water content and temperature.
! Uses the Clapp and Hornberger model for water potential.
! The energy-limit method used by Boone et al. 2000 and
! Giard and Bazile (2000) is the default. 
!
  DO JJ=1,INJ
    ZMATPOT(JJ)   = MIN(KK%XMPOTSAT(JJ,1), XLMTT*(PEK%XTG(JJ,1)-XTT)/(XG*PEK%XTG(JJ,1)) )
    ZWGMIN(JJ)    = ZWSAT_AVGZ(JJ)*( (ZMATPOT(JJ)/KK%XMPOTSAT(JJ,1))**(-1./KK%XBCOEF(JJ,1)) )

    ZMATPOT(JJ)   = KK%XMPOTSAT(JJ,1)*( (PEK%XWG(JJ,1)/ZWSAT_AVGZ(JJ))**(-KK%XBCOEF(JJ,1)) )
    ZTGMAX(JJ)    = XLMTT*XTT/(XLMTT - XG* ZMATPOT(JJ))
  ENDDO
ELSE
  ZWGMIN(:)    = XWGMIN
  ZTGMAX(:)    = XTT
ENDIF
!
DO JJ=1,INJ
! 
  ZDELTAT(JJ)  = PEK%XTG(JJ,1) - ZTGMAX(JJ) ! initial temperature depression
!
  ZWORK2(JJ) = XRHOLW*PK%XDG(JJ,1)
  ZEFFIC(JJ)    = MAX(ZEFFIC_MIN,(PEK%XWG(JJ,1)-XWGMIN)/ZWSAT_AVGZ(JJ))
  ZFREEZING(JJ) = MIN( MAX(0.0,PEK%XWG(JJ,1)-ZWGMIN(JJ))*ZWORK2(JJ),    &  
                  ZKSFC_FRZ(JJ)*ZEFFIC(JJ)*MAX( -ZDELTAT(JJ), 0.) )
!
!*       2.3    Ground Ice melt
!               ---------------
!
  ZEFFIC(JJ)    =  MAX(ZEFFIC_MIN,PEK%XWGI(JJ,1)/(ZWSAT_AVGZ(JJ)-XWGMIN))
  ZICE_MELT(JJ) = MIN( PEK%XWGI(JJ,1)*ZWORK2(JJ),                      &
                  ZKSFC_FRZ(JJ)*ZEFFIC(JJ)*MAX( ZDELTAT(JJ), 0. ) )
!
!*       2.4    Ice reservoir evolution
!               -----------------------
!
! Melting of ice/freezing of water:
!
  ZWGI1(JJ) = PEK%XWGI(JJ,1) + (PTSTEP/ZTAUICE(JJ))*(1.0-ZPSNG(JJ))*        &
              (ZFREEZING(JJ) - ZICE_MELT(JJ))/ZWORK2(JJ) 
!
!
  ZWGI1(JJ)  = MAX( ZWGI1(JJ) , 0.             )
  ZWGI1(JJ)  = MIN( ZWGI1(JJ) , ZWSAT_AVGZ(JJ)-XWGMIN)
!
! Time tendency:
!
  PDWGI1(JJ) = ZWGI1(JJ) - PEK%XWGI(JJ,1)
!
!
!*       2.5    Effect on temperature
!               ---------------------
!
  PEK%XTG(JJ,1)   = PEK%XTG(JJ,1) + PDWGI1(JJ)*XLMTT*DMK%XCT(JJ)*ZWORK2(JJ)
!
!-------------------------------------------------------------------------------
!
!*       3.     EFFECT OF THE MELTING/FREEZING 
!               ON THE DEEP-SOIL HEAT AND ICE CONTENTS
!               ('DEF' or Force-Restore soil option)
!               --------------------------------------
!
  ZWORK1(JJ) = PK%XDG(JJ,1)/PK%XDG(JJ,2)
!*       3.1  Available Deep ice content
!             --------------------------
!
  ZWIM(JJ) = ( PEK%XWGI(JJ,2) - ZWORK1(JJ) * PEK%XWGI(JJ,1) )  / ( 1. - ZWORK1(JJ) )
!
  ZWIM(JJ) = MAX(0.,ZWIM(JJ))  ! Just in case of round-off errors
!
!*       3.2  Deep liquid water content
!             -------------------------
!
  ZWM(JJ)  = ( PEK%XWG(JJ,2) - ZWORK1(JJ) * PEK%XWG(JJ,1) )  / ( 1. - ZWORK1(JJ) )
!
!*       3.3    Water freezing
!               --------------
!
! Total soil volumetric heat capacity [J/(m3 K)]:
!
  ZSOILHEATCAP(JJ) = XCL*XRHOLW*PEK%XWG(JJ,2)  +                           &
                     XCI*XRHOLI*PEK%XWGI(JJ,2) +                           &
                     XSPHSOIL*XDRYWGHT*(1.0-ZWSAT_AVGZ(JJ))*(1.0-ZWSAT_AVGZ(JJ))
!
! Soil thickness which corresponds to T2 (m): 2 times the diurnal
! surface temperature wave penetration depth as T2 is the average
! temperature for this layer:
!
  ZTDIURN(JJ)   = MIN(PK%XDG(JJ,2), 4./(ZSOILHEATCAP(JJ)*DMK%XCG(JJ)))
!
! Effective soil ice penetration depth (m):
!
  ZICEEFF(JJ)   = (PEK%XWGI(JJ,2)/(PEK%XWGI(JJ,2)+PEK%XWG(JJ,2)))*PK%XDG(JJ,2)
!
ENDDO
!
IF(IO%CSOILFRZ == 'LWT')THEN
!
! as for the surface layer (above)JJ 
! Note also that if the 'DIF'
! soil option is not in force, then the soil parameters are assumed
! to be homogeneous (in the verticalJJ thus we use 1st element of 2nd dimension
! of the 2D-soil parameter arrays).
!
  DO JJ=1,INJ

    ZMATPOT(JJ)   = MIN(KK%XMPOTSAT(JJ,1), XLMTT*(PEK%XTG(JJ,2)-XTT)/(XG*PEK%XTG(JJ,2)) )
    ZWGMIN(JJ)    = ZWSAT_AVGZ(JJ)*( (ZMATPOT(JJ)/KK%XMPOTSAT(JJ,1))**(-1./KK%XBCOEF(JJ,1)) )

    ZMATPOT(JJ)   = KK%XMPOTSAT(JJ,1)*( (PEK%XWG(JJ,2)/ZWSAT_AVGZ(JJ))**(-KK%XBCOEF(JJ,1)) )
    ZTGMAX(JJ)    = XLMTT*XTT/(XLMTT - XG* ZMATPOT(JJ))
  ENDDO
ELSE
  ZWGMIN(:)    = XWGMIN
  ZTGMAX(:)    = XTT
ENDIF
!
! Allow freezing by T2 up to a certain depth so that
! T2 energy can not be used to freeze soil water
! at levels sufficiently deep in the soil.
!
DO JJ=1,INJ
!
  ZDELTAT(JJ)  = PEK%XTG(JJ,2) - ZTGMAX(JJ) ! initial temperature depression 
!  
  ZWORK1(JJ) = PK%XDG(JJ,1)/PK%XDG(JJ,2)
  ZWORK2(JJ) = XRHOLW*(PK%XDG(JJ,2)-PK%XDG(JJ,1))

  ZFREEZING(JJ) = 0.0
  IF (ZICEEFF(JJ) <= ZTDIURN(JJ)) THEN
!
    ZEFFIC(JJ)    = MAX(ZEFFIC_MIN, MAX(0.0,ZWM(JJ) - XWGMIN)/ZWSAT_AVGZ(JJ))
    ZFREEZING(JJ) = MIN( MAX(0.0, ZWM(JJ) - ZWGMIN(JJ))* ZWORK2(JJ),            &
                     ZKSOIL*ZEFFIC(JJ)*MAX( -ZDELTAT(JJ) , 0. ) )
  ENDIF
!
!
!*       3.4    Ground Ice melt
!               ---------------
!
  ZEFFIC(JJ)    = MAX(ZEFFIC_MIN, ZWIM(JJ)/(ZWSAT_AVGZ(JJ)-XWGMIN))
  ZICE_MELT(JJ) = MIN( ZWIM(JJ)*ZWORK2(JJ),             &
                  ZKSOIL*ZEFFIC(JJ)*MAX( ZDELTAT(JJ) , 0. ) )
!
!
!*       3.5    Deep-part of deep-soil Ice reservoir evolution
!               ----------------------------------------------
!
  ZWIT(JJ)   = ZWIM(JJ) + (PTSTEP/ZTAUICE(JJ))*(1.0-ZPSNG(JJ))*       &
               ((ZFREEZING(JJ) - ZICE_MELT(JJ))/ ZWORK2(JJ))
!
  ZWIT(JJ)   = MAX( ZWIT(JJ) , 0.             )
  ZWIT(JJ)   = MIN( ZWIT(JJ) , ZWSAT_AVGZ(JJ)-XWGMIN)
!
!
!*       3.6    Add reservoir evolution from surface freezing (WI2 contains WI1)
!               ----------------------------------------------------------------
!
  ZWGI2(JJ)  = (1.-ZWORK1(JJ))*ZWIT(JJ) +  ZWORK1(JJ)*ZWGI1(JJ)
!
  PDWGI2(JJ) = ZWGI2(JJ) - PEK%XWGI(JJ,2)
!
!
!*       3.7    Effect on temperature
!               ---------------------
!
  PEK%XTG(JJ,2) = PEK%XTG(JJ,2) + PDWGI2(JJ)*XLMTT*DMK%XCG(JJ)*XRHOLW*PK%XDG(JJ,2)
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ICE_SOILFR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE ICE_SOILFR











