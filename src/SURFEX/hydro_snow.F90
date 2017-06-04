!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SNOW(OGLACIER, PTSTEP, PVEGTYPE, PSR, PLES, PMELT, TPSNOW, PPG_MELT )  
!     #####################################################################
!
!!****  *HYDRO_SNOW*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates i) Snow water transfer to soil for both snow scheme options.
!     ii) the evolution of the snowpack using the Force-Restore
!     option of Douville et al. (1995): 'DEF'
!     Calculate the snow cover liquid water equivalent (Ws), the albedo and density of
!     the snow (i.e., SNOWALB and SNOWRHO).  Also determine the runoff and drainage
!     into the soil.
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!REAL, DIMENSION(:), INTENT(INOUT) :: PTG
!                                      PTG = surface temperature at 't'

!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    14/03/95 
!!                  31/08/98 (V. Masson and F. Habets) add Dumenil et Todini
!!                           runoff scheme
!!                  31/08/98 (V. Masson and A. Boone) add the third soil-water
!!                           reservoir (WG3,D3)
!!                  14/05/02 (A. Boone) snow only, and skip code if '3-L' option in force
!!                   03/2009 (B. Decharme) Consistency with Arpege permanent snow/ice treatment
!!                                          (LGLACIER)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TYPE_SNOW, ONLY : SURF_SNOW
!
USE MODD_CSTS,        ONLY : XLSTT, XLMTT, XDAY
USE MODD_SNOW_PAR,    ONLY : XANS_T, XANS_TODRY, XANSMIN, XANSMAX, &
                               XRHOSMAX, XRHOSMIN, XWCRN, XAGLAMIN,  &
                               XAGLAMAX  
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
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
LOGICAL, INTENT(IN)               :: OGLACIER   ! True = Over permanent snow and ice, 
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                ! False = No specific treatment
REAL, INTENT(IN)                  :: PTSTEP
!                                    timestep of the integration
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE ! fraction of each vegetation
REAL, DIMENSION(:), INTENT(IN)    :: PSR,  PLES, PMELT
!                                    PSR = snow rate
!                                    PLES = latent heat of sublimation over the snow
!                                    PMELT = melting rate of snow
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW
REAL, DIMENSION(:), INTENT(INOUT) :: PPG_MELT
!                                    TPSNOW%WSNOW(:,1,1) = equivalent water content of the
!                                    PPG_MELT = total water reaching the ground
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSR)) :: ZSNOWSWEM, ZWSX,  ZANSMIN, ZANSMAX
!                             Prognostic variables of ISBA at 't-dt'
!                             ZSNOWSWEM = equivalent water content of the
!                                         snow reservoir
!                             ZANSMIN = Minimum glacier albedo
!                             ZANSMAX = Maximum glacier albedo
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HYDRO_SNOW',0,ZHOOK_HANDLE)                                                        
!-------------------------------------------------------------------------------
!
!*              Douville et al. (1995) 'DEF' snow option
!               ----------------------------------------
!        
!*       1.     Initialize:
!               -----------
!
ZWSX(:)       = 0.0
ZANSMIN(:)    = XANSMIN
ZANSMAX(:)    = XANSMAX
!
!
!*       2.     Fields at time t-dt
!               -------------------
!
ZSNOWSWEM (:) = TPSNOW%WSNOW(:,1)    
!
!*       3.     EVOLUTION OF THE SNOWPACK ('DEF' OPTION)
!               ----------------------------------------
!
!*       3.A    EVOLUTION OF THE EQUIVALENT WATER CONTENT snowSWE ('DEF' option)
!               --------------------------------------------------------------
!
!                                           evolution of Ws (without melting)
!
TPSNOW%WSNOW(:,1) = ZSNOWSWEM(:) + PTSTEP * ( PSR(:) - PLES(:)/XLSTT - PMELT(:))
!
!                                           melting of snow: more liquid water
!                                                            reaches the surface
!
PPG_MELT(:)     = PPG_MELT(:) + PMELT(:)  
!   
! removes very small values due to computation precision
!
WHERE(TPSNOW%WSNOW(:,1) < 1.0E-10) TPSNOW%WSNOW(:,1) = 0.
!
!-------------------------------------------------------------------------------
!
!*       3.B    EVOLUTION OF SNOW ALBEDO 
!               ------------------------
!
IF(OGLACIER)THEN
  ZANSMIN(:) = XAGLAMIN * PVEGTYPE(:,NVT_SNOW) + XANSMIN * (1.0-PVEGTYPE(:,NVT_SNOW))
  ZANSMAX(:) = XAGLAMAX * PVEGTYPE(:,NVT_SNOW) + XANSMAX * (1.0-PVEGTYPE(:,NVT_SNOW))
ELSE
  ZANSMIN(:) = XANSMIN
  ZANSMAX(:) = XANSMAX
ENDIF
!                                       the evolution of the snow albedo differs
!                                       if there is melting or not
!
WHERE (TPSNOW%WSNOW(:,1) > 0.0 )
  !
  WHERE ( ZSNOWSWEM > 0.0)
    !
    ! when there is melting 
    WHERE ( PMELT > 0.0 )
      TPSNOW%ALB(:) = (TPSNOW%ALB(:)-ZANSMIN(:))*EXP(-XANS_T*PTSTEP/XDAY) + ZANSMIN(:) &
                       + PSR(:)*PTSTEP/XWCRN*(ZANSMAX(:)-ZANSMIN(:))  
      ! when there is no melting
    ELSEWHERE 
      TPSNOW%ALB(:) = TPSNOW%ALB(:) - XANS_TODRY*PTSTEP/XDAY   &
                       + PSR(:)*PTSTEP/XWCRN*(ZANSMAX(:)-ZANSMIN(:))  
    END WHERE
    !
  ELSEWHERE (ZSNOWSWEM == 0.0)
    !
    ! new snow covered surface
    TPSNOW%ALB(:) = ZANSMAX(:)
  END WHERE
  !
  ! limits of the albedo
  TPSNOW%ALB(:) = MIN( ZANSMAX(:), TPSNOW%ALB(:) )
  TPSNOW%ALB(:) = MAX( ZANSMIN(:), TPSNOW%ALB(:) )
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       3.C    EVOLUTION OF SNOW DENSITY 
!               -------------------------
!
!                                      as for the snow albedo, the density's
!                                      evolution will depend whether or not
!                                      the snow is melting
!
WHERE ( TPSNOW%WSNOW(:,1) > 0.0 ) 
  WHERE ( ZSNOWSWEM > 0.0 ) 
    ZWSX(:)     = MAX( TPSNOW%WSNOW(:,1),PSR(:)*PTSTEP)
    TPSNOW%RHO(:,1) = (TPSNOW%RHO(:,1)-XRHOSMAX)*EXP(-XANS_T*PTSTEP/XDAY) + XRHOSMAX
    TPSNOW%RHO(:,1) = ( (ZWSX(:)-PSR(:)*PTSTEP) * TPSNOW%RHO(:,1)     &
                         + (PSR(:)*PTSTEP) * XRHOSMIN ) / ZWSX(:)  
  ELSEWHERE ( ZSNOWSWEM == 0.0) 
    TPSNOW%RHO(:,1) = XRHOSMIN
  END WHERE
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       4.     No SNOW
!               -------
!
WHERE ( TPSNOW%WSNOW(:,1) == 0.0 ) 
  TPSNOW%RHO(:,1) = XUNDEF 
  TPSNOW%ALB(:) = XUNDEF 
END WHERE
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE HYDRO_SNOW
