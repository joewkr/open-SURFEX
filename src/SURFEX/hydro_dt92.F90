!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_DT92(PTSTEP,                                 &
                              PRUNOFFB, PWWILT,                       &
                              PRUNOFFD, PWSAT,                        &
                              PWG2, PWGI2,                            &
                              PPG, PRUISDT                            )  
!     #####################################################################
!
!!****  *HYDRO_DT92*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the water variables, i.e., the superficial
!     and deep-soil volumetric water content (wg and w2), the equivalent
!     liquid water retained in the vegetation canopy (Wr), the equivalent
!     water of the snow canopy (Ws), and also of the albedo and density of
!     the snow (i.e., ALBS and RHOS).  Also determine the runoff and drainage
!     into the soil.
!         
!     
!!**  METHOD
!!    ------
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
!!                  16/05/02 (A. Boone) comments, F90 code standardization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,ONLY : XRHOLW
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
REAL, INTENT(IN)                  :: PTSTEP
!                                    timestep of the integration (s)
!
REAL, DIMENSION(:), INTENT(IN)    :: PWWILT   ! the wilting point volumetric
!                                             ! water content (m3 m-3)
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFFB ! slope of the runoff curve (-)
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFFD, PWSAT
!                                    PRUNOFFD = soil depth over which degree of saturation
!                                               used for runoff calculation (m)
!                                    PWSAT    = saturation volumetric water content
!                                               of the soil (m3 m-3)
!
REAL, DIMENSION(:), INTENT(IN)    :: PWG2, PWGI2
!                                    PWG2 = bulk root-soil moisture at 't+dt' (m3 m-3)
!                                    PWGI2 = bulk deep-soil ice at 't+dt' (m3 m-3)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PPG       
!                                    PPG = enters as rainfall/Canopy drip/snowmelt 
!                                          throughfall rate, leaves as infiltration
!                                          rate for Force-Restore method, and potential
!                                          infiltration rate for diffusion method (kg m-2 s-1)
REAL, DIMENSION(:), INTENT(OUT)   :: PRUISDT
!                                    PRUISDT = sub-grid surface runoff rate (kg m-2 s-1)
!
!
!*      0.2    declarations of local variables
!
! 
REAL, DIMENSION(SIZE(PRUNOFFD))    :: ZPAS, ZPG_INI, ZWG2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                     ZWG2   = Total vol. water content
!                                              of layer for calculating runoff (m3 m-3)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_DT92',0,ZHOOK_HANDLE)
ZPAS(:)    = 0.
ZPG_INI(:) = 0.
ZWG2(:)    = 0.
!
PRUISDT(:) = 0.
!
!-------------------------------------------------------------------------------
!
!
!*       1.     Dumenil et Todini (1992)  RUNOFF SCHEME
!               ---------------------------------------
!
ZPG_INI(:)= PPG(:)
!
ZWG2(:)   = MIN(PWSAT(:), PWG2(:) + PWGI2(:)) 
!
! Setting the expression below to 0 and solving for PG yields the critical throughfall rate:
!
ZPAS(:)   = (1.- (ZWG2(:)-PWWILT(:))/(PWSAT(:)-PWWILT(:)) )**(1./ (1.+PRUNOFFB(:)) ) -   &
                PPG(:)*PTSTEP/(XRHOLW*PRUNOFFD(:) )/( (1. + PRUNOFFB(:))* (PWSAT(:)-PWWILT(:)) )  
!
ZPAS(:)   = MAX(0.0, ZPAS(:))  ! Limit it to within a physical range:
!
! Surface runoff calculation:
! If PAS is <= 0 (i.e. throughfall rate is large enough), then method 
! collapses into a saturated bucket type model.
!
PRUISDT(:)  = PPG(:)*PTSTEP/(XRHOLW*PRUNOFFD(:) ) - ( PWSAT(:)-ZWG2(:) ) &
                 + (PWSAT(:)-PWWILT(:))* ( ZPAS(:)**(1.+PRUNOFFB(:)) )  
!
PRUISDT(:) = MAX(0.0, PRUISDT(:))
!
! Reduce infiltration into the soil by the runoff:
!
PPG(:)     = PPG(:) - PRUISDT(:)/PTSTEP*XRHOLW*PRUNOFFD(:)
!
! Supress numerical artifacts:
!
WHERE (PPG(:)<=0. .OR. PRUISDT(:)<=0.)
  PRUISDT(:) = 0.
  PPG(:)     = ZPG_INI(:)
END WHERE
!
! supress runoff over sufficiently dry soils: HERE chosen to be if the average 
! water content is less than the wilting point:
!
WHERE (ZWG2(:)<=PWWILT(:)) 
  PRUISDT(:) = 0.
  PPG(:)     = ZPG_INI(:)
END WHERE
IF (LHOOK) CALL DR_HOOK('HYDRO_DT92',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE HYDRO_DT92
