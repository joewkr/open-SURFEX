!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_GLACIER (PTSTEP, PSR, PEK, PICEFLUX)
!     ########################################################################
!
!!****  *HYDRO_GLACIER*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the ice runoff fluxes over permanent snow area with LGLACIER
!     option
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
!!    AUTHOR
!!    ------
!!      B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/09 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
!
USE MODD_CSTS,     ONLY : XDAY
USE MODD_SNOW_PAR, ONLY : XRHOSMAX, XHGLA, XSNOWDMIN, XRHOSMAX_ES
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!                                       KTSTEP = timestep [s]
!
REAL, DIMENSION(:), INTENT(IN)       :: PSR
!                                       PSR      = Snowfall    [kg/m²s]
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, DIMENSION(:), INTENT(OUT)      :: PICEFLUX
!                                       PICEFLUX = Ice flux from the Snowfall reservoir [kg/m²s]
!
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZTAU=365.25 !days
!
REAL, DIMENSION(SIZE(PSR)) :: ZGLASTO,ZSTOMAX,ZFLUX,ZSR,ZSWE
REAL, DIMENSION(SIZE(PSR)) :: ZSNOWD
!
REAL              ::ZRHOSMAX
!
INTEGER :: JWRK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_GLACIER',0,ZHOOK_HANDLE)
!
ZGLASTO (:) = PEK%XICE_STO(:)
ZSTOMAX (:) = 0.0
ZFLUX   (:) = 0.0
ZSR     (:) = 0.0
ZSWE    (:) = 0.0
!
PICEFLUX(:) = 0.0
!
!-------------------------------------------------------------------------------
!Ice accumulation only if snow amount is > to 33.3 meters of aged snow
!
IF(PEK%TSNOW%SCHEME/='3-L' .AND. PEK%TSNOW%SCHEME/='CRO')THEN
  ZRHOSMAX = XRHOSMAX
  ZSWE(:)  = PEK%TSNOW%WSNOW(:,1)
ELSE
  ZRHOSMAX=XRHOSMAX_ES
  DO JWRK=1,SIZE(PEK%TSNOW%WSNOW,2)
     ZSWE(:) = ZSWE(:) + PEK%TSNOW%WSNOW(:,JWRK)
  END DO
ENDIF
!
WHERE(ZSWE(:)>XHGLA*ZRHOSMAX)
     ZSR(:) = PSR(:)
ELSEWHERE
     ZSR(:) = 0.0
ENDWHERE
!
!Snow storage calculation
!
PEK%XICE_STO(:) = (ZGLASTO(:)+PTSTEP*ZSR(:))/(1.0+PTSTEP/(ZTAU*XDAY))
!
!supress numerical artifacs
!
ZSTOMAX(:) = ZSR(:)*PTSTEP+ZGLASTO(:)
!
PEK%XICE_STO(:) = MIN(ZSTOMAX(:),PEK%XICE_STO(:))
!
!Ice flux calculation                
!
ZFLUX(:) = (ZGLASTO(:)-PEK%XICE_STO(:))/PTSTEP+ZSR(:)
!      
!supress numerical artifacs
!
PICEFLUX(:) = MAX(0.0,ZFLUX(:))
PEK%XICE_STO(:) = PEK%XICE_STO(:) + PICEFLUX(:)-ZFLUX(:)             
!
WHERE(PEK%XICE_STO(:)<=1.E-10)PEK%XICE_STO(:)=0.0
!
!-------------------------------------------------------------------------------
!Snow pack update
!
IF(PEK%TSNOW%SCHEME/='3-L' .AND. PEK%TSNOW%SCHEME/='CRO')THEN
!
  WHERE(PEK%TSNOW%WSNOW(:,1)<=XHGLA*ZRHOSMAX)PICEFLUX(:)=0.0
  PEK%TSNOW%WSNOW(:,1)=PEK%TSNOW%WSNOW(:,1)-PICEFLUX(:)*PTSTEP
!
ELSE
!
  WHERE(ZSWE(:)<=XHGLA*ZRHOSMAX)PICEFLUX(:)=0.0
!
! Snow total depth
  ZSNOWD(:) = 0.
  DO JWRK=1,SIZE(PEK%TSNOW%WSNOW,2)
     ZSNOWD(:) = ZSNOWD(:) + PEK%TSNOW%WSNOW(:,JWRK)/PEK%TSNOW%RHO(:,JWRK)
  END DO
!
! Flux
  DO JWRK=1,SIZE(PEK%TSNOW%WSNOW,2)
     ZFLUX(:) = PICEFLUX(:)*(PEK%TSNOW%WSNOW(:,JWRK)/PEK%TSNOW%RHO(:,JWRK)) &
                /MAX(ZSNOWD(:),0.0001)
     PEK%TSNOW%WSNOW(:,JWRK)=PEK%TSNOW%WSNOW(:,JWRK)-ZFLUX(:)*PTSTEP
  END DO
!
ENDIF
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HYDRO_GLACIER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE HYDRO_GLACIER
