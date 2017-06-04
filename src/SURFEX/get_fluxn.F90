!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_FLUX_n (DGO, D, &
                             HPROGRAM,KI,PRN,PH,PLE,PLEI,PGFLUX,PT2M,PQ2M,   &
                            PHU2M,PZON10M,PMER10M,PSURFLWNET,PSURFSWNET,PCD,&  
                            PEVAP, PSUBL                                    )  
!     ########################################
!
!!****  *GET_FLUX_n* - routine to get some surface fields
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
!!      Original    01/2004
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,        ONLY   : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
!
CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
INTEGER,              INTENT(IN)     :: KI        ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PRN       ! Net radiation at surface    (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PH        ! Sensible heat flux          (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PLE       ! Total Latent heat flux      (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PLEI      ! Solid Latent heat flux      (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PGFLUX    ! Net soil-vegetation flux    (W/m2)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PT2M      ! Air temperature at 2 meters (K)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQ2M      ! Air humidity at 2 meters    (kg/kg)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PHU2M     ! Air relative humidity at 2 meters (-)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZON10M   ! zonal Wind at 10 meters     (m/s)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PMER10M   ! meridian Wind at 10 meters  (m/s)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSURFLWNET   ! LW net at the surface
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSURFSWNET   ! SW net at the surface
REAL, DIMENSION(KI),  INTENT(OUT)    :: PCD       ! exchange coeficient at the surface
REAL, DIMENSION(KI),  INTENT(OUT)    :: PEVAP     ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSUBL     ! Sublimation (kg/m2/s)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_FLUX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGO%LSURF_BUDGET)      THEN 
        PRN       = D%XRN      
        PH        = D%XH  
        PLE       = D%XLE 
        PLEI      = D%XLEI 
        PGFLUX    = D%XGFLUX 
        PSURFLWNET= D%XLWD-D%XLWU
        PSURFSWNET= D%XSWD-D%XSWU
        PEVAP     = D%XEVAP
        PSUBL     = D%XSUBL
   ELSE 
        PRN       = XUNDEF
        PH        = XUNDEF
        PLE       = XUNDEF
        PLEI      = XUNDEF
        PGFLUX    = XUNDEF
        PSURFLWNET= XUNDEF
        PSURFSWNET= XUNDEF  
        PEVAP     = XUNDEF
        PSUBL     = XUNDEF        
ENDIF           
!
IF (DGO%N2M>0)      THEN 
        PT2M      = D%XT2M
        PQ2M      = D%XQ2M
        PHU2M     = D%XHU2M
        PZON10M   = D%XZON10M
        PMER10M   = D%XMER10M
   ELSE 
        PT2M     = XUNDEF
        PQ2M     = XUNDEF
        PHU2M    = XUNDEF
        PZON10M  = XUNDEF
        PMER10M  = XUNDEF
ENDIF   
!
IF (DGO%LCOEF) THEN
  PCD      = D%XCD
ELSE
  PCD      = XUNDEF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('GET_FLUX_N',1,ZHOOK_HANDLE)
!==============================================================================
!
END SUBROUTINE GET_FLUX_n
