!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE FORCING_VERT_SHIFT(PZS_ATM,PZS_SURF,PTA_ATM,PQA_ATM,PPA_ATM, &
                                     PRHOA_ATM,PLW_ATM,PRAIN_ATM,PSNOW_ATM,    &
                                     PTA_SURF,PQA_SURF,PPA_SURF,PRHOA_SURF,    &
                                     PLW_SURF,PRAIN_SURF,PSNOW_SURF            )  
!      #########################################
!
!
!!****   *FORCING_VERT_SHIFT* - routine to shift atmospheric forcing to another altitude
!!
!!
!!     PURPOSE
!!     -------
!
!!**   METHOD
!!     ------
!!                      
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!       V. Masson
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        07/2003
!!       B. Decharme     06/2013 bug : qa_surf must be <= qsat_surf 
!!                               add longwave raditions and rain snow partition
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
USE MODD_CSTS,      ONLY : XRD, XG, XRV, XTT
USE MODD_ATM_CST,   ONLY : XCLIM_T_GRAD
USE MODD_SURF_ATM,  ONLY : LVSHIFT_LW, LVSHIFT_PRCP
!
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_ATM     ! orography of atmospheric grid
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_SURF    ! orography of surface     grid
REAL,    DIMENSION(:), INTENT(IN)  :: PTA_ATM     ! temperature at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PQA_ATM     ! humidity    at atmospheric altitude (kg/m3)
REAL,    DIMENSION(:), INTENT(IN)  :: PPA_ATM     ! pressure    at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PRHOA_ATM   ! density     at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PLW_ATM     ! lw rad      at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PRAIN_ATM   ! rainfall    at atmospheric altitude
REAL,    DIMENSION(:), INTENT(IN)  :: PSNOW_ATM   ! snowfall    at atmospheric altitude
!
REAL,    DIMENSION(:), INTENT(OUT) :: PTA_SURF    ! temperature at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PQA_SURF    ! humidity    at surface     altitude (kg/m3)
REAL,    DIMENSION(:), INTENT(OUT) :: PPA_SURF    ! pressure    at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PRHOA_SURF  ! density     at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PLW_SURF    ! lw rad      at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PRAIN_SURF  ! rainfall    at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PSNOW_SURF  ! snowfall    at surface     altitude
!
!*       0.2 declarations of local variables
!
REAL, PARAMETER                  :: ZVAPCOEF  = 0.622
REAL, PARAMETER                  :: ZEMISCOEF = 1.08
REAL, PARAMETER                  :: ZTEMPCOEF = 2016.0
!
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQA_ATM    ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQA_SURF   ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQSAT_ATM  ! air humidity at saturation (kg/kg)
REAL, DIMENSION(SIZE(PQA_ATM  )) :: ZQSAT_SURF ! air humidity at saturation (kg/kg)
REAL, DIMENSION(SIZE(PRHOA_ATM)) :: ZRHOA_ATM  ! approximated density
REAL, DIMENSION(SIZE(PRHOA_ATM)) :: ZRHOA_SURF ! approximated density
!
REAL, DIMENSION(SIZE(PLW_ATM  )) :: ZVAP_ATM   ! approximated vapour pressure
REAL, DIMENSION(SIZE(PLW_ATM  )) :: ZVAP_SURF  ! approximated vapour pressure
REAL, DIMENSION(SIZE(PLW_ATM  )) :: ZEMIS_ATM  ! approximated emissivity
REAL, DIMENSION(SIZE(PLW_ATM  )) :: ZEMIS_SURF ! approximated emissivity
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FORCING_VERT_SHIFT',0,ZHOOK_HANDLE)
!
!*       1.  climatological gradient for temperature
!            ---------------------------------------
!
PTA_SURF(:) = PTA_ATM(:) + XCLIM_T_GRAD * (PZS_SURF(:) - PZS_ATM(:))
!
!-------------------------------------------------------------------------------
!
!*       2.  hydrostatism for pressure
!            -------------------------
!
ZQSAT_ATM (:) = QSAT(PTA_ATM (:),PPA_ATM (:))
!
ZQA_ATM(:) = MIN(PQA_ATM(:)/PRHOA_ATM(:),ZQSAT_ATM(:))
!
PPA_SURF(:) = PPA_ATM(:) * EXP ( - XG/XRD/(0.5*(PTA_ATM(:)+PTA_SURF(:))*(1.+((XRV/XRD)-1.)*ZQA_ATM(:))) &
                              * (PZS_SURF(:)-PZS_ATM(:))                                              )  
!
!-------------------------------------------------------------------------------
!
!*       3.  conservation of relative humidity for humidity
!            ----------------------------------------------
!
!
ZQSAT_SURF(:) = QSAT(PTA_SURF(:),PPA_SURF(:))
!
ZQA_SURF(:) = MIN(ZQA_ATM(:)*ZQSAT_SURF(:)/ZQSAT_ATM(:),ZQSAT_SURF(:))
!
!-------------------------------------------------------------------------------
!
!*       4.  estimation of air density from temperature and humidity
!            -------------------------------------------------------
!
ZRHOA_ATM (:) = PPA_ATM (:) / XRD /  PTA_ATM (:) / ( 1.+((XRV/XRD)-1.)*ZQA_ATM (:) )
ZRHOA_SURF(:) = PPA_SURF(:) / XRD /  PTA_SURF(:) / ( 1.+((XRV/XRD)-1.)*ZQA_SURF(:) )
!
PRHOA_SURF(:) = PRHOA_ATM(:) * ZRHOA_SURF(:) / ZRHOA_ATM (:)
!
!-------------------------------------------------------------------------------
!
!*       5.  new humidity in kg/m3
!            ---------------------
!
PQA_SURF(:) = ZQA_SURF(:) * PRHOA_SURF(:)
!
!-------------------------------------------------------------------------------
!
!*       6.  new longwave radiations
!            -----------------------
!
IF(LVSHIFT_LW)THEN
!        
! Vapour pressures and emissivities (Cosgrove et al., JGR, 2003)
!
  ZVAP_ATM (:) = ZQA_ATM (:) * PPA_ATM(:)  / ZVAPCOEF
  ZVAP_SURF(:) = ZQA_SURF(:) * PPA_SURF(:) / ZVAPCOEF
!
  ZVAP_ATM (:) = EXP(LOG(ZVAP_ATM (:))*(PTA_ATM (:)/ZTEMPCOEF))
  ZVAP_SURF(:) = EXP(LOG(ZVAP_SURF(:))*(PTA_SURF(:)/ZTEMPCOEF))
!
  ZEMIS_ATM (:) = (1.0-EXP(ZVAP_ATM (:)))
  ZEMIS_SURF(:) = (1.0-EXP(ZVAP_SURF(:)))
!
! Radiations
!
  PLW_SURF(:) = PLW_ATM(:) * (ZEMIS_SURF(:)/ZEMIS_ATM(:)) * (PTA_SURF(:)/PTA_ATM(:))**4
!
ELSE
!
  PLW_SURF(:) = PLW_ATM(:)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       7.  new rain/snow partition
!            -----------------------
!
IF(LVSHIFT_PRCP)THEN
!
  WHERE(PTA_SURF(:)>=XTT+1.0)
        PRAIN_SURF(:) = PRAIN_ATM(:) + PSNOW_ATM(:)
        PSNOW_SURF(:) = 0.0
  ELSEWHERE
        PRAIN_SURF(:) = 0.0
        PSNOW_SURF(:) = PRAIN_ATM(:) + PSNOW_ATM(:)
  ENDWHERE
!
ELSE
!
  PRAIN_SURF(:) = PRAIN_ATM(:)
  PSNOW_SURF(:) = PSNOW_ATM(:)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FORCING_VERT_SHIFT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FORCING_VERT_SHIFT
