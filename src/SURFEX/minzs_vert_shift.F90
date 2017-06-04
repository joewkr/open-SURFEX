!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE MINZS_VERT_SHIFT(D, PZS_MOY, PZS_MIN, PPS_MIN, PRHOA_2M_MIN  )  
!      #########################################
!
!
!!****   *MINZS_VERT_SHIFT* - routine to shift 2m variables to 2m variables 
!!                            above the minimum orography of the grid mesh
!!
!!
!!     PURPOSE
!!     -------
!
!!**   METHOD
!!     ------
!!
!!     Same method like in forcing_vert_shift.F90
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
!!       B. Decharme
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        06/2013
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE MODD_CSTS,    ONLY : XRD, XG, XRV
USE MODD_ATM_CST, ONLY : XCLIM_T_GRAD
!
USE MODE_THERMOS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
TYPE(DIAG_t), INTENT(INOUT) :: D
!
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_MOY    ! mean orography of atmospheric grid
REAL,    DIMENSION(:), INTENT(IN)  :: PZS_MIN    ! min orography of atmospheric grid
!
REAL,    DIMENSION(:), INTENT(OUT) :: PPS_MIN    ! pressure    at surface     altitude
REAL,    DIMENSION(:), INTENT(OUT) :: PRHOA_2M_MIN  ! density     at surface     altitude
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(D%XQ2M  )) :: ZQA_2M       ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(D%XQ2M  )) :: ZQA_2M_MIN   ! air humidity (kg/kg)
REAL, DIMENSION(SIZE(D%XRHOA)) :: ZRHOA_2M     ! approximated density
REAL, DIMENSION(SIZE(D%XRHOA)) :: ZRHOA_2M_MIN ! approximated density
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MINZS_VERT_SHIFT',0,ZHOOK_HANDLE)
!
ZQA_2M = D%XQ2M / D%XRHOA
!
!*       1.  climatological gradient for temperature
!            ---------------------------------------
!
D%XT2M_MIN_ZS = D%XT2M + XCLIM_T_GRAD * (PZS_MIN - PZS_MOY)
!
!-------------------------------------------------------------------------------
!
!*       2.  hydrostatism for pressure
!            -------------------------
!
PPS_MIN = D%XPS * EXP ( - XG/XRD/(0.5*(D%XT2M+D%XT2M_MIN_ZS)*( 1.+((XRV/XRD)-1.)*ZQA_2M(:) )) &
                              * (PZS_MIN-PZS_MOY)                                              )  
!
!-------------------------------------------------------------------------------
!
!*       3.  conservation of relative humidity for humidity
!            ----------------------------------------------
!
ZQA_2M_MIN = ZQA_2M / QSAT(D%XT2M, D%XPS) * QSAT(D%XT2M_MIN_ZS,PPS_MIN)
!
!-------------------------------------------------------------------------------
!
!*       4.  estimation of air density from temperature and humidity
!            -------------------------------------------------------
!
ZRHOA_2M    (:) = D%XPS (:) / XRD /  D%XT2M       (:) / ( 1.+((XRV/XRD)-1.)*ZQA_2M(:) )
ZRHOA_2M_MIN(:) = PPS_MIN(:) / XRD /  D%XT2M_MIN_ZS(:) / ( 1.+((XRV/XRD)-1.)*ZQA_2M_MIN(:) )
!
PRHOA_2M_MIN(:) = D%XRHOA(:) * ZRHOA_2M_MIN(:) / ZRHOA_2M (:)
!
!-------------------------------------------------------------------------------
!
!*       5.  new humidity in kg/m3
!            ---------------------
!
D%XQ2M_MIN_ZS = ZQA_2M_MIN * PRHOA_2M_MIN
!
IF (LHOOK) CALL DR_HOOK('MINZS_VERT_SHIFT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MINZS_VERT_SHIFT
