!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE  DIAG_SURF_BUDGET_SEA(D, DI, S, PTT, PRHOA, PSFTH, PSFTH_ICE, &
                                 PSFTQ, PSFTQ_ICE, PDIR_SW, PSCA_SW, PLW,    &
                                 PDIR_ALB, PSCA_ALB, PEMIS, PTRAD,           &
                                 PSFZON, PSFZON_ICE, PSFMER, PSFMER_ICE   ) 


!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_WATER * - Computes diagnostics over water
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!       S.Senesi    01/2014 : Handle fluxes on seaice
!!------------------------------------------------------------------
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE MODD_CSTS,           ONLY : XSTEFAN, XLSTT, XLVTT
USE MODD_WATER_PAR,      ONLY : XEMISWATICE, XALBSEAICE
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DI
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL,               INTENT(IN) :: PTT       ! freezing temperature of water surface
REAL, DIMENSION(:), INTENT(IN) :: PRHOA     ! air density
REAL, DIMENSION(:), INTENT(IN) :: PSFTH     ! heat flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux on seaice
!
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux on seaice
!
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
!
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD     ! radiative temperature                 (K)
!
REAL, DIMENSION(:), INTENT(IN) :: PSFZON    ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFZON_ICE! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER    ! meridional friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER_ICE! meridional friction
!
!*      0.2    declarations of local variables
!
INTEGER                      :: I
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',0,ZHOOK_HANDLE)
!
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  D%XSWBD(:,JSWB) = PDIR_SW(:,JSWB)                    + PSCA_SW(:,JSWB)
  D%XSWBU(:,JSWB) = PDIR_SW(:,JSWB) * PDIR_ALB(:,JSWB) + PSCA_SW(:,JSWB) * PSCA_ALB(:,JSWB) 
ENDDO
!
D%XSWD(:) = 0.
D%XSWU(:) = 0.
DO JSWB=1,ISWB
   D%XSWD(:) = D%XSWD(:) + D%XSWBD(:,JSWB)
   D%XSWU(:) = D%XSWU(:) + D%XSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
D%XLWD(:)=PLW(:)
D%XLWU(:)=PEMIS(:)*XSTEFAN*PTRAD(:)**4 + (1.-PEMIS(:))*PLW(:)
!
!* net radiation
!
D%XRN(:)    =   D%XSWD(:) - D%XSWU(:)     + D%XLWD(:) - D%XLWU    (:)
!
IF (.NOT.S%LHANDLE_SIC) THEN
  !
  !* sensible heat flux
  !
  D%XH     = PSFTH
  !
  !* latent heat flux
  !
  WHERE (S%XSST<PTT  )
     D%XLE    = PSFTQ * XLSTT
     D%XLEI   = PSFTQ * XLSTT
     D%XEVAP  = PSFTQ
     D%XSUBL  = PSFTQ
  ELSEWHERE
     D%XLE    = PSFTQ * XLVTT
     D%XLEI   = 0.0
     D%XEVAP  = PSFTQ
     D%XSUBL  = 0.0
  END WHERE
  !
  !* wind stress
  !
  D%XFMU = PSFZON
  D%XFMV = PSFMER
  !
ELSE
  !
  !---------------------------------------------------------------------------- 
  ! Sea ice or mixed diag
  !---------------------------------------------------------------------------- 
  !
  !
  !* total incoming and outgoing SW
  !
  DO JSWB=1,ISWB
    DI%XSWBU(:,JSWB) = (PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB)) * S%XICE_ALB(:) 
  ENDDO
  !
  DI%XSWU(:) = 0.
  DO JSWB=1,ISWB
     DI%XSWU(:) = DI%XSWU(:) + DI%XSWBU(:,JSWB)
  ENDDO
  !
  !*incoming outgoing LW
  !
  DI%XLWU(:)=XEMISWATICE*XSTEFAN*S%XTICE(:)**4 + (1.-XEMISWATICE)*PLW(:)
  !
  !* net radiation
  !
  DI%XRN(:) =   D%XSWD(:) - DI%XSWU(:) + D%XLWD(:) - DI%XLWU(:)
  !
  !* sensible heat flux
  !
  D%XH     = (1 - S%XSIC) * PSFTH + S%XSIC * PSFTH_ICE 
  DI%XH    =                                 PSFTH_ICE
  !
  !* latent heat flux
  !
  D%XLE     = (1 - S%XSIC) * PSFTQ * XLVTT + S%XSIC * PSFTQ_ICE * XLSTT
  D%XLEI    =                                         PSFTQ_ICE * XLSTT
  D%XEVAP   = (1 - S%XSIC) * PSFTQ         + S%XSIC * PSFTQ_ICE 
  D%XSUBL   =                                S%XSIC * PSFTQ_ICE 
  !
  !* ice storage flux
  !
  DI%XGFLUX = DI%XRN - DI%XH - D%XLEI
  !
  !* wind stress
  !
  D%XFMU  = (1 - S%XSIC) * PSFZON + S%XSIC * PSFZON_ICE
  DI%XFMU =                                  PSFZON_ICE
  D%XFMV  = (1 - S%XSIC) * PSFMER + S%XSIC * PSFMER_ICE
  DI%XFMV =                                  PSFMER_ICE
!  
ENDIF
!
!* total storage flux
!
D%XGFLUX = D%XRN - D%XH - D%XLE
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_SEA
