!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      MODULE MODE_HYDRO_DIF 
!     ################
!
!!****  *MODE_HYDRO_DIF * - pedo-transfert functions
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
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
!!      B. Decharme       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/2010
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
INTERFACE VAPCONDCF
  MODULE PROCEDURE VAPCONDCF       
END INTERFACE
!
INTERFACE INFMAX_FUNC
  MODULE PROCEDURE INFMAX_FUNC
END INTERFACE
!
INTERFACE TRIDIAG_DIF
  MODULE PROCEDURE TRIDIAG_DIF
END INTERFACE
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!vapor conductivity (m s-1)
!-------------------------------------------------------------------------------
!
FUNCTION VAPCONDCF(PTG,PPS,PWG,PWGI,PPSIA,PWSAT,PWFC,PQSAT,PQSATI,KWG_LAYER,KNL) RESULT(PVAPCOND)
!
! Uses method of Braud et al. (1993) for
!
USE MODD_CSTS,       ONLY : XMV, XMD, XTT, XP00, XG, XRV, XRHOLW
USE MODD_ISBA_PAR,   ONLY : XWGMIN
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:  ), INTENT(IN)         :: PPS
REAL, DIMENSION(:,:), INTENT(IN)         :: PWG,PWGI,PPSIA,PWSAT, &
                                            PWFC,PTG,PQSAT,PQSATI
INTEGER, DIMENSION(:), INTENT(IN)        :: KWG_LAYER       !Moisture layer
INTEGER,               INTENT(IN)        :: KNL             ! number of vertical levels
!
REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: PVAPCOND
!
!
!*      0.2    declarations of local variables
!
REAL    :: ZDVA, ZFVA, ZCHI, ZHUM, ZWORK,  &
           ZPV, ZESAT, ZESATI, ZWG, ZVC
!
INTEGER :: INI, JJ, JL, IDEPTH
!
! Parameters:
!
REAL, PARAMETER                     :: ZTORTY = 0.66         ! (-)
REAL, PARAMETER                     :: ZNV    = 1.88         ! (-)
REAL, PARAMETER                     :: ZCV    = 2.17e-5      ! (m2/s)
REAL, PARAMETER                     :: ZWK    = 0.05         ! (m3 m-3)
REAL, PARAMETER                     :: ZLIM   = TINY(1.0)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:VAPCONDCF',0,ZHOOK_HANDLE)
!
INI = SIZE(PWG,1)
!
PVAPCOND(:,:) = 0.0
!
! Only perform this computation if the soil is sufficiently
! dry (as otherwise the hydraulic conductivity dominates
! the diffusion coefficient). Arbitrarily base threshold on field
! capacity water content:
!
DO JL=1,KNL
   DO JJ=1,INI
!
      IDEPTH = KWG_LAYER(JJ)
      ZWG    = PWG (JJ,JL) + PWGI(JJ,JL)
!      
      IF(JL<=IDEPTH .AND. ZWG < PWFC(JJ,JL) .AND. ZWG > XWGMIN)THEN
!
!        Vapor pressure over liquid and solid water surfaces (Pa), respectively:
!
         ZESAT  = PQSAT(JJ,JL)* PPS(JJ)/((XMV/XMD)+PQSAT(JJ,JL) *(1.-(XMV/XMD)))
!
         ZESATI = PQSATI(JJ,JL)*PPS(JJ)/((XMV/XMD)+PQSATI(JJ,JL)*(1.-(XMV/XMD)))
!
!        molecular diffusivity of water vapor (m2 s-1):
!
         ZWORK  = ZNV*LOG(PTG(JJ,JL)/XTT)
         ZDVA   = ZCV*(XP00/PPS(JJ))*EXP(ZWORK)
!
!        function of pore space: 
!
         ZFVA   = (PWSAT(JJ,JL) - ZWG)*(1.+(ZWG/(PWSAT(JJ,JL)-ZWK)))
         ZFVA   = MIN(ZFVA,PWSAT(JJ,JL))
!
!        relative humidity of air in soil pores:
!
         ZHUM   = MAX(ZLIM,EXP(PPSIA(JJ,JL)*XG/(XRV*PTG(JJ,JL))))
!
!        fraction of frozen water:
!
         ZCHI   = PWGI(JJ,JL)/ZWG
!
!        vapor pressure within pore space (Pa):
!
         ZPV    = ZHUM*(ZCHI*ZESAT + (1.-ZCHI)*ZESATI)
!
!        vapor conductivity (kg m-2 s-1)
!
         ZVC    = ZTORTY*PPS(JJ)*ZDVA*ZFVA*XG*ZPV/                  &
                  ((PPS(JJ)-ZPV)*(XRV*XRV*PTG(JJ,JL)*PTG(JJ,JL)))
!
!        vapor conductivity (m s-1)
!
         PVAPCOND(JJ,JL) = ZVC/XRHOLW
!
      ENDIF
!
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:VAPCONDCF',1,ZHOOK_HANDLE)
!
END FUNCTION VAPCONDCF
!
!-------------------------------------------------------------------------------
!Green-Ampt approximation (derived form) for maximum infiltration
!-------------------------------------------------------------------------------
!
FUNCTION INFMAX_FUNC(PWG,PWSAT,PFRZ,PCONDSAT,PMPOTSAT,PBCOEF,PDZG,PDG,KLAYER_HORT)
USE YOMHOOK      ,ONLY : LHOOK,   DR_HOOK
USE MODD_SGH_PAR, ONLY : XHORT_DEPTH
USE PARKIND1     ,ONLY : JPRB
IMPLICIT NONE
REAL, DIMENSION(:,:), INTENT(IN) :: PWG
REAL, DIMENSION(:,:), INTENT(IN) :: PWSAT           
REAL, DIMENSION(:,:), INTENT(IN) :: PFRZ
REAL, DIMENSION(:,:), INTENT(IN) :: PCONDSAT            
REAL, DIMENSION(:,:), INTENT(IN) :: PMPOTSAT    
REAL, DIMENSION(:,:), INTENT(IN) :: PBCOEF 
REAL, DIMENSION(:,:), INTENT(IN) :: PDZG
REAL, DIMENSION(:,:), INTENT(IN) :: PDG
INTEGER,              INTENT(IN) :: KLAYER_HORT   
!
REAL, DIMENSION(SIZE(PWG,1)) :: ZGREEN_AMPT, ZDEPTH
REAL                         :: ZS, ZCOEF
INTEGER                      :: JJ,JL,INI
!
REAL, DIMENSION(SIZE(PWG,1)) :: INFMAX_FUNC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:INFMAX_FUNC',0,ZHOOK_HANDLE)
!
INI   =SIZE(PWG,1)
!
ZGREEN_AMPT(:) = 0.0
ZDEPTH     (:) = 0.0
!
DO JL=1,KLAYER_HORT
   DO JJ=1,INI  
      IF(ZDEPTH(JJ)<XHORT_DEPTH)THEN
         ZS              = MIN(1.0,PWG(JJ,JL)/PWSAT(JJ,JL))
         ZCOEF           = PBCOEF(JJ,JL)*PMPOTSAT(JJ,JL)*(ZS-1.0)/PDZG(JJ,JL)        
         ZGREEN_AMPT(JJ) = ZGREEN_AMPT(JJ)+PDZG(JJ,JL)*PFRZ(JJ,JL)*PCONDSAT(JJ,JL)*(ZCOEF+1.0)
         ZDEPTH     (JJ) = PDG(JJ,JL)
      ENDIF      
   ENDDO
ENDDO
!
INFMAX_FUNC(:) = ZGREEN_AMPT(:)/ZDEPTH(:)
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:INFMAX_FUNC',1,ZHOOK_HANDLE)
END FUNCTION INFMAX_FUNC
!
!-------------------------------------------------------------------------------
!Solve tridiagonal matrix (for method see tridiag_ground.F90)
!-------------------------------------------------------------------------------
!
SUBROUTINE TRIDIAG_DIF(PAMTRX,PBMTRX,PCMTRX,PFRC,KWG_LAYER,KNL,PSOL)
USE MODD_SURF_PAR, ONLY : XUNDEF
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
IMPLICIT NONE
REAL,    DIMENSION(:,:), INTENT(IN)  :: PAMTRX    ! lower diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PBMTRX    ! main  diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PCMTRX    ! upper diag. elements of A matrix
REAL,    DIMENSION(:,:), INTENT(IN)  :: PFRC      ! Forcing term
INTEGER, DIMENSION(:),   INTENT(IN)  :: KWG_LAYER !Moisture layer
INTEGER,                 INTENT(IN)  :: KNL       ! number of vertical levels
REAL,    DIMENSION(:,:), INTENT(OUT) :: PSOL      ! solution of A.SOL = FRC
!
REAL, DIMENSION(SIZE(PFRC,1),SIZE(PFRC,2)) :: ZWORK! work array
REAL, DIMENSION(SIZE(PFRC,1))              :: ZDET ! work array
INTEGER                                    :: JL   ! vertical loop control
INTEGER :: JJ, INI, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:TRIDIAG_DIF',0,ZHOOK_HANDLE)
!
INI=SIZE(PFRC,1)
!
PSOL (:,:)=XUNDEF
ZWORK(:,:)=XUNDEF
!
!first level
ZDET(:)   = PBMTRX(:,1)
PSOL(:,1) = PFRC(:,1) / ZDET(:)
!
!other levels
DO JL=2,KNL
   DO JJ=1,INI
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
        ZWORK(JJ,JL) = PCMTRX(JJ,JL-1)/ZDET(JJ)
        ZDET (JJ)    = PBMTRX(JJ,JL) - PAMTRX(JJ,JL)*ZWORK(JJ,JL)
        PSOL (JJ,JL) = (PFRC (JJ,JL) - PAMTRX(JJ,JL)*PSOL(JJ,JL-1))/ZDET(JJ)  
      ENDIF
   ENDDO 
ENDDO        
!
!levels going down
DO JL=KNL-1,1,-1
   DO JJ=1,INI
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<IDEPTH)THEN
         PSOL(JJ,JL) = PSOL(JJ,JL)-ZWORK(JJ,JL+1)*PSOL(JJ,JL+1)
      ENDIF
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_HYDRO_DIF:TRIDIAG_DIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIDIAG_DIF
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_HYDRO_DIF
