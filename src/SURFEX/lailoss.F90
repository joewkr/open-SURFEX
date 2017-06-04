!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LAILOSS(PK, PEK, PBIOMASS)  
!   ###############################################################
!!****  *LAILOSS*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the time change in LAI due to senesence 
!     and cutting: ie losses/decreases to LAI. This in turn
!     reduces the dry biomass of the canopy.
!              
!!**  METHOD
!!    ------
!     Calvet at al (1997) [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. (1997)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!      Modified    12/03/04  by P LeMoigne: ZXSEFOLD in days
!!      L. Jarlan   27/10/04  add RHOA as input to express IP%XANMAX(:,1) in
!!                            kgCO2 m-2s-1 instead of kgCO2 kgAir-1 m s-1
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S. Lafont   03/2011 modification for consistency with nitro_decline
!!
!-------------------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_CSTS,  ONLY : XDAY
USE MODD_CO2V_PAR, ONLY: XMC, XMCO2, XPCCO2
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PBIOMASS ! total dry canopy biomass 
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PEK%XSEFOLD,1))  :: ZXSEFOLD, ZXM
REAL                               :: ZBMCOEF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LAILOSS',0,ZHOOK_HANDLE)
!
ZBMCOEF     = XMC/(XMCO2*XPCCO2)
!
! Once a day (at midnight), adjust biomass:
! ----------------------------------------
!
WHERE((PEK%XVEG(:)>0) )
  !
  ! leaf life expectancy
  !
  ZXSEFOLD(:) = PEK%XSEFOLD(:)*MIN(1.0, PEK%XANFM(:)/PK%XANMAX(:))/XDAY
  !
  ! avoid possible but unlikely division by zero
  !
  ZXSEFOLD(:) = MAX(1.0E-8,ZXSEFOLD(:))
  !
  ! limitation of leaf life expectancy
  !
  ZXSEFOLD(:) = MAX(5.,ZXSEFOLD(:))
  !
  ! senesence of active biomass
  !
  ZXM(:)      = PBIOMASS(:)*(1.0-EXP(-1.0/ZXSEFOLD(:)))
  !
  ! decrease biomass:
  !
  PBIOMASS(:) = PBIOMASS(:) - ZXM(:)
  !
  ! same modification than nitro_decline.f90
  ! now the assimilation is added here
  ! in that way laigain.f90 is consistant between the different carbon options.
  PBIOMASS(:) =  PBIOMASS(:) + PEK%XANDAY(:)*ZBMCOEF
  !
  ! maximum leaf assimilation (kgCO2 kgAir-1 m s-1):
  !
  PEK%XANFM(:)    = 0.0
  !
END WHERE
!
IF (LHOOK) CALL DR_HOOK('LAILOSS',1,ZHOOK_HANDLE)
!
END SUBROUTINE LAILOSS
