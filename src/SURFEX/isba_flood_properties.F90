!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_FLOOD_PROPERTIES(PLAI,PFFLOOD,PFFROZEN,PZ0FLOOD, &
                                       PFFG_NOSNOW,PFFV_NOSNOW         )  
!     ############################################################################
!
!
!!****  *ISBA_FLOOD_PROPERTIES*  
!!
!!    PURPOSE
!!    -------
!      Calculate the Flood fractions without snow and roughness length.  
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
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
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      25/05/08 
!!      Modified      09/2009  B. Decharme: limitation of Ri in surface_ri.F90
!!      Modified      02/2014  B. Decharme: Simplification
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SNOW_PAR,   ONLY : XZ0SN
USE MODD_FLOOD_PAR,  ONLY : XZ0FLOOD
!
USE MODE_SURF_FLOOD_FRAC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)       :: PLAI  ! leaf area index
REAL, DIMENSION(:), INTENT(IN)       :: PFFLOOD
REAL, DIMENSION(:), INTENT(IN)       :: PFFROZEN
!
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0FLOOD   ! roughness length over floodplains
REAL, DIMENSION(:), INTENT(OUT)      :: PFFG_NOSNOW
REAL, DIMENSION(:), INTENT(OUT)      :: PFFV_NOSNOW
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PFFLOOD)) :: ZPSNG
REAL, DIMENSION(SIZE(PFFLOOD)) :: ZPSNV
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       0.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLOOD_PROPERTIES',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!       1.     Flood fractions without snow
!              ----------------------------
!
ZPSNG(:)=0.0
ZPSNV(:)=0.0
!
PFFG_NOSNOW(:) = FLOOD_FRAC_GROUND(ZPSNG,PFFLOOD)
PFFV_NOSNOW(:) = FLOOD_FRAC_VEG(PLAI,ZPSNV,PFFLOOD)
!
!-------------------------------------------------------------------------------
!
!       2.     roughness length
!              ----------------
!
WHERE(PFFROZEN(:)==0.0)
    PZ0FLOOD(:) = XZ0FLOOD
ELSEWHERE
    PZ0FLOOD(:) = MIN(0.005,10.*XZ0FLOOD)
END WHERE
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLOOD_PROPERTIES',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_FLOOD_PROPERTIES
