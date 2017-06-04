!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################################
      SUBROUTINE CANOPY_GRID(KI,SB)
!     #########################################
!
!!****  *CANOPY_GRID* - computation of vertical grid coordinatesa at 
!!                      half levels and grid depths at half and full
!!                      levels
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!
!
!  --------------------------------- PZ(k+1)                     PDZ(k+1)
!                                                                           ^
!                                                                           |
!                                                                           |
!  - - - - - - - - - - - - - - - - - PZf(k+1)                               | PDZf(k+1)
!                                                              ^            |
!                                                              |            |
!  --------------------------------- PZ(k), XU, XT, XQ, XTKE   | PDZ(k)     V
!                                                              |            ^
!  - - - - - - - - - - - - - - - - - PZf(k)                    V            | PDZf(k)
!  --------------------------------- PZ(k-1)                     PDZ(k-1)   V
!  - - - - - - - - - - - - - - - - - PZf(k-1)
!

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
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI     ! number of horizontal points
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JLAYER                 ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CANOPY_GRID',0,ZHOOK_HANDLE)
!
!*    1. Geometric computations
!        ----------------------
!
!
!*    1.1 layer depths (variable located at half levels below full levels)
!         ------------
!
SB%XDZF(:,:) = -999.
SB%XDZF(:,1) = 2.*SB%XZ(:,1)
DO JLAYER=2,SB%NLVL
  SB%XDZF(:,JLAYER) = SB%XZ(:,JLAYER) - SB%XZ(:,JLAYER-1)
END DO
!
!*    1.2 Layer heights (variable located at half levels below full levels)
!         -------------
!
SB%XZF(:,:) = -999.
SB%XZF(:,1) = 0.
DO JLAYER=2,SB%NLVL
  SB%XZF(:,JLAYER) = 2.*SB%XZ(:,JLAYER-1) - SB%XZF(:,JLAYER-1)
END DO
!
!
!*    1.3 layer depths (variable located at full levels)
!         ------------
!
SB%XDZ(:,:) = -999.
DO JLAYER=1,SB%NLVL-1
  SB%XDZ(:,JLAYER) = SB%XZF(:,JLAYER+1) - SB%XZF(:,JLAYER)
END DO
!
SB%XDZ(:,SB%NLVL) = 2.*(SB%XZ(:,SB%NLVL)-SB%XZF(:,SB%NLVL))
!
IF (LHOOK) CALL DR_HOOK('CANOPY_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE CANOPY_GRID
