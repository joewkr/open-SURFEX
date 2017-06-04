!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_GRID_CONF_ISBA_n (IG, &
                                       PLONMIN,PLONMAX,PLATMIN,PLATMAX,KX,KY,KL)
!     #########################################
!
!!****  *GET_GRID_CONF_ISBA_n* - routine to get the ISBA grid configuration
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODE_GRIDTYPE_LONLAT_REG
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
TYPE(GRID_t), INTENT(INOUT) :: IG
!
REAL,               INTENT(OUT) :: PLONMIN
REAL,               INTENT(OUT) :: PLONMAX
REAL,               INTENT(OUT) :: PLATMIN
REAL,               INTENT(OUT) :: PLATMAX
INTEGER,            INTENT(OUT) :: KX
INTEGER,            INTENT(OUT) :: KY
INTEGER,            INTENT(OUT) :: KL
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_GRID_CONF_ISBA_N',0,ZHOOK_HANDLE)
SELECT CASE (IG%CGRID)
!     
!  CASE("CONF PROJ ")
!
!  CASE("CARTESIAN ")

  CASE("LONLAT REG")
    CALL GET_GRIDTYPE_LONLAT_REG(IG%XGRID_PAR,PLONMIN,PLONMAX,PLATMIN,PLATMAX,KX,KY,KL)
!
!  CASE("GAUSS     ")
!
!  CASE("NONE      ", "IGN       ")

END SELECT
IF (LHOOK) CALL DR_HOOK('GET_GRID_CONF_ISBA_N',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_GRID_CONF_ISBA_n
