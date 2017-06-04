!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE SET_SSO_LEVELS (SB, KDIM)
!     #################################################################################
!
!!****  *SET_SSO_LEVELS* - prepares SSO canopy fields
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
!!      Original    07/2006
!!      S. Riette   06/2009 XT, XU, XQ, XTKE are set to XUNDEF
!!                          No more argument needed
!!      E. Martin   01/2012 XUNDEF fields are no more written in PREP file
!!------------------------------------------------------------------
!
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_PREP_SBL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
INTEGER, INTENT(IN) :: KDIM ! 1D physical dimension

!
!*      0.2    declarations of local variables
!
INTEGER :: JLAYER
INTEGER :: ILU      ! number of points
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.    number of levels (MUST be at least equal to 2)
!             ----------------
!
IF (LHOOK) CALL DR_HOOK('SET_SSO_LEVELS',0,ZHOOK_HANDLE)
!
 CALL PREP_SBL(KDIM, SB)
!
!*      3.    wind in canopy (m/s)
!             --------------
!
ALLOCATE(SB%XU(KDIM,SB%NLVL))
SB%XU(:,:) = XUNDEF
!
!*      4.    Tke in canopy (m2/s2)
!             -------------
!
ALLOCATE(SB%XTKE(KDIM,SB%NLVL))
SB%XTKE(:,:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('SET_SSO_LEVELS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SET_SSO_LEVELS
