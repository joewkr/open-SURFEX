!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_DIAG_FLAKE(K2M,OSURF_BUDGET,O2M_MIN_ZS,ORAD_BUDGET,OCOEF,OSURF_VARS,&
                                     OWATER_PROFILE,OSURF_BUDGETC,ORESET_BUDGETC,PDIAG_TSTEP,&
                                     PZWAT_PROFILE          )  
!     ########################################################################
!
!!****  *DEFAULT_DIAG_FLAKE* - routine to set default values for the choice of diagnostics
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
INTEGER,  INTENT(OUT) :: K2M           ! flag for operational 2m quantities
LOGICAL,  INTENT(OUT) :: OSURF_BUDGET  ! flag for surface budget
LOGICAL,  INTENT(OUT) :: O2M_MIN_ZS
LOGICAL,  INTENT(OUT) :: ORAD_BUDGET   ! flag for radiative budget
LOGICAL,  INTENT(OUT) :: OCOEF 
LOGICAL,  INTENT(OUT) :: OSURF_VARS
LOGICAL,  INTENT(OUT) :: OWATER_PROFILE ! flag for inline computation of water 
                                        !temperature at given depths according
                                        !to FLake model
LOGICAL,  INTENT(OUT) :: OSURF_BUDGETC ! flag for cumulated surface budget
LOGICAL,  INTENT(OUT) :: ORESET_BUDGETC! flag for cumulated surface budget
REAL,     INTENT(OUT) :: PDIAG_TSTEP   ! time-step for writing

REAL, DIMENSION(:), INTENT(OUT) :: PZWAT_PROFILE ! depths where to compute water
                                                ! temperatures according FLake
                                                ! model - variable readed in the
                                                ! namelist
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_FLAKE',0,ZHOOK_HANDLE)
!
K2M = 0
OSURF_BUDGET = .FALSE.
!
O2M_MIN_ZS = .FALSE. 
!
ORAD_BUDGET  = .FALSE.
!
OCOEF        = .FALSE.
OSURF_VARS   = .FALSE.
!
OWATER_PROFILE = .FALSE.
!
OSURF_BUDGETC= .FALSE.
ORESET_BUDGETC= .FALSE.
!
PDIAG_TSTEP  = XUNDEF
!
!ALLOCATE (PZWAT_PROFILE(100)) !maximum of 100 levels may be asked
PZWAT_PROFILE = XUNDEF
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_FLAKE',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_DIAG_FLAKE
