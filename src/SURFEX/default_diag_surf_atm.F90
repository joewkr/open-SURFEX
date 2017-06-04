!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_DIAG_SURF_ATM(K2M, OT2MMW, OSURF_BUDGET, O2M_MIN_ZS, ORAD_BUDGET, &
                                       OCOEF, OSURF_VARS, OSURF_BUDGETC, ORESET_BUDGETC, &
                                       OSELECT, OPROVAR_TO_DIAG, ODIAG_GRID, OFRAC, &
                                       PDIAG_TSTEP, OSNOWDIMNC, ORESETCUMUL, CSELECT  )                                         
!     ########################################################################
!
!!****  *DEFAULT_DIAG_SURF_ATM* - routine to set default values for the choice of diagnostics
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
!!      Modified    01/2006 : sea flux parameterization.
!!      B. Decharme   2008    flag for mean grid diag
!!      B. Decharme   2009    flag for cumulative budget and to write selected diags
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
LOGICAL,  INTENT(OUT) :: OT2MMW        ! flag for modified weighting of 2m temperature
LOGICAL,  INTENT(OUT) :: OSURF_BUDGET  ! flag for surface budget
LOGICAL,  INTENT(OUT) :: O2M_MIN_ZS    ! flag for 2m quantities on min.  orography
LOGICAL,  INTENT(OUT) :: ORAD_BUDGET   ! flag for radiative budget
LOGICAL,  INTENT(OUT) :: OCOEF         ! flag for transfer coefficients
LOGICAL,  INTENT(OUT) :: OSURF_VARS    ! flag for surface variables
LOGICAL,  INTENT(OUT) :: OSURF_BUDGETC ! flag for cumulated surface budget
LOGICAL,  INTENT(OUT) :: ORESET_BUDGETC  ! flag for cumulated surface budget
LOGICAL,  INTENT(OUT) :: OSELECT       ! switch to control which fields are written
LOGICAL,  INTENT(OUT) :: OPROVAR_TO_DIAG    ! switch to write (or not) prognostic variable
LOGICAL,  INTENT(OUT) :: OSNOWDIMNC    ! if true create a snow layer dimension in nc files
LOGICAL,  INTENT(OUT) :: ORESETCUMUL   ! if true reset cumulated variables at each output timestep
LOGICAL,  INTENT(OUT) :: ODIAG_GRID    ! flag for mean grid diag
LOGICAL,  INTENT(OUT) :: OFRAC         ! flag for fractions of tiles
REAL,     INTENT(OUT) :: PDIAG_TSTEP   ! time-step for writing
CHARACTER(LEN=12), DIMENSION(200), INTENT(OUT), OPTIONAL :: CSELECT  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_SURF_ATM',0,ZHOOK_HANDLE)
!
K2M          = 0
OT2MMW       = .FALSE.
OSURF_BUDGET = .FALSE.
!
O2M_MIN_ZS   = .FALSE.
!
ORAD_BUDGET  = .FALSE.
!
OCOEF        = .FALSE.
OSURF_VARS   = .FALSE.
!
OSURF_BUDGETC     = .FALSE.
ORESET_BUDGETC    = .FALSE.
!
OSELECT            = .FALSE.
!
OPROVAR_TO_DIAG    = .FALSE.
!
OSNOWDIMNC = .FALSE.
ORESETCUMUL = .FALSE.
!
ODIAG_GRID   = .TRUE.
!
OFRAC        = .FALSE.
!
PDIAG_TSTEP  = XUNDEF
!
IF (PRESENT(CSELECT)) CSELECT(:) = '            '
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_SURF_ATM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_DIAG_SURF_ATM
