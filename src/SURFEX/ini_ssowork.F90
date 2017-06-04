!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_SSOWORK(PMESHLENGTH,PDLAT,PDLON)
!     ###############################################
!
!!**** *INI_SSOWORK* initializes and allocate work arrays for SSO reading
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PGDWORK,  ONLY : NSSO, NSSO_ALL, XSSO_ALL
USE MODD_SURF_PAR, ONLY : NUNDEF, XUNDEF
USE MODD_SURFEX_MPI, ONLY : NINDEX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!----------------------------------------------------------------------------
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, OPTIONAL, INTENT(IN) :: PMESHLENGTH ! average mesh length in degrees
REAL, OPTIONAL, INTENT(IN) :: PDLAT       ! input file mesh size (in latitude,  degrees)
REAL, OPTIONAL, INTENT(IN) :: PDLON       ! input file mesh size (in longitude, degrees)

INTEGER :: IDIMF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!
!*    1.     Adapt subgrid mesh to input file resolution
!            -------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_SSOWORK',0,ZHOOK_HANDLE)
!
IDIMF = SIZE(NINDEX)
!
IF (PRESENT(PMESHLENGTH) .AND. PRESENT(PDLAT) .AND. PRESENT(PDLON)) THEN
  IF (PDLAT/= XUNDEF .AND. PDLON /= XUNDEF) THEN
    NSSO = NINT( 2. * PMESHLENGTH / (PDLAT + PDLON) )
    NSSO = MAX(NSSO,3)
    NSSO = MIN(NSSO,10)
  ELSE
    NSSO = 10
  END IF
ELSE
  NSSO = 10
END IF
!
!----------------------------------------------------------------------------
!
!*    2.     Allocate subgrid arrays
!            -----------------------
!
IF (ALLOCATED(XSSO_ALL)) DEALLOCATE(XSSO_ALL)
!
ALLOCATE(XSSO_ALL(IDIMF,NSSO,NSSO))
XSSO_ALL(:,:,:) = -XUNDEF
!
IF (ALLOCATED(NSSO_ALL)) DEALLOCATE(NSSO_ALL)
!
ALLOCATE(NSSO_ALL(IDIMF,NSSO,NSSO))
NSSO_ALL(:,:,:) = 0
!
IF (LHOOK) CALL DR_HOOK('INI_SSOWORK',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE INI_SSOWORK
