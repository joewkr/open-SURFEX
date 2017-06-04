!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_INTERP_HALO(HPROGRAM,HGRID,KHALO)
!     #######################################################
!
!
!!****  *GET_INTERP_HALO* - gets the value of the number of points in the halo 
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef SFX_OL
USE MODI_GET_INTERP_HALO_OL
#endif
#ifdef SFX_MNH
USE MODI_GET_INTERP_HALO_MNH
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),      INTENT(IN) :: HPROGRAM ! program calling SURFEX
 CHARACTER(LEN=10),     INTENT(IN) :: HGRID    ! grid type
INTEGER,               INTENT(OUT):: KHALO
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_INTERP_HALO',0,ZHOOK_HANDLE)
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  CALL GET_INTERP_HALO_MNH(KHALO)
#endif
ELSE
#ifdef SFX_OL
  ! to be coded properly once Offline version is parallelized
  CALL GET_INTERP_HALO_OL(HGRID,KHALO)
#endif
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_INTERP_HALO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_INTERP_HALO
