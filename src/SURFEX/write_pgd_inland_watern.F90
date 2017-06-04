!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_PGD_INLAND_WATER_n (DTCO, HSELECT, U, WG, W, FG, F, &
                                           HPROGRAM)
!     ####################################
!
!!****  *WRITE_PGD_INLAND_WATER_n* - routine to write pgd surface variables in their respective files
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2011
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
!
USE MODI_WRITE_PGD_WATFLUX_n
USE MODI_WRITE_PGD_FLAKE_n
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                             ! 'ALL' : all fields are written
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       1.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_INLAND_WATER_N',0,ZHOOK_HANDLE)
IF (U%CWATER=='WATFLX') THEN
  CALL WRITE_PGD_WATFLUX_n(DTCO, HSELECT, U, WG, W, HPROGRAM)
ELSE IF (U%CWATER=='FLAKE ') THEN
  CALL WRITE_PGD_FLAKE_n(DTCO, HSELECT, U, FG, F, HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_PGD_INLAND_WATER_n
