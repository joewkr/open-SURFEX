!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_PGD_SEA_n (DTCO, HSELECT, U, DTS, G, S, &
                                  HPROGRAM)
!     ####################################
!
!!****  *WRITE_PGD_SEA_n* - routine to write pgd surface variables in their respective files
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
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODI_WRITE_PGD_SEAFLUX_n
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
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
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_SEA_N',0,ZHOOK_HANDLE)
IF (U%CSEA=='SEAFLX') THEN
  CALL WRITE_PGD_SEAFLUX_n(DTCO, HSELECT, U, DTS, G, S, HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_SEA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_PGD_SEA_n
