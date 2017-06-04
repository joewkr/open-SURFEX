!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_INLAND_WATER_n (DLO, DL, DLC, FM, WM, HWATER, &
                                HPROGRAM, DUP, DUPC, KMASK       )
!     ###############################################################################
!
!!****  *DIAG_INLAND_WATER_n * - Chooses the surface schemes for lakes diagnostics
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
!!      Original    01/2004
!!      Modified    08/2009 : cumulated diag & t2m min/max
!!       V.Masson   10/2013 Adds min and max 2m parameters
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t, WATFLUX_MODEL_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DLO
TYPE(DIAG_t), INTENT(INOUT) :: DL
TYPE(DIAG_t), INTENT(INOUT) :: DLC
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
!
 CHARACTER(LEN=*), INTENT(IN) :: HWATER
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
TYPE(DIAG_t), INTENT(INOUT) :: DUP
TYPE(DIAG_t), INTENT(INOUT) :: DUPC
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLAND_WATER_N',0,ZHOOK_HANDLE)
IF (HWATER=='WATFLX') THEN
  CALL DIAG_EVAP(WM%DWO, WM%DW, WM%DWC, HPROGRAM, DUP, DUPC, KMASK)
ELSE IF (HWATER=='FLAKE ') THEN
  CALL DIAG_EVAP(FM%DFO, FM%DF, FM%DFC, HPROGRAM, DUP, DUPC, KMASK)       
ELSE IF (HWATER=='FLUX  ') THEN
  CALL DIAG_EVAP(DLO, DL, DLC, HPROGRAM, DUP, DUPC, KMASK)                     
ELSE IF (HWATER=='NONE  ') THEN
  CALL INIT_BUD(WM%DWO,DUP,DUPC,XUNDEF)         
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLAND_WATER_n
