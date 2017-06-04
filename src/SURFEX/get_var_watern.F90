!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_VAR_WATER_n (DFO, DF, DWO, DW, &
                                  HPROGRAM,KI,HWATER,PQS,PZ0,PZ0H)
!     ###########################################################
!
!!****  *GET_VAR_WATER_n* - routine to get variables defined only over water
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
!!      P. Le Moigne *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,       ONLY   : XUNDEF
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
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DFO
TYPE(DIAG_t), INTENT(INOUT) :: DF
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DWO
TYPE(DIAG_t), INTENT(INOUT) :: DW
!
 CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
 CHARACTER(LEN=6),     INTENT(IN)     :: HWATER
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQS     ! surface humidity
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! surface roughness length
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! surface roughness length for heat
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATER_N',0,ZHOOK_HANDLE)
IF (HWATER=='FLAKE') THEN
   CALL GET_VAR_FLAKE_n
ELSE
   CALL GET_VAR_WATFLX_n
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATER_N',1,ZHOOK_HANDLE)
CONTAINS
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
SUBROUTINE GET_VAR_WATFLX_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_WATFLX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DWO%LSURF_VARS) THEN 
        PQS      = DW%XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (DWO%LCOEF) THEN 
        PZ0      = DW%XZ0
        PZ0H     = DW%XZ0H
ELSE 
        PZ0      = XUNDEF
        PZ0H     = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_VAR_WATFLX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_VAR_WATFLX_n
!
!-------------------------------------------------------------------------------
!
SUBROUTINE GET_VAR_FLAKE_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('GET_VAR_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DFO%LSURF_VARS) THEN 
        PQS      = DF%XQS      
ELSE 
        PQS      = XUNDEF      
ENDIF           
IF (DFO%LCOEF) THEN 
        PZ0      = DF%XZ0
        PZ0H     = DF%XZ0H
ELSE 
        PZ0      = XUNDEF
        PZ0H     = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_VAR_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_VAR_FLAKE_n
!
!==============================================================================
!
END SUBROUTINE GET_VAR_WATER_n
