!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_Z0_n (DGO, D, HPROGRAM,KI,PZ0,PZ0H)
!     #########################################
!
!!****  *GET_Z0_n* - routine to get roughness lengths
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
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,        ONLY   : XUNDEF
!
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
TYPE(DIAG_OPTIONS_t), INTENT(IN) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
!
 CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! roughness length for momentum (m)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! roughness length for heat     (m)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_Z0_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGO%LCOEF)      THEN 
        PZ0      = D%XZ0      
        PZ0H     = D%XZ0H
   ELSE 
        PZ0      = XUNDEF      
        PZ0H     = XUNDEF
ENDIF           
IF (LHOOK) CALL DR_HOOK('GET_Z0_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_Z0_n
