!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_VAR_NATURE_n (S, DGO, D, DMI, &
                                   HPROGRAM,KI,PQS,PSNG,PSNV,PZ0EFF,PZ0,PZ0H,PTWSNOW,PBARE)
!     ######################################################################
!
!!****  *GET_VAR_NATURE_n* - routine to get variables defined only over nature
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
!       M. Jidane   08/2008 Z0 and Z0H recovery from nature tiles
!       S. Riette   06/2010 TWSNOW added
!       V. Masson   02/2015 adds LAI, height of trees, fraction of bare soil
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_S_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,    ONLY   : XUNDEF
USE MODD_DATA_COVER_PAR,   ONLY   : NVT_NO
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
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMI
!
 CHARACTER(LEN=6),     INTENT(IN)     :: HPROGRAM
INTEGER,              INTENT(IN)     :: KI      ! Number of points
REAL, DIMENSION(KI),  INTENT(OUT)    :: PQS     ! surface humidity
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSNG    ! snow fraction over ground
REAL, DIMENSION(KI),  INTENT(OUT)    :: PSNV    ! snow fraction over vegetation
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0EFF  ! effective roughness length (z0v+z0rel)
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0     ! surface roughness length
REAL, DIMENSION(KI),  INTENT(OUT)    :: PZ0H    ! surface roughness length for heat
REAL, DIMENSION(KI),  INTENT(OUT)    :: PTWSNOW ! Snow total reservoir
REAL, DIMENSION(KI),  INTENT(OUT)    :: PBARE   ! Bare soil fraction
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_VAR_NATURE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF (DGO%LSURF_VARS) THEN 
        PQS      = D%XQS      
   ELSE 
        PQS      = XUNDEF      
ENDIF           
!
IF (DMI%LSURF_MISC_BUDGET) THEN 
        PSNG     = DMI%XPSNG      
        PSNV     = DMI%XPSNV      
        PTWSNOW  = DMI%XTWSNOW
   ELSE 
        PSNG     = XUNDEF      
        PSNV     = XUNDEF      
        PTWSNOW  = XUNDEF
ENDIF           
!
IF (DGO%LCOEF) THEN
   PZ0EFF   = D%XZ0EFF
   PZ0      = D%XZ0      
   PZ0H     = D%XZ0H
ELSE
   PZ0EFF   = XUNDEF
   PZ0      = XUNDEF      
   PZ0H     = XUNDEF
ENDIF
!
PBARE = S%XVEGTYPE(:,NVT_NO)
!
IF (LHOOK) CALL DR_HOOK('GET_VAR_NATURE_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_VAR_NATURE_n
