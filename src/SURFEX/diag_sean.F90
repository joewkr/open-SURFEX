!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_SEA_n (DLO, DL, DLC, SD, HSEA, HPROGRAM, DUP, DUPC, KMASK )
!     #####################################################################
!
!!****  *DIAG_SEA_n * - Chooses the surface schemes for sea diagnostics
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : new diag
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : SEAFLUX_DIAG_t
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
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DLO
TYPE(DIAG_t), INTENT(INOUT) :: DL
TYPE(DIAG_t), INTENT(INOUT) :: DLC
TYPE(SEAFLUX_DIAG_t), INTENT(INOUT) :: SD
!
 CHARACTER(LEN=*), INTENT(IN) :: HSEA
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
TYPE(DIAG_t), INTENT(INOUT) :: DUP
TYPE(DIAG_t), INTENT(INOUT) :: DUPC
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEA_N',0,ZHOOK_HANDLE)
IF (HSEA=='SEAFLX') THEN
  CALL DIAG_EVAP(SD%O, SD%D, SD%DC, HPROGRAM, DUP, DUPC, KMASK)
ELSEIF (HSEA=='FLUX') THEN
  CALL DIAG_EVAP(DLO, DL, DLC, HPROGRAM, DUP, DUPC, KMASK)            
ELSE IF (HSEA=='NONE  ') THEN
  CALL INIT_BUD(SD%O, DUP, DUPC, XUNDEF)
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_SEA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEA_n
