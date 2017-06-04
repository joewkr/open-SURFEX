!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_NATURE_n (DLO, DL, DLC, ID, HNATURE, &
                          HPROGRAM, DUP, DUPC, KMASK )
!     ###############################################################################
!
!!****  *DIAG_NATURE_n * - Chooses the surface schemes for diagnostics over
!!    natural continental parts
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
!       P. Le Moigne 03/2015 : add diagnostics IDEAL case
!!------------------------------------------------------------------
!
USE MODE_DIAG
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : ISBA_DIAG_t
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
TYPE(ISBA_DIAG_t), INTENT(INOUT) :: ID
!
 CHARACTER(LEN=*), INTENT(IN) :: HNATURE
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
TYPE(DIAG_t), INTENT(INOUT) :: DUP
TYPE(DIAG_t), INTENT(INOUT) :: DUPC
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
!*      0.2    declarations of local variables
!
INTEGER :: ISIZE, JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_NATURE_N',0,ZHOOK_HANDLE)
IF (HNATURE=='ISBA  ' .OR. HNATURE=='TSZ0  ' ) THEN
  !  
  CALL DIAG_CUMUL(ID%O, ID%D, ID%DC, HPROGRAM, DUP, DUPC, KMASK)
  !
  ISIZE = SIZE(KMASK)
  !
  IF (ID%O%LSURF_BUDGET.AND.ID%DE%LSURF_EVAP_BUDGET) THEN
    DO JJ=1,ISIZE        
      DUP%XEVAP    (KMASK(JJ))  = ID%D%XEVAP     (JJ)
      DUP%XSUBL    (KMASK(JJ))  = ID%D%XSUBL     (JJ)
    ENDDO
  ENDIF
  !                   
ELSE IF (HNATURE=='FLUX  ') THEN
  CALL DIAG_EVAP(DLO, DL, DLC, HPROGRAM, DUP, DUPC, KMASK)
ELSE IF (HNATURE=='NONE  ') THEN
  CALL INIT_BUD(ID%O, DUP, DUPC, XUNDEF)
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_NATURE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_NATURE_n
