!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CARBON_SPINUP(TPTIME, IO )
!  
!     #######################################################################
!
!
!!****  *CARBON_SPINUP*  
!!
!!    PURPOSE
!!    -------
!     Number of times the accelerated subroutine is called  
!     for each time step  
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      R. Alkama           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      03/26/2012
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!                              
USE MODI_SPINUP_MAX
!                              
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(DATE_TIME), INTENT(IN) :: TPTIME
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
!*      0.    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SPINUP',0,ZHOOK_HANDLE)
!
!       1.     Initializations
!              ---------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! number of times CARBON_SOIL subroutine is called for each time step
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
IO%NSPINS =1
IF ( IO%LSPINUPCARBS .AND. IO%CPHOTO/='NON' .AND. IO%CRESPSL=='CNT' ) THEN
   CALL SPINUP_MAX(IO%XSPINMAXS,IO%NNBYEARSPINS,IO%NNBYEARSOLD,IO%NSPINS)
ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! number of times  WOOD carbon subroutine is called for each time step
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IO%NSPINW=1
IF ( IO%LSPINUPCARBW .AND. IO%CPHOTO=='NCB' ) THEN
   CALL SPINUP_MAX(IO%XSPINMAXW,IO%NNBYEARSPINW,IO%NNBYEARSOLD,IO%NSPINW)
ENDIF
!
IF (TPTIME%TDATE%MONTH == 1 .AND. TPTIME%TDATE%DAY==1 .AND. TPTIME%TIME == 0.0 )THEN
   IO%NNBYEARSOLD = IO%NNBYEARSOLD + 1
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CARBON_SPINUP',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE 
