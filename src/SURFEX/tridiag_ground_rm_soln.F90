!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE TRIDIAG_GROUND_RM_SOLN(PSOLN,PA_COEF,PB_COEF)
!
!
!!****  *TRIDIAG_GROUND_RM_SOLN*  
!!
!!    PURPOSE
!!    -------
!
!     Back substitution ("downward sweep") for solution of a tri-diagnoal matrix using
!     the method of Richtmeyer and Morton (1967), given coefficients A and B.
!
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!
!     Richtmeyer, R. and K. Morton, 1967: Difference method for initial values problems,
!     Interscience Publishers, 2.
!
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/03/11 
!!      Modif       23/02/12 A. Boone: Optimization
!!      Modif       03/2013  A. Boone: MEB
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PA_COEF     ! RM67 A-soil coefficient           (-)
REAL, DIMENSION(:,:), INTENT(IN)    :: PB_COEF     ! RM67 B-soil coefficient           (K)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSOLN       ! solution vector                   
!                                                  ! of the input variable             (*)
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_RM_SOLN',0,ZHOOK_HANDLE)
!
! Get  the solution vector.
! NOTE: surface value obtained in energy budget routine, so
! this is the *sub-surface* profile solution.
!      
DO JJ=2,SIZE(PSOLN,2)
   DO JI=1,SIZE(PSOLN,1)
      PSOLN(JI,JJ) = PA_COEF(JI,JJ)*PSOLN(JI,JJ-1) + PB_COEF(JI,JJ)
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG_GROUND_RM_SOLN',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
END SUBROUTINE TRIDIAG_GROUND_RM_SOLN
