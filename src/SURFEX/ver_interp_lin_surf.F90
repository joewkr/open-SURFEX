!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODI_VER_INTERP_LIN_SURF
!     ######################
INTERFACE VER_INTERP_LIN_SURF
      MODULE PROCEDURE VER_INTERP_LIN3D_SURF
      MODULE PROCEDURE VER_INTERP_LIN2D_SURF
      MODULE PROCEDURE VER_INTERP_LIN1D_SURF
!
END INTERFACE
CONTAINS
!     ##############################################
      FUNCTION VER_INTERP_LIN3D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!     This function interpolates the 3D fields from one grid
!     to another using linear interpolation cofficients stored in module
!     MODD_VER_INTERP_LIN.
!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Book 2
!!
!!    AUTHOR
!!    ------
!!
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/07/97
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2),SIZE(KKLIN,3))                   &
                                       :: PVAR2 ! variable values on target
!                                             ! grid
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER                                               :: JI,JJ,JK2
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN3D_SURF',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JK2,JJ,JI)
DO JK2=1,SIZE(KKLIN,3)
  DO JJ=1,SIZE(KKLIN,2)
    DO JI=1,SIZE(KKLIN,1)
      PVAR2(JI,JJ,JK2)=    PCOEFLIN(JI,JJ,JK2) *PVAR1(JI,JJ,KKLIN(JI,JJ,JK2)  )&
                        +(1.-PCOEFLIN(JI,JJ,JK2))*PVAR1(JI,JJ,KKLIN(JI,JJ,JK2)+1)
    END DO
  END DO
END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN3D_SURF',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN3D_SURF
!
!     ##############################################
      FUNCTION VER_INTERP_LIN2D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2
REAL,   DIMENSION(:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2))                   &
                                       :: PVAR2 ! variable values on target
!                                             ! grid
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER                                               :: JJ,JK2
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN2D_SURF',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JK2,JJ)
DO JK2=1,SIZE(KKLIN,2)
  DO JJ=1,SIZE(KKLIN,1)
    PVAR2(JJ,JK2)=    PCOEFLIN(JJ,JK2) *PVAR1(JJ,KKLIN(JJ,JK2)  )&
                      +(1.-PCOEFLIN(JJ,JK2))*PVAR1(JJ,KKLIN(JJ,JK2)+1)
  END DO
END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN2D_SURF',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN2D_SURF
!
!     ##############################################
      FUNCTION VER_INTERP_LIN1D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2
REAL,   DIMENSION(:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN))                   &
                                       :: PVAR2 ! variable values on target
!                                             ! grid
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER                                               :: JK2
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN1D_SURF',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JK2)
DO JK2=1,SIZE(KKLIN)
  PVAR2(JK2)=    PCOEFLIN(JK2) *PVAR1(KKLIN(JK2)  )&
                        +(1.-PCOEFLIN(JK2))*PVAR1(KKLIN(JK2)+1)
END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN1D_SURF',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN1D_SURF
END MODULE MODI_VER_INTERP_LIN_SURF
