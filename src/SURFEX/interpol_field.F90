!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################
      SUBROUTINE INTERPOL_FIELD (UG, U, &
                                 HPROGRAM,KLUOUT,KCODE,PFIELD,HFIELD,PDEF,KNPTS)
!     ################################################
!
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_ABOR1_SFX
USE MODI_INTERPOL_FIELD2D
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),        INTENT(IN)   :: HPROGRAM ! host program
INTEGER,                 INTENT(IN)   :: KLUOUT   ! output listing logical unit
INTEGER,DIMENSION(:),    INTENT(INOUT):: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:),    INTENT(INOUT):: PFIELD   ! pgd field on grid mesh
 CHARACTER(LEN=*),        INTENT(IN)   :: HFIELD   ! name of the field for prints
REAL,           OPTIONAL,INTENT(IN)   :: PDEF     ! default value if not enough data
INTEGER, OPTIONAL,       INTENT(IN)   :: KNPTS    ! number of points to interpolate with
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(PFIELD),1) :: ZFIELD
REAL, DIMENSION(1)              :: ZDEF
INTEGER                         :: INPTS          ! number of points to interpolate with
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD',0,ZHOOK_HANDLE)
!
INPTS = 3
IF (PRESENT(KNPTS)) INPTS = KNPTS
!
ZFIELD(:,1) = PFIELD(:)
!
IF (PRESENT(PDEF)) THEN
  ZDEF = PDEF
  CALL INTERPOL_FIELD2D(UG, U, &
                        HPROGRAM,KLUOUT,KCODE,ZFIELD,HFIELD,ZDEF,KNPTS=INPTS)
ELSE
  CALL INTERPOL_FIELD2D(UG, U, &
                        HPROGRAM,KLUOUT,KCODE,ZFIELD,HFIELD,KNPTS=INPTS)
END IF
!
PFIELD(:)   = ZFIELD(:,1)
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
END SUBROUTINE INTERPOL_FIELD

