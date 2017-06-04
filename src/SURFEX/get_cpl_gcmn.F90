!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_CPL_GCM_n (U, &
                                HPROGRAM,KI,PRAIN,PSNOW,PZ0,PZ0H,PQSURF)
!     ######################################################
!
!!****  *GET_CPL_GCM_n* - routine to get physical fields   
!!                      for initialise ARPEGE/ALADIN run
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_ATM,   ONLY : LCPL_GCM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),        INTENT(IN)  :: HPROGRAM
INTEGER,                 INTENT(IN)  :: KI       ! number of points
!
REAL, DIMENSION(KI),     INTENT(OUT) :: PRAIN    ! total rainfall rate (kg/m2/s)
REAL, DIMENSION(KI),     INTENT(OUT) :: PSNOW    ! total snowfall rate (kg/m2/s)
REAL, DIMENSION(KI),     INTENT(OUT) :: PZ0      ! roughness length for momentum (m)
REAL, DIMENSION(KI),     INTENT(OUT) :: PZ0H     ! roughness length for heat (m)
REAL, DIMENSION(KI),     INTENT(OUT) :: PQSURF   ! specific humidity at surface (kg/kg)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_CPL_GCM_N',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF(LCPL_GCM) THEN
!
  IF(SIZE(PZ0)/=SIZE(U%XZ0H))THEN
    WRITE(ILUOUT,*)'try to get Z0 field from atmospheric model, but size is not correct'
    WRITE(ILUOUT,*)'size of field expected by the atmospheric model (PZ0) :', SIZE(PZ0)
    WRITE(ILUOUT,*)'size of field in SURFEX                         (XZ0) :', SIZE(U%XZ0)
    CALL ABOR1_SFX('GET_CPL_GCMN: PZ0 ARGUMENT SIZE /= XZ0 MODULE SIZE')
  ENDIF
!
  PRAIN (:) = U%XRAIN
  PSNOW (:) = U%XSNOW
  PZ0   (:) = U%XZ0
  PZ0H  (:) = U%XZ0H
  PQSURF(:) = U%XQSURF
!
ELSE
!
  WRITE(ILUOUT,*)'LCPL_GCM must be TRUE when you use atmospheric model'
  CALL ABOR1_SFX('GET_CPL_GCMN: LCPL_GCM must be TRUE')
!  
ENDIF

!
IF (LHOOK) CALL DR_HOOK('GET_CPL_GCM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_CPL_GCM_n
