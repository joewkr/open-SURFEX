!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_COORD_n (UG, &
                              HPROGRAM,KI,PLON,PLAT)
!     #############################################
!
!!****  *GET_COORD_n* - routine to get some surface fields
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_GET_LUOUT
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI         ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(OUT) :: PLON, PLAT
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_COORD_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PLON) /= SIZE(UG%G%XLON) .OR. SIZE(PLAT) /= SIZE(UG%G%XLAT) ) THEN
  WRITE(ILUOUT,*) 'try to get LON/LAT field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PLON) :', SIZE(PLON)
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PLAT) :', SIZE(PLAT)
  WRITE(ILUOUT,*) 'size of field in the surface                    (XLAT) :', SIZE(UG%G%XLAT)
  WRITE(ILUOUT,*) 'size of field in the surface                    (XLAT) :', SIZE(UG%G%XLAT)
  CALL ABOR1_SFX('GET_COORDN: LON/LAT SIZE NOT CORRECT')
ELSE
  PLON = UG%G%XLON
  PLAT = UG%G%XLAT
END IF
IF (LHOOK) CALL DR_HOOK('GET_COORD_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_COORD_n
