!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################
      SUBROUTINE PUT_ZS_INLAND_WATER_n (F, W, &
                                        HPROGRAM,KI,PZS,HWATER)
!     #################################################
!
!!****  *PUT_ZS_INLAND_WATER_n* - routine to modify inland water oropgraphy using atmospheric
!                    model orography
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
!!      Original    05/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODI_GET_LUOUT
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
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HWATER ! name of the scheme for inland water
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(IN)  :: PZS     ! orography
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PUT_ZS_INLAND_WATER_N',0,ZHOOK_HANDLE)
IF (HWATER=='FLAKE ') THEN
   CALL PUT_ZS_FLAKE_n
ELSE
   CALL PUT_ZS_WATFLX_n
END IF
!
IF (LHOOK) CALL DR_HOOK('PUT_ZS_INLAND_WATER_N',1,ZHOOK_HANDLE)
CONTAINS
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
SUBROUTINE PUT_ZS_WATFLX_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('PUT_ZS_WATFLX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PZS) /= SIZE(W%XZS) ) THEN
  WRITE(ILUOUT,*) 'try to get ZS field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PZS) :', SIZE(PZS)
  WRITE(ILUOUT,*) 'size of field for inland water (WATFLX)         (XZS) :', SIZE(W%XZS)
  CALL ABOR1_SFX('PUT_ZS_INLAND_WATERN (WATFLX): GET ZS FROM ATMOSPHERIC MODEL: SIZE NOT CORRECT')
ELSE
  W%XZS = PZS
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ZS_WATFLX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE PUT_ZS_WATFLX_n
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
SUBROUTINE PUT_ZS_FLAKE_n
!
!
!-------------------------------------------------------------------------------

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('PUT_ZS_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PZS) /= SIZE(F%XZS) ) THEN
  WRITE(ILUOUT,*) 'try to get ZS field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PZS) :', SIZE(PZS)
  WRITE(ILUOUT,*) 'size of field for inland water (FLAKE)          (XZS) :', SIZE(F%XZS)
  CALL ABOR1_SFX('PUT_ZS_INLAND_WATERN (FLAKE): GET ZS FROM ATMOSPHERIC MODEL: SIZE NOT CORRECT')
ELSE
  F%XZS = PZS
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ZS_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE PUT_ZS_FLAKE_n
!==============================================================================
!
END SUBROUTINE PUT_ZS_INLAND_WATER_n
