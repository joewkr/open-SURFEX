!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE PUT_ZS_SURF_ATM_n (U, &
                                    HPROGRAM,KI,PZS)
!     ########################################
!
!!****  *PUT_ZS_SURF_ATM_n* - routine to modify surface oropgraphy using atmospheric
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
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
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_ZS_SURF_ATM_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PZS) /= SIZE(U%XZS) ) THEN
  WRITE(ILUOUT,*) 'try to get ZS field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PZS) :', SIZE(PZS)
  WRITE(ILUOUT,*) 'size of field in the surface                    (XZS) :', SIZE(U%XZS)
  CALL ABOR1_SFX('PUT_ZS_SURF_ATMN: GET ZS FROM ATMOSPHERIC MODEL: SIZE NOT CORRECT')
ELSE
  U%XZS = PZS
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ZS_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE PUT_ZS_SURF_ATM_n
