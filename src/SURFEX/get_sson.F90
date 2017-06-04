!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SSO_n (USS, &
                            HPROGRAM,KI,PSSO_SLOPE)
!     ########################################
!
!!****  *GET_SSO_n* - routine to get some surface fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SSO_n, ONLY : SSO_t
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
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI          ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(OUT) :: PSSO_SLOPE  ! subgrid slope
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SSO_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PSSO_SLOPE) /= SIZE(USS%XSSO_SLOPE) ) THEN
  WRITE(ILUOUT,*) 'try to get SSO_SLOPE fields from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PSSO_SLOPE) :', SIZE(PSSO_SLOPE)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XSSO_SLOPE) :', SIZE(USS%XSSO_SLOPE)
  CALL ABOR1_SFX('GET_SSON: SSO_SLOPE SIZE NOT CORRECT')
ELSE
  PSSO_SLOPE = USS%XSSO_SLOPE
END IF
IF (LHOOK) CALL DR_HOOK('GET_SSO_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_SSO_n
