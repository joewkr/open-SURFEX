!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_SSO_STDEV_n (USS, &
                                  HPROGRAM,KI,PSSO_STDEV)
!     ########################################
!
!!****  *GET_SSO_STDEV_n* - routine to get some surface fields
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
!!      S. Riette   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
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
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(OUT) :: PSSO_STDEV     ! S.S.O. standard deviation (m)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SSO_STDEV_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PSSO_STDEV) /= SIZE(USS%XSSO_STDEV) ) THEN
  WRITE(ILUOUT,*) 'try to get SSO_STDEV field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PSSO_STDEV) :', SIZE(PSSO_STDEV)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XSSO_STDEV) :', SIZE(USS%XSSO_STDEV)
  CALL ABOR1_SFX('GET_SSO_STDEVN: SSO_STDEV SIZE NOT CORRECT')
ELSE
  PSSO_STDEV = USS%XSSO_STDEV
END IF
IF (LHOOK) CALL DR_HOOK('GET_SSO_STDEV_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_SSO_STDEV_n
