!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_AOS_n (USS, &
                            HPROGRAM,KI,PAOSIP,PAOSIM,PAOSJP,PAOSJM,&
                             PHO2IP,PHO2IM,PHO2JP,PHO2JM             )  
!     ########################################
!
!!****  *GET_AOS_n* - routine to get some surface fields
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
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(OUT) :: PAOSIP  ! subgrid orographic A/S, dir i+
REAL, DIMENSION(KI), INTENT(OUT) :: PAOSIM  ! subgrid orographic A/S, dir i-
REAL, DIMENSION(KI), INTENT(OUT) :: PAOSJP  ! subgrid orographic A/S, dir j+
REAL, DIMENSION(KI), INTENT(OUT) :: PAOSJM  ! subgrid orographic A/S, dir j-
REAL, DIMENSION(KI), INTENT(OUT) :: PHO2IP  ! subgrid orographic h/2, dir i+
REAL, DIMENSION(KI), INTENT(OUT) :: PHO2IM  ! subgrid orographic h/2, dir i-
REAL, DIMENSION(KI), INTENT(OUT) :: PHO2JP  ! subgrid orographic h/2, dir j+
REAL, DIMENSION(KI), INTENT(OUT) :: PHO2JM  ! subgrid orographic h/2, dir j-
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_AOS_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PAOSIP) /= SIZE(USS%XAOSIP) ) THEN
  WRITE(ILUOUT,*) 'try to get A/S fields from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PAOSIP) :', SIZE(PAOSIP)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XAOSIP) :', SIZE(USS%XAOSIP)
  CALL ABOR1_SFX('GET_AOSN: A/S SIZE NOT CORRECT')
ELSE
  PAOSIP = USS%XAOSIP
  PAOSIM = USS%XAOSIM
  PAOSJP = USS%XAOSJP
  PAOSJM = USS%XAOSJM
  PHO2IP = USS%XHO2IP
  PHO2IM = USS%XHO2IM
  PHO2JP = USS%XHO2JP
  PHO2JM = USS%XHO2JM
END IF
IF (LHOOK) CALL DR_HOOK('GET_AOS_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_AOS_n
