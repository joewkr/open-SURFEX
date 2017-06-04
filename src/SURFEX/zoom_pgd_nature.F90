!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE ZOOM_PGD_NATURE (DTCO, IM, UG, U, USS, GCP, &
                                  HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE, &
                                   OECOCLIMAP                                      )  
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ZOOM_PGD_ISBA
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',0,ZHOOK_HANDLE)
IF (U%CNATURE=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CNATURE=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (U%CNATURE=='ISBA  ' .OR. U%CNATURE=='TSZ0') THEN
  CALL ZOOM_PGD_ISBA(IM%CHI, DTCO, IM%DTV, IM%G, IM%O, IM%S, IM%K, IM%ISS, UG, U, USS, GCP, &
                     HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE,OECOCLIMAP)
END IF
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
!
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_NATURE
