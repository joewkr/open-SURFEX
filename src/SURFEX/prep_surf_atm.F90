!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE PREP_SURF_ATM (YSC, HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_SURF_ATM* - driver for surface fields preparation
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE MODI_PREP_NATURE
USE MODI_PREP_SEA
USE MODI_PREP_INLAND_WATER
USE MODI_PREP_TOWN
!
USE MODE_READ_GRIB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_SURF_VERSION
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
TYPE (PREP_CTL),   INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=28), INTENT(IN) :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),  INTENT(IN) :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28), INTENT(IN) :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),  INTENT(IN) :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
 CHARACTER(LEN=28)               :: YATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6)                :: YATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28)               :: YPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6)                :: YPGDFILETYPE! type of the Atmospheric file
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_SURF_ATM',0,ZHOOK_HANDLE)
 CALL SURF_VERSION
!-------------------------------------------------------------------------------------
!
IF ( LEN_TRIM(HATMFILE)>0 ) THEN
  YATMFILE=HATMFILE
ELSE
  YATMFILE='                            '
ENDIF
!
IF ( LEN_TRIM(HPGDFILE)>0 ) THEN
  YPGDFILE=HPGDFILE
ELSE
  YPGDFILE='                            '
ENDIF
!
IF (  LEN_TRIM(HATMFILETYPE)>0 ) THEN
  YATMFILETYPE=HATMFILETYPE
ELSE
  YATMFILETYPE='      '
ENDIF
!
IF (  LEN_TRIM(HPGDFILETYPE)>0 ) THEN
  YPGDFILETYPE=HPGDFILETYPE
ELSE
  YPGDFILETYPE='      '
ENDIF
!
IF (ASSOCIATED(YSC%U%XCOVER)) DEALLOCATE(YSC%U%XCOVER)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF(YSC%U%NDIM_SEA>0) CALL PREP_SEA(YSC%DTCO, YSC%UG, YSC%U, YSC%GCP, YSC%SM,  &
                                HPROGRAM,YATMFILE,YATMFILETYPE,YPGDFILE,YPGDFILETYPE,YDCTL)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF(YSC%U%NDIM_WATER>0) CALL PREP_INLAND_WATER(YSC%DTCO, YSC%USS, YSC%UG, YSC%U, YSC%GCP, YSC%FM, YSC%WM, &
                                        HPROGRAM,YATMFILE,YATMFILETYPE,YPGDFILE,YPGDFILETYPE,YDCTL)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF(YSC%U%NDIM_NATURE>0) CALL PREP_NATURE(YSC%DTCO, YSC%IM, YSC%UG, YSC%U, YSC%USS, YSC%GCP, &
                                         HPROGRAM,YATMFILE,YATMFILETYPE,YPGDFILE,YPGDFILETYPE,YDCTL)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF(YSC%U%NDIM_TOWN>0) CALL PREP_TOWN(YSC%DTCO, YSC%UG, YSC%U, YSC%USS, YSC%GCP, YSC%TM, YSC%GDM, YSC%GRM, &
                                     HPROGRAM,YATMFILE,YATMFILETYPE,YPGDFILE,YPGDFILETYPE,YDCTL)
!
 CALL CLEAR_GRIB_INDEX
!
IF (LHOOK) CALL DR_HOOK('PREP_SURF_ATM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SURF_ATM
