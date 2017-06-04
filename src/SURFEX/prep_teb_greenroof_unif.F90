!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GREENROOF_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GREENROOF_UNIF* - prepares ISBA field from prescribed values
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    Based on "prep_teb_garden_unif"
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!    A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!------------------------------------------------------------------
!
!
USE MODD_PREP,              ONLY : CINTERP_TYPE
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_PREP_TEB_GREENROOF,ONLY : XHUG_SURF_GR, XHUG_ROOT_GR, XHUG_DEEP_GR,     &
                                   XHUGI_SURF_GR, XHUGI_ROOT_GR, XHUGI_DEEP_GR,  &
                                   XTG_SURF_GR, XTG_ROOT_GR, XTG_DEEP_GR,        &
                                   XWR_DEF  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: JV ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1,1))
    PFIELD = 0.
   
!
!*      3.1    Profile of soil relative humidity
!
  CASE('WG     ')
    ALLOCATE(PFIELD(1,3,1))
    PFIELD(:,1,1) = XHUG_SURF_GR
    PFIELD(:,2,1) = XHUG_ROOT_GR
    PFIELD(:,3,1) = XHUG_DEEP_GR
!
!*      3.2    Profile of soil humidity for ice

  CASE('WGI    ')
    ALLOCATE(PFIELD(1,3,1))
    PFIELD(:,1,1) = XHUGI_SURF_GR
    PFIELD(:,2,1) = XHUGI_ROOT_GR
    PFIELD(:,3,1) = XHUGI_DEEP_GR

!*      3.3    Profile of temperatures

  CASE('TG     ')
    ALLOCATE(PFIELD(1,3,1))
    PFIELD(:,1,1) = XTG_SURF_GR
    PFIELD(:,2,1) = XTG_ROOT_GR
    PFIELD(:,3,1) = XTG_DEEP_GR

!*      3.4    Other quantities

  CASE('WR     ')
    ALLOCATE(PFIELD(1,1,1))
    PFIELD = XWR_DEF

  CASE('LAI    ')
    ALLOCATE(PFIELD(1,1,1))
    PFIELD = XUNDEF

END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_UNIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GREENROOF_UNIF
