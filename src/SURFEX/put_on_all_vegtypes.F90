!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE PUT_ON_ALL_VEGTYPES(KNI,KLAYER,KPATCH,KVEGTYPE,PFIELD_PATCH,PFIELD_VEGTYPE)
!     #######################
!
USE MODI_VEGTYPE_TO_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!* dummy arguments
!  ---------------
!
INTEGER, INTENT(IN) :: KNI      ! number of points
INTEGER, INTENT(IN) :: KLAYER   ! number of layers
INTEGER, INTENT(IN) :: KPATCH   ! number of patch
INTEGER, INTENT(IN) :: KVEGTYPE ! number of vegtypes
REAL, DIMENSION(KNI,KLAYER,KPATCH  ),INTENT(IN) :: PFIELD_PATCH   ! field for each patch
REAL, DIMENSION(KNI,KLAYER,KVEGTYPE), INTENT(OUT) :: PFIELD_VEGTYPE ! field for each vegtype
!
!
!* local variables
!  ---------------
!
INTEGER :: IPATCH   ! patch   counter
INTEGER :: JVEGTYPE ! vegtype counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PUT_ON_ALL_VEGTYPES',0,ZHOOK_HANDLE)
IF (KVEGTYPE==1) THEN
  PFIELD_VEGTYPE(:,:,1) = PFIELD_PATCH(:,:,1)
ELSE
  DO JVEGTYPE=1,KVEGTYPE
    IPATCH = VEGTYPE_TO_PATCH(JVEGTYPE,KPATCH)
    PFIELD_VEGTYPE(:,:,JVEGTYPE) = PFIELD_PATCH(:,:,IPATCH)
  END DO
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ON_ALL_VEGTYPES',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE PUT_ON_ALL_VEGTYPES
