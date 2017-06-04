!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      SUBROUTINE REFRESH_PGDWORK(HSUBROUTINE)
!     ##########################
!
!!**** *REFRESH_PGDWORK* ! refreshes arrays used in PGD work module
!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!!
!
USE MODD_PGDWORK,  ONLY : XSSO_ALL, NSSO_ALL, XSUMVAL, XEXT_ALL, NSIZE_ALL, XALL
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!----------------------------------------------------------------------------
!
!*    1.     Cover array
!            -----------
!
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('REFRESH_PGDWORK',0,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
!
!*    2.     General arrays
!            --------------
!
IF (ALLOCATED(XALL)) THEN
  XALL(:,:,:) = 0.
ENDIF
IF (ALLOCATED(XSUMVAL)) THEN
  XSUMVAL(:,:)=0.
END IF
IF (ALLOCATED(XEXT_ALL)) THEN
  XEXT_ALL(:,1)=-99999.
  XEXT_ALL(:,2)=99999.
END IF
IF (ALLOCATED(NSIZE_ALL)) THEN
  NSIZE_ALL(:,:)=0
END IF
!----------------------------------------------------------------------------
!
!*    3.     Subgrid arrays
!            --------------
!
IF (ALLOCATED(XSSO_ALL) .AND. ALLOCATED(NSSO_ALL)) THEN
  IF (HSUBROUTINE=='A_OROG') THEN
    XSSO_ALL(:,:,:) = -XUNDEF
  ELSE
    XSSO_ALL(:,:,:) = 0.
  ENDIF
  NSSO_ALL(:,:,:) = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('REFRESH_PGDWORK',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE REFRESH_PGDWORK
