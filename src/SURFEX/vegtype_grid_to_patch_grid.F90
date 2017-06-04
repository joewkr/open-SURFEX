!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE VEGTYPE_GRID_TO_PATCH_GRID(KPATCH,KNPATCH,PVEGTYPE_PATCH,PPATCH,KMASK,PFIELDOUT,PW)
!        ################################################
!!
!!****  *VEGTYPE_GRID_TO_PATCH_GRID* averages fields from all (12) vegtypes 
!!                                   on only a few patches
!!    PURPOSE
!!    -------
!
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
!!
!!      V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!
!-------------------------------------------------------------------------------

!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
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
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN) :: KPATCH
INTEGER, INTENT(IN) :: KNPATCH
REAL, DIMENSION(:,:), INTENT(IN)   :: PVEGTYPE_PATCH
REAL, DIMENSION(:), INTENT(IN)   :: PPATCH
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
REAL, DIMENSION(:,:,:), INTENT(IN) :: PFIELDOUT
REAL, DIMENSION(:,:), INTENT(OUT) :: PW
!
!
!*      0.2    declarations of local variables
!
REAL :: ZINT
REAL, PARAMETER     :: ZPREC=1.0E+6
!
REAL, DIMENSION(SIZE(PW,1)) :: ZSUM
INTEGER                       :: JP    ! loop on patches
INTEGER                       :: JVEG  ! loop on vegtypes
INTEGER                       :: JL, JI, IMASK    ! loop on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!* averages from vegtypes to chosen number of patches
IF (LHOOK) CALL DR_HOOK('VEGTYPE_GRID_TO_PATCH_GRID',0,ZHOOK_HANDLE)
!
ZSUM(:) = 0.
DO JVEG=1,NVEGTYPE
  JP = VEGTYPE_TO_PATCH(JVEG,KNPATCH)
  IF (JP/=KPATCH) CYCLE
  DO JI = 1,SIZE(PW,1)
    ZSUM(JI) =ZSUM(JI) + PVEGTYPE_PATCH(JI,JVEG)   
  ENDDO
ENDDO
!
PW(:,:) = 0.
!
DO JVEG=1,NVEGTYPE
  JP = VEGTYPE_TO_PATCH(JVEG,KNPATCH)
  IF (JP/=KPATCH) CYCLE
  DO JL=1,SIZE(PW,2)
    DO JI = 1,SIZE(PW,1)
      IMASK = KMASK(JI)
      PW(JI,JL) = PW(JI,JL) + PVEGTYPE_PATCH(JI,JVEG) * PFIELDOUT(IMASK,JL,JVEG)
    ENDDO
  END DO
END DO
!
DO JI = 1,SIZE(PW,1)
  IF (ZSUM(JI)/=0.) PW(JI,:) = PW(JI,:) / ZSUM(JI)
ENDDO
!
DO JP = 1,SIZE(PW,2)
  DO JI = 1,SIZE(PW,1)
    IF (PW(JI,JP)/=XUNDEF) THEN
      ZINT = AINT(PW(JI,JP),KIND=16)
      IF (PW(JI,JP)/=ZINT) THEN
        PW(JI,JP) = ZINT + NINT((PW(JI,JP)-ZINT)*ZPREC)/ZPREC
      ENDIF
    ENDIF
  ENDDO
ENDDO
!
!* insures undefined value when patch is not present
!
DO JP=1,KNPATCH
  DO JL=1,SIZE(PW,2)
    WHERE(PPATCH(:)==0.) PW(:,JL) = XUNDEF
  END DO
END DO
WHERE( ABS(PW-XUNDEF)/XUNDEF < 1.E-6 ) PW = XUNDEF
IF (LHOOK) CALL DR_HOOK('VEGTYPE_GRID_TO_PATCH_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE VEGTYPE_GRID_TO_PATCH_GRID
