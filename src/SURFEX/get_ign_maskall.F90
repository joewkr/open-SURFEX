!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE GET_IGN_MASKALL (UG, KNI, PX, PY, OTOT)
!     #######################################################
!!****  *GET_IGN_MASKALL* - 
!!
!!    PURPOSE
!!    -------
!!
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
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010 
!!      07/2011     add specific computation for IGN grid (B. Decharme)
!-------------------------------------------------------------------------------                         
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_IO_SURF_OL, ONLY: NMASK_IGN
!
USE MODE_GRIDTYPE_IGN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER, INTENT(IN)              :: KNI
REAL,DIMENSION(:), INTENT(OUT)   :: PX, PY
LOGICAL, INTENT(IN), OPTIONAL :: OTOT
!
REAL, DIMENSION(KNI)             :: ZXX, ZYY
INTEGER                          :: JI, JJ, JK, JL
LOGICAL :: GTOT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_IGN_MASKALL',0,ZHOOK_HANDLE)
!
GTOT = .TRUE.
IF (PRESENT(OTOT)) GTOT = OTOT
!
IF (ASSOCIATED(UG%XGRID_FULL_PAR).AND.GTOT) THEN
  CALL GET_GRIDTYPE_IGN(UG%XGRID_FULL_PAR,PX=ZXX,PY=ZYY,PXALL=PX,PYALL=PY)
ENDIF
IF (.NOT.GTOT .AND. ASSOCIATED(UG%G%XGRID_PAR)) THEN
  IF (ASSOCIATED(UG%XGRID_FULL_PAR)) THEN
    CALL GET_GRIDTYPE_IGN(UG%XGRID_FULL_PAR,PXALL=PX,PYALL=PY)
    CALL GET_GRIDTYPE_IGN(UG%G%XGRID_PAR,PX=ZXX,PY=ZYY)
  ELSE
    CALL GET_GRIDTYPE_IGN(UG%G%XGRID_PAR,PX=ZXX,PY=ZYY,PXALL=PX,PYALL=PY)
  ENDIF
ENDIF
!
IF (.NOT.ALLOCATED(NMASK_IGN))THEN
  ALLOCATE(NMASK_IGN(KNI))
  JL=0
  DO JJ=1,SIZE(PY)  
    DO JI=1,SIZE(PX)
      JL=JL+1
      DO JK=1,KNI
        IF((ZXX(JK)==PX(JI)).AND.(ZYY(JK)==PY(JJ)))THEN
          NMASK_IGN(JK) = JL
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('GET_IGN_MASKALL',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_IGN_MASKALL
