!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_ADJ_MES_CONF_PROJ(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
!     ##############################################################
!
!!**** *GET_ADJACENT_MESHES_CONF_PROJ* get the near grid mesh indices
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_CONF_PROJ
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KLEFT     ! left   mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KRIGHT    ! right  mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KTOP      ! top    mesh index
INTEGER, DIMENSION(KL),          INTENT(OUT)   :: KBOTTOM   ! bottom mesh index
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(KL) :: ZX, ZY
INTEGER             :: IIMAX, IJMAX
INTEGER             :: JI, JJ
INTEGER             :: JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_CONF_PROJ',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,KIMAX=IIMAX,KJMAX=IJMAX,PX=ZX,PY=ZY)
!
KLEFT  (:) = 0
KRIGHT (:) = 0
KTOP   (:) = 0
KBOTTOM(:) = 0
!
IF (IIMAX*IJMAX==KL) THEN
  DO JJ=1,IJMAX
    DO JI=1,IIMAX
      JL = JI + IIMAX * (JJ-1)
      IF (JI>1    ) KLEFT  (JL) = JL-1
      IF (JI<IIMAX) KRIGHT (JL) = JL+1
      IF (JJ>1    ) KBOTTOM(JL) = JL-IIMAX
      IF (JJ<IJMAX) KTOP   (JL) = JL+IIMAX
    END DO
  END DO
END IF
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_ADJ_MES_CONF_PROJ
