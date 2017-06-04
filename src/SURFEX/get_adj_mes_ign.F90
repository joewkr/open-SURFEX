!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_ADJ_MES_IGN(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
!     ##############################################################
!
!!**** *GET_ADJACENT_MESHES_IGN* get the near grid mesh indices
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
!!    E. Martin         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2007
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_IGN
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
REAL,DIMENSION(KL)    :: ZX
REAL,DIMENSION(KL)    :: ZY
REAL,DIMENSION(KL)    :: ZDX
REAL,DIMENSION(KL)    :: ZDY
REAL :: ZECX, ZECY, ZECDX, ZECDY
INTEGER :: JX, JY
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_IGN',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY)
!
KLEFT  (:) = 0
KRIGHT (:) = 0
KTOP   (:) = 0
KBOTTOM(:) = 0
!
DO JX=1,KL
  !
  DO JY=1,KL
    !
    ZECX = ABS(ZX(JY)-ZX(JX))
    ZECY = ABS(ZY(JY)-ZY(JX))
    !
    ZECDX = (ZDX(JY)+ZDX(JX))/2.
    ZECDY = (ZDY(JY)+ZDY(JX))/2.
    !
    IF ( ZECX <= ZECDX .AND. ZECY <= ZECDY ) THEN ! points overlap or are next to each other in x and y directions
      !
      IF ( ZECDY-ZECY <= ZECDX-ZECX .AND. ZECX/=ZECDX ) THEN ! overlap smaller in y than in x
        !
        IF ( ZY(JY) < ZY(JX) .AND. &                       ! Y under X in y direction
           ( KBOTTOM(JX)==0                     .OR. &     ! bottom not assigned yet
             ZECY < ABS(ZY(MAX(1,KBOTTOM(JX)))-ZY(JX)) .OR. &     ! this y point is closer to x in y direction
             ZECX < ABS(ZX(MAX(1,KBOTTOM(JX)))-ZX(JX)) ) ) THEN   ! this y point is closer to x in x direction
          !
          KBOTTOM(JX) = JY
          !
        ELSEIF ( ZY(JY) > ZY(JX) .AND. &                   ! Y above X in y direction
               ( KTOP(JX)==0                     .OR. &    ! top not assigned yet
                 ZECY < ABS(ZY(MAX(1,KTOP(JX)))-ZY(JX)) .OR. &    ! this y point is closer to x in y direction
                 ZECX < ABS(ZX(MAX(1,KTOP(JX)))-ZX(JX)) ) ) THEN  ! this y point is closer to x in x direction
          !
          KTOP(JX) = JY
          !
        ENDIF
        !
      ELSEIF (ZECDX-ZECX < ZECDY-ZECY ) THEN ! overlap smaller in x than in y
        !
        IF ( ZX(JY) < ZX(JX) .AND. &                     ! Y left X in x direction
           ( KLEFT(JX)==0                     .OR. &     ! left not assigned yet
             ZECY < ABS(ZY(MAX(1,KLEFT(JX)))-ZY(JX)) .OR. &     ! this y point is closer to x in y direction
             ZECX < ABS(ZX(MAX(1,KLEFT(JX)))-ZX(JX)) ) ) THEN   ! this y point is closer to x in x direction            
          !
          KLEFT(JX)=JY
          !
        ELSEIF ( ZX(JY) > ZX(JX) .AND. &                     ! Y right X in x direction
               ( KRIGHT(JX)==0                     .OR. &    ! right not assigned yet
                 ZECY < ABS(ZY(MAX(1,KRIGHT(JX)))-ZY(JX)) .OR. &    ! this y point is closer to x in y direction
                 ZECX < ABS(ZX(MAX(1,KRIGHT(JX)))-ZX(JX)) ) ) THEN  ! this y point is closer to x in x direction
          !
          KRIGHT(JX)=JY
          !
        ENDIF
        !
      ENDIF  
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_ADJ_MES_IGN
