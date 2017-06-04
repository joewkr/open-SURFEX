!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_ADJ_MES_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
!     ##############################################################
!
!!**** *GET_ADJ_MES_LONLATVAL* get the near grid mesh indices
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
USE MODE_GRIDTYPE_LONLATVAL
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
REAL,DIMENSION(KL)    :: ZX
REAL,DIMENSION(KL)    :: ZY
REAL,DIMENSION(KL)    :: ZDX
REAL,DIMENSION(KL)    :: ZDY
REAL :: ZECX, ZECY, ZECDX, ZECDY
INTEGER :: JLAT, JLON
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_LONLATVAL',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,IL,ZX,ZY,ZDX,ZDY)
!
KLEFT  (:) = 0
KRIGHT (:) = 0
KTOP   (:) = 0
KBOTTOM(:) = 0
!
DO JLAT=1,KL
  !
  DO JLON=1,KL
    !
    ZECX = ABS(ZX(JLON)-ZX(JLAT))
    ZECY = ABS(ZY(JLON)-ZY(JLAT))
    !
    ZECDX = (ZDX(JLON)+ZDX(JLAT))/2.
    ZECDY = (ZDY(JLON)+ZDY(JLAT))/2.
    !
    IF ( ZECX <= ZECDX .AND. ZECY <= ZECDY ) THEN ! points overlap or are next to each other in x and y directions
      !
      IF ( ZECDY-ZECY <= ZECDX-ZECX .AND. ZECX/=ZECDX ) THEN ! overlap smaller in y than in x
        !
        IF ( ZY(JLON) < ZY(JLAT) .AND. &                       ! Y under X in y direction
           ( KBOTTOM(JLAT)==0                     .OR. &     ! bottom not assigned yet
             ZECY < ABS(ZY(MAX(1,KBOTTOM(JLAT)))-ZY(JLAT)) .OR. &     ! this y point is closer to x in y direction
             ZECX < ABS(ZX(MAX(1,KBOTTOM(JLAT)))-ZX(JLAT)) ) ) THEN   ! this y point is closer to x in x direction
          !
          KBOTTOM(JLAT) = JLON
          !
        ELSEIF ( ZY(JLON) > ZY(JLAT) .AND. &                   ! Y above X in y direction
               ( KTOP(JLAT)==0                     .OR. &    ! top not assigned yet
                 ZECY < ABS(ZY(MAX(1,KTOP(JLAT)))-ZY(JLAT)) .OR. &    ! this y point is closer to x in y direction
                 ZECX < ABS(ZX(MAX(1,KTOP(JLAT)))-ZX(JLAT)) ) ) THEN  ! this y point is closer to x in x direction
          !
          KTOP(JLAT) = JLON
          !
        ENDIF
        !
      ELSEIF (ZECDX-ZECX < ZECDY-ZECY ) THEN ! overlap smaller in x than in y
        !
        IF ( ZX(JLON) < ZX(JLAT) .AND. &                     ! Y left X in x direction
           ( KLEFT(JLAT)==0                     .OR. &     ! left not assigned yet
             ZECY < ABS(ZY(MAX(1,KLEFT(JLAT)))-ZY(JLAT)) .OR. &     ! this y point is closer to x in y direction
             ZECX < ABS(ZX(MAX(1,KLEFT(JLAT)))-ZX(JLAT)) ) ) THEN   ! this y point is closer to x in x direction            
          !
          KLEFT(JLAT)=JLON
          !
        ELSEIF ( ZX(JLON) > ZX(JLAT) .AND. &                     ! Y right X in x direction
               ( KRIGHT(JLAT)==0                     .OR. &    ! right not assigned yet
                 ZECY < ABS(ZY(MAX(1,KRIGHT(JLAT)))-ZY(JLAT)) .OR. &    ! this y point is closer to x in y direction
                 ZECX < ABS(ZX(MAX(1,KRIGHT(JLAT)))-ZX(JLAT)) ) ) THEN  ! this y point is closer to x in x direction
          !
          KRIGHT(JLAT)=JLON
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
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_LONLATVAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_ADJ_MES_LONLATVAL
