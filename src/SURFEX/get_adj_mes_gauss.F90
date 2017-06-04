!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_ADJ_MES_GAUSS(KGRID_PAR,KL,PGRID_PAR,KLEFT,KRIGHT,KTOP,KBOTTOM)
!     ##############################################################
!
!!**** *GET_ADJACENT_MESHES_GAUSS* get the near grid mesh indices
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
USE MODE_GRIDTYPE_GAUSS
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
INTEGER :: INLATI   ! number of pseudo-latitudes
REAL    :: ZLAPO    ! latitude of the rotated pole (deg)
REAL    :: ZLOPO    ! logitude of the rotated pole (deg)
REAL    :: ZCODIL   ! stretching factor
INTEGER, DIMENSION(:),ALLOCATABLE :: INLOPA ! number of pseudo-longitudes
!                                           ! on each pseudo-latitude circle
!                                           ! on pseudo-northern hemisphere
!                                           ! (starting from the rotated pole)
REAL, DIMENSION(:),ALLOCATABLE :: ZXCEN
!
INTEGER :: JLAT, JLON, IL, JL, ILGRID, JLON2, ID, JL0
!
REAL :: ZDIS, ZINTER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_GAUSS',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL)
!
ALLOCATE(INLOPA(0:INLATI))
ALLOCATE(ZXCEN(KL))
!
 CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA(1:INLATI), &
                         PLON_XY=ZXCEN        )
!
INLOPA(0) = 0
!
KLEFT  (:) = 0
KRIGHT (:) = 0
KTOP   (:) = 0
KBOTTOM(:) = 0
!
IL=0
DO JLAT=1,INLATI
   DO JLON=1,INLOPA(JLAT)
      IL=IL+1
   ENDDO
ENDDO
!
JL = 0.0
IF (IL==KL) THEN
  DO JLAT=1,INLATI
    !
    JL0 = JL
    !
    DO JLON=1,INLOPA(JLAT)
      !
      JL = JL + 1
      !
      IF (JLON>1            ) KLEFT  (JL) = JL-1
      IF (JLON<INLOPA(JLAT) ) KRIGHT (JL) = JL+1
      !
      IF (JLON==1           ) KLEFT  (JL) = JL+INLOPA(JLAT)-1
      IF (JLON==INLOPA(JLAT)) KRIGHT (JL) = JL-INLOPA(JLAT)+1
      !
      IF (JLAT>1            ) THEN
        ZDIS = ABS(ZXCEN(JL) - ZXCEN(JL0 - INLOPA(JLAT-1) + 1))
        ID = 1
        DO JLON2 = 1,INLOPA(JLAT-1)
          ZINTER = ABS(ZXCEN(JL) - ZXCEN(JL0 - INLOPA(JLAT-1) + JLON2))
          IF (ZINTER<ZDIS) THEN
            ZDIS = ZINTER
            ID = JLON2
          ENDIF
        ENDDO
        KTOP(JL) = JL0 - INLOPA(JLAT-1) + ID
      ENDIF
      !
      IF (JLAT<INLATI       ) THEN
        ZDIS = ABS(ZXCEN(JL) - ZXCEN(JL0 + INLOPA(JLAT) + 1))
        ID = 1
        DO JLON2 = 1,INLOPA(JLAT+1)
          ZINTER = ABS(ZXCEN(JL) - ZXCEN(JL0 + INLOPA(JLAT) + JLON2))
          IF (ZINTER<ZDIS) THEN
            ZDIS = ZINTER
            ID = JLON2
          ENDIF
        ENDDO
        KBOTTOM(JL) = JL0 + INLOPA(JLAT) + ID
      ENDIF
      !
    END DO
  END DO
END IF
!
DEALLOCATE(INLOPA)
DEALLOCATE(ZXCEN)
!
IF (LHOOK) CALL DR_HOOK('GET_ADJ_MES_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_ADJ_MES_GAUSS
