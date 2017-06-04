!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
SUBROUTINE GET_XYALL_IGN(PX,PY,PDX,PDY,PXALL,PYALL,KDIMX,KDIMY)
!     ################################################################
!
!!****  *GET_XYALL_IGN* 
!!
!!    PURPOSE
!!    -------
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
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN) :: PX
REAL, DIMENSION(:), INTENT(IN) :: PY
REAL, DIMENSION(:), INTENT(IN) :: PDX
REAL, DIMENSION(:), INTENT(IN) :: PDY
REAL, DIMENSION(:), INTENT(OUT) :: PXALL
REAL, DIMENSION(:), INTENT(OUT) :: PYALL
INTEGER, INTENT(OUT) :: KDIMX
INTEGER, INTENT(OUT) :: KDIMY
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(MAX(SIZE(PXALL),SIZE(PYALL))*3) :: ZALL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN',0,ZHOOK_HANDLE)
!
KDIMX = 0
KDIMY = 0
!
 CALL GET_COORD(PX,PDX,ZALL,KDIMX)
PXALL(1:KDIMX) = ZALL(1:KDIMX)
!
 CALL GET_COORD(PY,PDY,ZALL,KDIMY)
PYALL(1:KDIMY) = ZALL(1:KDIMY) 
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE GET_COORD(PIN,PDIN,POUT,KSIZE)
!
IMPLICIT NONE
!
REAL, DIMENSION(:), INTENT(IN) :: PIN
REAL, DIMENSION(:), INTENT(IN) :: PDIN
REAL, DIMENSION(:), INTENT(OUT) :: POUT
INTEGER, INTENT(INOUT) :: KSIZE
REAL, DIMENSION(SIZE(POUT)) :: ZDOUT, ZOUT, ZDOUT2
REAL :: ZMAX, ZMIN
INTEGER :: I, J, IDMIN, ICPT, ISIZE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:GET_COORD',0,ZHOOK_HANDLE)
!
KSIZE = SIZE(PIN)
ZOUT (1:KSIZE) = PIN (:)
ZDOUT(1:KSIZE) = PDIN(:)
!
ZMAX  = MAXVAL(ZOUT(1:KSIZE))
DO J=1,KSIZE
  ZMIN  = MINVAL(ZOUT(1:KSIZE))
  POUT(J) = ZMIN
  IDMIN = MINLOC(ZOUT(1:KSIZE),1)
  ZDOUT2(J) = ZDOUT(IDMIN)  
  ZOUT(IDMIN) = ZMAX+1
ENDDO
!
ZOUT (1:KSIZE) = POUT (1:KSIZE)
ZDOUT(1:KSIZE) = ZDOUT2(1:KSIZE)
!
ICPT = 1
ISIZE = KSIZE
DO J=1,ISIZE-1
  IF (ZOUT(J)==ZOUT(J+1)) THEN
    IF (J<ISIZE-1) THEN
      POUT (ICPT+1:KSIZE-1) = ZOUT  (J+2:ISIZE)
      ZDOUT(ICPT+1:KSIZE-1) = ZDOUT2(J+2:ISIZE)
    ENDIF
    KSIZE = KSIZE - 1
  ELSE
    ICPT = ICPT + 1
  ENDIF
ENDDO
!
ISIZE = KSIZE
DO J=1,ISIZE-1
  IF (POUT(J)+ZDOUT(J)/2.<POUT(J+1)-ZDOUT(J+1)/2.) THEN
    ICPT = 0
    KSIZE = KSIZE + 1
    POUT (KSIZE) = POUT (J) + ZDOUT(J)
    ZDOUT(KSIZE) = ZDOUT(J)
    DO WHILE ( POUT(KSIZE)+ZDOUT(KSIZE)/2. < POUT(J+1)-ZDOUT(J+1)/2. )
      POUT (KSIZE+1) = POUT (KSIZE) + ZDOUT(KSIZE)
      ZDOUT(KSIZE+1) = ZDOUT(KSIZE)
      KSIZE = KSIZE + 1
    ENDDO
  ENDIF
ENDDO
!
ZOUT(1:KSIZE) = POUT(1:KSIZE)
!
ZMAX  = MAXVAL(ZOUT(1:KSIZE))
DO J=1,KSIZE
  ZMIN  = MINVAL(ZOUT(1:KSIZE))
  POUT(J) = ZMIN
  IDMIN = MINLOC(ZOUT(1:KSIZE),1)
  ZOUT(IDMIN) = ZMAX+1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:GET_COORD',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_COORD
!
END SUBROUTINE GET_XYALL_IGN
