!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_HORIBL_SURF_GRIDIN
CONTAINS
    SUBROUTINE HORIBL_SURF_GRIDIN(KINLA,KINLO,KILEN,PARIN,KOLEN,&
                                 ODVECT,KLUOUT,OGLOBS,OGLOBN,OGLOBLON,KP,&
                                 PARIN0_OUT,PARIN_OUT,KLSMIN_OUT,KLSMIN,KLSMOUT,KMASK )
!   ###########################################################################
!
!!****  *HORIBL_SURF_GRIDIN* - horitontal bilinear interpolation
!!
!!    PURPOSE
!!    -------
!!
!!    Interpolates a field, supports masks.
!!
!!    METHOD
!!    ------
!!
!!    This routine performs a bilinear interpolation based on the 12 surrounding
!!    points. It begins with an interpolation along the latitudes (with third order
!!    polynoms interpolation with 4 points and linear interpolation for 2 points)
!!    and then a second along the longitude (third order polynoms interpolation).
!!    Two interpolations are performed : first along the parallels then between the
!!    four resulting points.
!!
!!    The disposition of the points is the following :
!!
!!
!!            N         1   2
!!
!!            ^     3   4   5   6
!!            |           x
!!            |     7   8   9  10
!!            |
!!                     11  12
!!            S
!!              W ---------------> E
!!
!!   Note : the name 'south', 'north', may not be exact if the last data point is
!!     to the south of first (delta latitude < 0). This does not affect computations.
!!
!!   The formula used to compute the weight is :
!!        (Lon   - Lon.i) . (Lon   - Lon.i) . (Lon   - Lon.i)
!!   Wi = ---------------------------------------------------
!!        (Lon.i - Lon.j) . (Lon.i - Lon.k) . (Lon.i - Lon.l)
!!   Where j,k,l are the other points of the line.
!!
!!   When masks are used, points with different types than the output points are
!!   not taken in account (in the formula, the corresponding coefficient is set
!!   to 1). If no points of the same nature are available, the interpolation is
!!   performed anyway with the 12 points. It is the task of the calling program
!!   to react to this situation.
!!
!!   When the inputs parameters define a circular map (or global), the inputs data
!!   are extended. The value of the parameter ODVECT is used to know if the datas
!!   are vectorial or scalar (this affects the sign of extended values).
!!
!!   EXTERNAL
!!   --------
!!
!!   subroutine FMLOOK_ll : to retrieve the logical unit number of the listing file
!!
!!   IMPLICIT ARGUMENTS
!!   ------------------
!!
!!   REFERENCE
!!   ---------
!!
!!   This routine is based on the one used by the software FULL-POS from Meteo France.
!!   More informations may be found in 'Book 1'
!!
!!   AUTHOR
!!   ------
!!
!!   J.Pettre & V.Bousquet
!!
!!   MODIFICATIONS
!!   -------------
!!
!!   Original       07/01/1999
!!                  21/04/1999 (V. Masson) set correct prefixes and bug in
!!                             a logical definition
!!                  21/04/1999 (V. Masson) bug in north and south poles
!!                             extension for input map land-sea mask
!!                  27/05/1999 (V. Masson) bug in 'grib south pole'
!!                             extrapolation (number of point per parallel)
!!                  27/05/1999 (V. Masson) bug in 'grib pole' extrapolation
!!                             extra latitudes are now computed symetrically
!!                             to the poles.
!!                  17/03/2010 (P. LeMoigne) bug in weights computations
!!                  16/06/2010 (G. Tanguy) bug in 'grib north pole"
!!                              extrapolation (tabular ZARIN not totaly filled)
!!
!------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!       ---------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, IDX_I
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1. Declaration of arguments
!
INTEGER,                   INTENT(IN)  :: KINLA   ! Number of parallels
INTEGER, DIMENSION(:), INTENT(IN)  :: KINLO   ! Number of point along a parallel
INTEGER,                   INTENT(IN)  :: KILEN   ! size of input arrays
REAL,    DIMENSION(:,:), INTENT(IN)  :: PARIN   ! input array
INTEGER,                   INTENT(IN)  :: KOLEN   ! size of output array
LOGICAL,                   INTENT(IN)  :: ODVECT  ! data is vectorial (True/False)
INTEGER,                   INTENT(IN)  :: KLUOUT  ! output listing logical unit
LOGICAL,                   INTENT(IN)  :: OGLOBS
LOGICAL,                   INTENT(IN)  :: OGLOBN
LOGICAL,                   INTENT(IN)  :: OGLOBLON
INTEGER, DIMENSION(:,:), INTENT(IN) :: KP
REAL,    DIMENSION(:,:), POINTER  :: PARIN0_OUT   ! input array
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PARIN_OUT
INTEGER, DIMENSION(:,:,:), INTENT(OUT) :: KLSMIN_OUT
INTEGER, DIMENSION(:), POINTER, OPTIONAL :: KMASK
INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL  :: KLSMIN  ! input land/sea mask
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL  :: KLSMOUT ! output land/sea mask
!
!*      0.2. Declaration of local variables
!
REAL,   DIMENSION(:,:), ALLOCATABLE :: ZARIN     ! Extended input datas
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZOUT
REAL :: ZVECT     ! -1 if input is vectorial
 ! Variables implied in the extension procedure
INTEGER, DIMENSION(:,:), ALLOCATABLE :: ILSMIN    ! Extended land/sea mask
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IP
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IOUT
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDARIN
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IDLS
INTEGER, DIMENSION(2) :: IEXT
INTEGER :: IBIGSIZE  ! Size of the extended map
INTEGER :: IMID   ! Used for extensions around the poles
INTEGER :: IOF1  ! Offset in map
INTEGER :: IOF2  ! Offset in map
 ! Loop counters
INTEGER :: JOP     ! Output position
INTEGER :: JIP     ! Input position
INTEGER :: JL, JI, JI2, JL2, JT, INL, JC   ! Dummy counter
INTEGER :: ID1, ID2, ISIZE
INTEGER :: INFOMPI, I, J
INTEGER :: IKINLO
LOGICAL :: LDLSM     ! Specify if land/sea mask is present or not
!
!------------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!------------------------------------------------------------------------------
!
!*     1. DETERMINATION  of the latitude of the poles (depending of the latitude
!         -------------                                 of the first data point)
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_1',0,ZHOOK_HANDLE)
!
IF (PRESENT(KMASK)) NULLIFY(KMASK)
!
INL = SIZE(PARIN_OUT,2)
!
LDLSM = .FALSE.
IF (PRESENT(KLSMIN) .AND. PRESENT(KLSMOUT)) LDLSM = .TRUE.
!
!* 2.6 Compute the resulting map
!
ZVECT = 1.
IF (ODVECT) ZVECT=-1.
!
IF (NRANK==NPIO) THEN

  IBIGSIZE = KILEN
  IF (OGLOBS  ) IBIGSIZE=IBIGSIZE+(4+KINLO(    1))+(4+KINLO(      2))
  IF (OGLOBN  ) IBIGSIZE=IBIGSIZE+(4+KINLO(KINLA))+(4+KINLO(KINLA-1))
  IF (OGLOBLON) IBIGSIZE=IBIGSIZE+ 4*KINLA
  !
  ALLOCATE (ZARIN (IBIGSIZE,INL))
  ALLOCATE (ILSMIN(IBIGSIZE,INL))
  ALLOCATE (KMASK (IBIGSIZE))
  KMASK(:) = -1
!
! 2.6.1 Compute the longitude extension
!
! This is a basic copy of the data. If extension is possible, the first and last
! two lines are copied twice this way :
!
!    /---------------\
!    |               |
!   [.] [.] [....   ...] [.] [.]
!        |            |
!        \------------/
!
! A point represent a data.
!

  DO JT = 1,INL

    JIP = 1
    JOP = 1
    IF (OGLOBS) JOP = JOP + (4+KINLO(1)) + (4+KINLO(2))

    IF (OGLOBLON) THEN

      DO JL = 1,KINLA
        ID1 = JIP+KINLO(JL)
        ID2 = JOP+2+KINLO(JL)

        KMASK(JOP  ) = ID1-2
        KMASK(JOP+1) = ID1-1
        DO JI = JOP+2,ID2-1
          KMASK(JI) = JIP + JI - (JOP+2)
        ENDDO
        KMASK(ID2  ) = JIP
        KMASK(ID2+1) = JIP+1

        ZARIN(JOP  ,JT) = PARIN(ID1-2,JT)
        ZARIN(JOP+1,JT) = PARIN(ID1-1,JT)
        ZARIN(JOP+2:ID2-1,JT) = PARIN(JIP:ID1-1,JT)
        ZARIN(ID2  ,JT) = PARIN(JIP  ,JT)
        ZARIN(ID2+1,JT) = PARIN(JIP+1,JT)

        IF (LDLSM) THEN
          ILSMIN(JOP  ,JT) = KLSMIN(ID1-2,JT)
          ILSMIN(JOP+1,JT) = KLSMIN(ID1-1,JT)
          ILSMIN(JOP+2:ID2-1,JT) = KLSMIN(JIP:ID1-1,JT)
          ILSMIN(ID2  ,JT) = KLSMIN(JIP  ,JT)
          ILSMIN(ID2+1,JT) = KLSMIN(JIP+1,JT)
        END IF

        JIP = JIP + KINLO(JL)
        JOP = JOP + KINLO(JL) + 4

      END DO

    ELSE

      DO JI = JOP,JOP+KILEN-1
        KMASK(JI) = JIP + JI - JOP
      ENDDO

      ZARIN(JOP:JOP+KILEN-1,JT) = PARIN(JIP:JIP+KILEN-1,JT)
      IF (LDLSM) THEN
        ILSMIN(JOP:JOP+KILEN-1,JT) = KLSMIN(JIP:JIP+KILEN-1,JT)
      END IF
    END IF
!
! 2.6.2 Compute the south pole extension
!
! Pole extension is performed by copying the first half datas to the last half
! datas of the extension parallel :
!
!  [.] [.] [....] [....] [.] [.]
!                  ||||
!            /-------/
!           ||||
!  [.] [.] [....] [....] [.] [.]
!
    DO JC = 1,4
      !
      IF (JC<3.AND.OGLOBS.OR.JC>2.AND.OGLOBN) THEN
        !
        IF (JC==1) THEN
          IOF1 = 4 + KINLO(2)
          IOF2 = IOF1 + 4 + KINLO(1)
          IMID = (KINLO(1)+4) / 2
          IKINLO = KINLO(1)
        ELSEIF (JC==2) THEN
          IOF1 = 0
          IOF2 = IOF2 + 4 + KINLO(2)
          IMID = (KINLO(2)+4) / 2
          IKINLO = KINLO(2)
        ELSEIF (JC==3) THEN
          IOF1 = IBIGSIZE - (4+KINLO(KINLA-1)) - (4+KINLO(KINLA))
          IOF2 = IOF1 - (4+KINLO(KINLA))
          IMID = (KINLO(KINLA)+4) / 2
          IKINLO =  KINLO(KINLA)
        ELSEIF (JC==4) THEN
          IOF1 = IOF1 + (4+KINLO(KINLA))
          IOF2 = IOF2 - (4+KINLO(KINLA-1))
          IMID = (KINLO(KINLA-1)+4) / 2
          IKINLO = KINLO(KINLA-1)
        ENDIF
        !
        DO JI = 1,IMID
          KMASK(IOF1+JI) = IOF2 + IMID + JI - 2
        ENDDO
        DO JI = IMID+1,IKINLO+4
          KMASK(IOF1+JI) = IOF2 + 1 + 2 + JI - (IMID + 1)
        ENDDO
        !
        ZARIN(IOF1+1:IOF1+IMID,JT) = ZVECT*ZARIN(IOF2+1+IMID-2:IOF2+2*IMID-2,JT)
        ZARIN(IOF1+IMID+1:IOF1+IKINLO+4,JT) = ZVECT*ZARIN(IOF2+1+2:IOF2+IKINLO+4-IMID+2,JT)
        IF (LDLSM) THEN
          ILSMIN(IOF1+1:IOF1+IMID,JT) = ILSMIN(IOF2+1+IMID-2:IOF2+2*IMID-2,JT)
          ILSMIN(IOF1+IMID+1:IOF1+IKINLO+4,JT) = ILSMIN(IOF2+1+2:IOF2+IKINLO+4-IMID+2,JT)
        END IF
        !
      END IF
      !
    ENDDO
    !
  ENDDO
  !
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(IBIGSIZE,KIND(IBIGSIZE)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  IF (NRANK/=NPIO) ALLOCATE(KMASK(IBIGSIZE))
  CALL MPI_BCAST(KMASK,SIZE(KMASK)*KIND(KMASK)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
IF (NRANK==NPIO .AND. (OGLOBLON.OR.OGLOBN.OR.OGLOBS)) THEN
  !
  ALLOCATE(PARIN0_OUT(SIZE(ZARIN,1),SIZE(ZARIN,2)))
  PARIN0_OUT = ZARIN
  !
ELSE
  !
  ALLOCATE(PARIN0_OUT(0,0))
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_1',1,ZHOOK_HANDLE)

IF (NRANK/=NPIO) THEN
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_3',0,ZHOOK_HANDLE)
  IEXT(1) = MINVAL(KP)
  IEXT(2) = MAXVAL(KP)
  IDX_I = IDX_I + 1
#ifdef SFX_MPI
  CALL MPI_SEND(IEXT,2*KIND(IEXT)/4,MPI_INTEGER,NPIO,IDX_I,NCOMM,INFOMPI)
#endif
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_3',1,ZHOOK_HANDLE)
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_4',0,ZHOOK_HANDLE)
    ISIZE = IEXT(2)-IEXT(1)+1
    ALLOCATE(ZDARIN(ISIZE,INL))
    ALLOCATE(IDLS(ISIZE,INL))
    IDX_I = IDX_I + 1
#ifdef SFX_MPI
  CALL MPI_RECV(ZDARIN,SIZE(ZDARIN)*KIND(ZDARIN)/4,MPI_REAL,NPIO,IDX_I,NCOMM,ISTATUS,INFOMPI)
#endif
  IDX_I = IDX_I + 1
#ifdef SFX_MPI
  CALL MPI_RECV(IDLS,SIZE(IDLS)*KIND(IDLS)/4,MPI_INTEGER,NPIO,IDX_I,NCOMM,ISTATUS,INFOMPI)
#endif
  IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_4',1,ZHOOK_HANDLE)
ELSE
#ifdef SFX_MPI
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(J,IEXT,ISIZE,ZDARIN,IDLS,JT,JL,ISTATUS,INFOMPI,ZHOOK_HANDLE_OMP)
#endif
  DO J=0,NPROC-1
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_2',0,ZHOOK_HANDLE_OMP)
    IF (J/=NPIO) THEN
#ifdef SFX_MPI
      CALL MPI_RECV(IEXT,2*KIND(IEXT)/4,MPI_INTEGER,J,IDX_I+1,NCOMM,ISTATUS,INFOMPI)
#endif
    ELSE
      IEXT(1) = MINVAL(KP)
      IEXT(2) = MAXVAL(KP)
    ENDIF
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_2',1,ZHOOK_HANDLE_OMP)
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_30',0,ZHOOK_HANDLE_OMP)
    ISIZE = IEXT(2)-IEXT(1)+1
    ALLOCATE(ZDARIN(ISIZE,INL))
    ALLOCATE(IDLS(ISIZE,INL))
    DO JT = 1,INL
      DO JL = IEXT(1),IEXT(2)
        ZDARIN(JL-IEXT(1)+1,JT) = ZARIN(JL,JT)
        IDLS(JL-IEXT(1)+1,JT) = ILSMIN(JL,JT)
      ENDDO
    ENDDO
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_30',1,ZHOOK_HANDLE_OMP)
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_40',0,ZHOOK_HANDLE_OMP)
    IF (J/=NPIO) THEN
#ifdef SFX_MPI
      CALL MPI_SEND(ZDARIN,SIZE(ZDARIN)*KIND(ZDARIN)/4,MPI_REAL,J,IDX_I+2,NCOMM,INFOMPI)
      CALL MPI_SEND(IDLS,SIZE(IDLS)*KIND(IDLS)/4,MPI_INTEGER,J,IDX_I+3,NCOMM,INFOMPI)
#endif
    ELSE
      DO JT = 1,INL
        DO JL2 = 1,12
          DO JL = 1,SIZE(KP,1)
            ID1 = KP(JL,JL2) - IEXT(1) + 1
            PARIN_OUT(JL,JT,JL2) = ZDARIN(ID1,JT)
            KLSMIN_OUT(JL,JT,JL2) = IDLS(ID1,JT)
          ENDDO
        ENDDO
      ENDDO
    ENDIF
    DEALLOCATE(ZDARIN,IDLS)
    IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_40',1,ZHOOK_HANDLE_OMP)
  ENDDO
#ifdef SFX_MPI
!$OMP END PARALLEL DO
#endif
  IDX_I = IDX_I+3
ENDIF
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_5',0,ZHOOK_HANDLE)
IF (NRANK/=NPIO) THEN
DO JT = 1,INL
  DO JL2 = 1,12
    DO JL = 1,SIZE(KP,1)
      ID1 = KP(JL,JL2) - IEXT(1) + 1
      PARIN_OUT(JL,JT,JL2) = ZDARIN(ID1,JT)
      KLSMIN_OUT(JL,JT,JL2) = IDLS(ID1,JT)
    ENDDO
  ENDDO
ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_GRIDIN_5',1,ZHOOK_HANDLE)
!
END SUBROUTINE HORIBL_SURF_GRIDIN
END MODULE MODI_HORIBL_SURF_GRIDIN
