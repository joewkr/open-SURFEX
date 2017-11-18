!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_HORIBL_SURF_INIT
CONTAINS
    SUBROUTINE HORIBL_SURF_INIT(PILA1,PILO1,PILA2,PILO2,KINLA,KINLO,KOLEN,&
                           PXOUT,PYOUT,OINTERP,OGLOBLON,OGLOBN,OGLOBS,&
                           KO,KINLO_OUT,POLA,POLO,PILO1_OUT,&
                           PILO2_OUT,PLA,PILATARRAY )
!   ###########################################################################
!
!!****  *HORIBL_SURF_INIT* - horitontal bilinear interpolation
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
!!   MODIFICATKONS
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
!
!*      0. DECLARATKONS
!       ---------------
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1. Declaration of arguments
!
REAL,                      INTENT(IN)  :: PILA1   ! Lat. (y) of first input point KDGSA
REAL,                      INTENT(IN)  :: PILO1   ! Lon. (x) of first input point
REAL,                      INTENT(IN)  :: PILA2   ! Lat. (y) of last input point KDGEN
REAL,                      INTENT(IN)  :: PILO2   ! Lon. (x) of last input point
INTEGER,                   INTENT(IN)  :: KINLA   ! Number of parallels
INTEGER, DIMENSION(:), INTENT(IN)  :: KINLO   ! Number of point along a parallel
INTEGER,                   INTENT(IN)  :: KOLEN   ! size of output array
REAL,    DIMENSION(:), INTENT(IN)  :: PXOUT   ! X (lon.) of output points
REAL,    DIMENSION(:), INTENT(IN)  :: PYOUT   ! Y (lat.) of output points
LOGICAL, DIMENSION(:), INTENT(IN)  :: OINTERP ! .true. where physical value is needed
!
LOGICAL, INTENT(OUT)  :: OGLOBLON  ! True if the map is circular
LOGICAL, INTENT(OUT)  :: OGLOBN    ! True if the map has the north pole
LOGICAL, INTENT(OUT)  :: OGLOBS    ! True if the map has the south pole
  ! Number of the surrounding latitudes
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KO
INTEGER, DIMENSION(:), INTENT(OUT) :: KINLO_OUT     ! Extended KINLO
REAL, INTENT(OUT) :: PILO1_OUT     ! Longitude of the first data point
REAL, INTENT(OUT) :: PILO2_OUT     ! Longitude of the last data point
! Variables used to perform the interpolation
REAL, DIMENSION(:), INTENT(OUT) :: POLA     ! Latitude of the output point
REAL, DIMENSION(:), INTENT(OUT) :: POLO     ! Longitude of the output point
  ! Latitudes and longitudes of the surrounding points
REAL, DIMENSION(:,:), INTENT(OUT) :: PLA
REAL,    DIMENSION(:), INTENT(IN), OPTIONAL  :: PILATARRAY! latitudes array
!
!*      0.2. Declaration of local variables
!
REAL, DIMENSION(:), ALLOCATABLE    :: ZIDLAT   ! Deltai latitude
REAL                               :: ZIDLA    ! Delta latitude
REAL                               :: ZSOUTHPOLE! south pole latitude (-90 or  90)
REAL                               :: ZNORTHPOLE! north pole latitude ( 90 or -90)
!
! Variables implied in the extension procedure
!
INTEGER                            :: IOFFSET   ! Offset in map
INTEGER                            :: IINLA     ! Number of parallel
 ! Loop counters
INTEGER                            :: JOPOS     ! Output position
INTEGER                            :: JL, JL2   ! Dummy counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
!*     1. DETERMINATKON  of the latitude of the poles (depending of the latitude
!         -------------                                 of the first data point)
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_INIT',0,ZHOOK_HANDLE)
!
IF (PILA1>0.) THEN
  ZSOUTHPOLE= 90.
  ZNORTHPOLE=-90.
ELSE
  ZSOUTHPOLE=-90.
  ZNORTHPOLE= 90.
END IF
!
!------------------------------------------------------------------------------
!
!*     2. EXTEND DATA GRID
!         ----------------
!
!*    2.1 Alias input data
!
PILO1_OUT = PILO1
PILO2_OUT = PILO2
!
!*   2.2 Center input domain in order to have Lo1 < Lo 2
!
IF (PILO2_OUT < 0.)    PILO2_OUT = PILO2_OUT + 360.
IF (PILO1_OUT < 0.)    PILO1_OUT = PILO1_OUT + 360.
IF (PILO2_OUT < PILO1_OUT) PILO1_OUT = PILO1_OUT - 360.
!
!*   2.3 Extend one point (needed for reduced grids)
!
! Longitude coordinate of points are found by :
!                      i
!  Lon(i) = Lon1 + ------------- . (Lon2 - Lon1)
!                   Npts(Lat)-1
! Where i goes from 0 to Npts(Lat)-1. The result of this is that the last point of
! each parallel is located at Lon2. This is not the case for reduced grid where the
! position of the last point depends upon the number of points of the parallel. For
! reduced grid, the right formula to use is the following :
!                       i
!  Lon(i) = Lon1 + ----------- . (Lon2' - Lon1)
!                   Npts(Lat)
! Where Lon2' = Lon1 + 2.PI.
!
!                                              Lon2 - Lon1
! This can be generalized with Lon2' = Lon2 + -------------
!                                              Nptsmax - 1
!
JOPOS = MAXVAL(KINLO(1:KINLA))
PILO2_OUT = PILO1_OUT + (PILO2_OUT - PILO1_OUT) * JOPOS / (JOPOS - 1.)
!
!* 2.4 Test if the input is global or partially global
!
! Note that we must have a global map to make extension around the poles
OGLOBN   = .FALSE.
OGLOBS   = .FALSE.
OGLOBLON = .FALSE.
IF (PILO2_OUT-360.>PILO1_OUT-1.E-3) OGLOBLON = .TRUE.
ZIDLA = (PILA2 - PILA1) / (KINLA - 1)
IF (PRESENT(PILATARRAY)) THEN
  ALLOCATE(ZIDLAT(KINLA+1))
  ZIDLAT(KINLA+1)=0.
  DO JL=2,KINLA
    ZIDLAT(JL) = PILATARRAY(JL)-PILATARRAY(JL-1)
  END DO
  ZIDLAT(1)=ZIDLAT(2)
ENDIF
IF ((PILA1-ZIDLA>= 90.) .OR. (PILA1-ZIDLA<=-90.)) OGLOBS=OGLOBLON
IF ((PILA2+ZIDLA>= 90.) .OR. (PILA2+ZIDLA<=-90.)) OGLOBN=OGLOBLON
! Aladin case (input PILA2, PILO2 are in meters) no extension
IF ( PILA2 > 100. ) THEN
  OGLOBN   = .FALSE.
  OGLOBS   = .FALSE.
  OGLOBLON = .FALSE.
END IF
!
!
!*  2.7  Compute the resulting parameters of the map
!
IINLA = KINLA
IF (OGLOBS) IINLA = IINLA + 2
IF (OGLOBN) IINLA = IINLA + 2
!
IOFFSET = 0
IF (OGLOBS) THEN
  KINLO_OUT(IOFFSET+1) = KINLO(2)
  KINLO_OUT(IOFFSET+2) = KINLO(1)
  IOFFSET = IOFFSET + 2
END IF
KINLO_OUT(IOFFSET+1:IOFFSET+KINLA) = KINLO(1:KINLA)
IOFFSET = IOFFSET + KINLA
IF (OGLOBN) THEN
  KINLO_OUT(IOFFSET+1) = KINLO(KINLA)
  KINLO_OUT(IOFFSET+2) = KINLO(KINLA-1)
  IOFFSET = IOFFSET + 2
END IF
!
!------------------------------------------------------------------------------
!
!*     3.   LOOP OVER ALL THE POINTS OF THE OUTPUT GRID
!           -------------------------------------------
!
POLA(:) = 0.
POLO(:) = 0.
!
DO JL = 1, KOLEN
  !
  IF (.NOT. OINTERP(JL)) CYCLE
  !
  POLA(JL)  = PYOUT(JL)
  !
  POLO(JL)  = PXOUT(JL)
  IF (POLO(JL) < PILO1_OUT) POLO(JL) = POLO(JL) + 360.
  IF (POLO(JL) > PILO2_OUT) POLO(JL) = POLO(JL) - 360.
  !
  ! 3.1.1. find positions of latitudes
  IF (PRESENT(PILATARRAY)) THEN
    !
    DO JL2 = 1,KINLA
      IF( POLA(JL)>=PILATARRAY(JL2) .AND.&
          POLA(JL)<PILATARRAY(JL2)+ZIDLAT(JL2+1) ) THEN
        KO(JL,3) = JL2
        EXIT
      ENDIF
    ENDDO
    PLA(JL,3) = PILATARRAY(KO(JL,3))
    !
  ELSE
    !
    KO(JL,3)  = NINT( (POLA(JL)-PILA1)/ZIDLA -0.5) ! because of the zero
    IF ( KO(JL,3)<-1) CALL ABOR1_SFX('HORIBLE_SURF_INIT: INPUT DOMAIN SMALLER THAN OUTPUT ONE - LATITUDE')
    !
    PLA(JL,3) = PILA1 + KO(JL,3) * ZIDLA
    !
    KO (JL,3) = KO(JL,3) + 1
    !
  ENDIF
  !
  IF (OGLOBS) KO(JL,3) = KO(JL,3) + 2
  !
  KO(JL,1) = KO(JL,3) + 2
  KO(JL,2) = KO(JL,3) + 1
  KO(JL,4) = KO(JL,3) - 1
  !
  IF (.NOT.OGLOBS) THEN
    KO(JL,1:2) = MIN(KO(JL,1:2),IINLA)
    KO(JL,3:4) = MAX(KO(JL,3:4),1)
  ENDIF
  !
  IF (PRESENT(PILATARRAY)) THEN
    !
    IF (KO(JL,3)==1) THEN
      PLA(JL,4) = PILATARRAY(1) - ZIDLAT(1)/2.
    ELSE
      PLA(JL,4) = PILATARRAY(KO(JL,3)-1)
    ENDIF
    IF (KO(JL,3)==IINLA) THEN
      PLA(JL,2) = PILATARRAY(IINLA) + ZIDLAT(IINLA)/2.
    ELSE
      PLA(JL,2) = PILATARRAY(KO(JL,3)+1)
    ENDIF
    IF (KO(JL,2)==IINLA) THEN
      PLA(JL,1) = PILATARRAY(IINLA) + ZIDLAT(IINLA)
    ELSE
      PLA(JL,1) = PILATARRAY(KO(JL,2)+1)
    ENDIF
    !
  ELSE
    !
    PLA(JL,1) = PLA(JL,3) + 2*ZIDLA
    PLA(JL,2) = PLA(JL,3) + ZIDLA
    PLA(JL,4) = PLA(JL,3) - ZIDLA
    !
  ENDIF
  !
  ! extra latitudes are computed symetrically compared to the poles
  !
  IF (OGLOBS) THEN
    IF (KO(JL,3)==2) THEN
      PLA(JL,4) = 2. * ZSOUTHPOLE - PLA(JL,1)
      PLA(JL,3) = 2. * ZSOUTHPOLE - PLA(JL,2)
    ELSEIF (KO(JL,3)==3) THEN
      PLA(JL,4) = 2. * ZSOUTHPOLE - PLA(JL,3)
    END IF
  ENDIF
  IF (OGLOBN) THEN
    IF (KO(JL,3)==IINLA-2) THEN
      PLA(JL,1) = 2. * ZNORTHPOLE - PLA(JL,4)
      PLA(JL,2) = 2. * ZNORTHPOLE - PLA(JL,3)
    ELSEIF (KO(JL,3)==IINLA-3) THEN
     PLA(JL,1) = 2. * ZNORTHPOLE - PLA(JL,2)
    END IF
  ENDIF
  !
  IF ((KO(JL,4)<1).OR.ANY((KO(JL,:)>IINLA))) THEN
    CALL ABOR1_SFX('HORIBLE_SURF_INIT: INPUT DOMAIN SMALLER THAN OUTPUT ONE - LATITUDE')
  END IF
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_INIT',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!
END SUBROUTINE HORIBL_SURF_INIT
END MODULE MODI_HORIBL_SURF_INIT
