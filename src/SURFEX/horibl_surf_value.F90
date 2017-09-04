!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE HORIBL_SURF_VALUE(KILEN,KOLEN,PAROUT,OINTERP,PARIN,KLSMIN,&
                                 POLO,POLA,PLA,PLOP,&
                                 KMASKIN,KLSMOUT )
!   ###########################################################################
!
!!****  *HORIBL_SURF_VALUE* - horitontal bilinear interpolation
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
!!                              extrapolation (tabular PARIN not totaly filled)
!!
!------------------------------------------------------------------------------
!
!
!*      0. DECLARATIONS
!       ---------------
!
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
INTEGER,                   INTENT(IN)  :: KILEN
INTEGER,                   INTENT(IN)  :: KOLEN   ! size of output array
REAL,    DIMENSION(:), INTENT(OUT) :: PAROUT  ! output array
LOGICAL, DIMENSION(:), INTENT(IN)  :: OINTERP ! .true. where physical value is needed
REAL, DIMENSION(:,:), INTENT(IN) :: PARIN
INTEGER, DIMENSION(:,:), INTENT(IN) :: KLSMIN
REAL, DIMENSION(:), INTENT(IN) :: POLO, POLA
REAL, DIMENSION(:,:), INTENT(IN) :: PLA
REAL, DIMENSION(:,:), INTENT(IN) :: PLOP
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL  :: KMASKIN  ! input land/sea mask
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL  :: KLSMOUT ! output land/sea mask
!
!*      0.2. Declaration of local variables
!
! Weights of the latitudes and of the points
REAL, DIMENSION(4) :: ZWV
REAL, DIMENSION(12) :: ZWP
  ! Land/sea mask coefficient for each point : 0 -> point not taken in account,
  !                                            1 -> point taken in account
REAL, DIMENSION(12) :: ZLSMP
REAL, DIMENSION(4) :: ZLSMV
REAL :: ZLSMTOT
 ! Variables implied in the extension procedure
LOGICAL :: LDLSM     ! Specify if land/sea mask is present or not
 ! Loop counters
INTEGER :: JL, JI    ! Dummy counter
!
!------------------------------------------------------------------------------
REAL, DIMENSION(3) :: ZP
REAL :: ZMAX, ZT     ! Max of 12 surrounding values
REAL :: ZMIN      ! Min of 12 surrounding values
INTEGER :: JL2    ! Dummy counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!------------------------------------------------------------------------------
!
!*     1. DETERMINATION  of the latitude of the poles (depending of the latitude
!         -------------                                 of the first data point)
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_1',0,ZHOOK_HANDLE)
!
LDLSM = .FALSE.
IF (PRESENT(KMASKIN) .AND. PRESENT(KLSMOUT)) LDLSM = .TRUE.
!
!------------------------------------------------------------------------------
!
!*     3.   LOOP OVER ALL THE POINTS OF THE OUTPUT GRID
!           -------------------------------------------
!
PAROUT(:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_1',1,ZHOOK_HANDLE)
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JL,ZLSMP,ZLSMV,ZLSMTOT,ZP,ZT,&
!$OMP   ZWV,ZWP,JI,ZMIN,ZMAX,JL2)
DO JL = 1, KOLEN
!
  IF (.NOT. OINTERP(JL)) CYCLE
!
!*  3.2 Land / Sea mask
!
  ZLSMP(:) = 1.
  ZLSMV(:) = 1.
  !
  IF (LDLSM) THEN
    !
    DO JI = 1,12
      IF (KLSMIN(JL,JI).NE.KLSMOUT(JL)) ZLSMP(JI) = 0.
    ENDDO
    !
    ZLSMV(1) = ZLSMP(1) + ZLSMP(2)
    ZLSMV(2) = ZLSMP(3) + ZLSMP(4) + ZLSMP(5) + ZLSMP(6)
    ZLSMV(3) = ZLSMP(7) + ZLSMP(8) + ZLSMP(9) + ZLSMP(10)
    ZLSMV(4) = ZLSMP(11) + ZLSMP(12)
    !
    ZLSMV(:) = MIN(ZLSMV(:),1.)
    ZLSMTOT = MIN(SUM(ZLSMV),1.)
    !
    IF (ZLSMV(1) < 1.E-3) ZLSMP  (1:2) = 1.
    IF (ZLSMV(2) < 1.E-3) ZLSMP  (3:6) = 1.
    IF (ZLSMV(3) < 1.E-3) ZLSMP (7:10) = 1.
    IF (ZLSMV(4) < 1.E-3) ZLSMP(11:12) = 1.
    IF (ZLSMTOT < 1.E-3) ZLSMV(:) = 1.
    !
  ENDIF
!
!*  3.3 Weight of points
!
  ZP(1) = PLA(JL,1) - PLA(JL,2)
  ZP(2) = PLA(JL,1) - PLA(JL,3)
  ZP(3) = PLA(JL,2) - PLA(JL,3)
  !
  ZT = POLA(JL) - PLA(JL,1)
  ZWV(1) = ZLSMV(1) * (1.+ZLSMV(2)*ZT/ZP(1)) * (1.+ZLSMV(3)*ZT/ZP(2)) * (1.+ZLSMV(4)*ZT/(PLA(JL,1)-PLA(JL,4)))
  ZT = POLA(JL) - PLA(JL,2)
  ZWV(2) = ZLSMV(2) * (1.-ZLSMV(1)*ZT/ZP(1)) * (1.+ZLSMV(3)*ZT/ZP(3)) * (1.+ZLSMV(4)*ZT/(PLA(JL,2)-PLA(JL,4)))
  ZT = POLA(JL) - PLA(JL,3)
  ZWV(3) = ZLSMV(3) * (1.-ZLSMV(1)*ZT/ZP(2)) * (1.-ZLSMV(2)*ZT/ZP(3)) * (1.+ZLSMV(4)*ZT/(PLA(JL,3)-PLA(JL,4)))
  ZWV(4) = 1. - ZWV(1) - ZWV(2) - ZWV(3)
!
  ! 3.3.1 northern
  ZWP(1)  = ZLSMP(1) * (1.+ZLSMP(2) *(POLO(JL) -PLOP(JL,1))/(PLOP(JL,1) -PLOP(JL,2)))
  ZWP(2)  = 1. - ZWP(1)
  ! 3.3.4. southern
  ZWP(11) = ZLSMP(11)* (1.+ZLSMP(12)*(POLO(JL)-PLOP(JL,11))/(PLOP(JL,11)-PLOP(JL,12)))
  ZWP(12) = 1. - ZWP(11)

  ! 3.3.2. north
  ZP(1) = PLOP(JL,3) - PLOP(JL,4)
  ZP(2) = PLOP(JL,3) - PLOP(JL,5)
  ZP(3) = PLOP(JL,4) - PLOP(JL,5)
  !
  ZT = POLO(JL) - PLOP(JL,3)
  ZWP(3) = ZLSMP(3) * (1.+ZLSMP(4)*ZT/ZP(1)) * (1.+ZLSMP(5)*ZT/ZP(2)) * (1.+ZLSMP(6)*ZT/(PLOP(JL,3)-PLOP(JL,6)))
  ZT = POLO(JL) - PLOP(JL,4)
  ZWP(4) = ZLSMP(4) * (1.-ZLSMP(3)*ZT/ZP(1)) * (1.+ZLSMP(5)*ZT/ZP(3)) * (1.+ZLSMP(6)*ZT/(PLOP(JL,4)-PLOP(JL,6)))
  ZT = POLO(JL) - PLOP(JL,5)
  ZWP(5) = ZLSMP(5) * (1.-ZLSMP(3)*ZT/ZP(2)) * (1.-ZLSMP(4)*ZT/ZP(3)) * (1.+ZLSMP(6)*ZT/(PLOP(JL,5)-PLOP(JL,6)))
  ZWP(6) = 1. - ZWP(3) - ZWP(4) - ZWP(5)
!
  ! 3.3.3. south
  ZP(1) = PLOP(JL,7) - PLOP(JL,8)
  ZP(2) = PLOP(JL,7) - PLOP(JL,9)
  ZP(3) = PLOP(JL,8) - PLOP(JL,9)
  !
  ZT = POLO(JL) - PLOP(JL,7)
  ZWP(7)  = ZLSMP(7) * (1.+ZLSMP(8)*ZT/ZP(1)) * (1.+ZLSMP(9)*ZT/ZP(2)) * (1.+ZLSMP(10)*ZT/(PLOP(JL,7)-PLOP(JL,10)))
  ZT = POLO(JL) - PLOP(JL,8)
  ZWP(8)  = ZLSMP(8) * (1.-ZLSMP(7)*ZT/ZP(1)) * (1.+ZLSMP(9)*ZT/ZP(3)) * (1.+ZLSMP(10)*ZT/(PLOP(JL,8)-PLOP(JL,10)))
  ZT = POLO(JL) - PLOP(JL,9)
  ZWP(9)  = ZLSMP(9) * (1.-ZLSMP(7)*ZT/ZP(2)) * (1.-ZLSMP(8)*ZT/ZP(3)) * (1.+ZLSMP(10)*ZT/(PLOP(JL,9)-PLOP(JL,10)))
  ZWP(10) = 1. - ZWP(7) - ZWP(8) - ZWP(9)
!
! In order to exclude undef values from computation of PAROUT,
! weights w2, w6, w10, w12 and wss which can be numerically very low
! because they are residual, are set to 0
!
  IF (ABS(ZWP(2)) < 1.E-10) ZWP(2) =0.
  IF (ABS(ZWP(6)) < 1.E-10) ZWP(6) =0.
  IF (ABS(ZWP(10))< 1.E-10) ZWP(10)=0.
  IF (ABS(ZWP(12))< 1.E-10) ZWP(12)=0.
  IF (ABS(ZWV(4)) < 1.E-10) ZWV(4) =0.
!
      ! 3.3.5. longitude weight x latitude weight
  ZWP(1:2) = ZWP(1:2) * ZWV(1)
  ZWP(3:6) = ZWP(3:6) * ZWV(2)
  ZWP(7:10) = ZWP(7:10) * ZWV(3)
  ZWP(11:12) = ZWP(11:12) * ZWV(4)
!
  PAROUT(JL) = 0
  DO JI = 1,12
    PAROUT(JL) = PAROUT(JL) + ZWP(JI) * PARIN(JL,JI)
  ENDDO
!
! For surface fields, the interpoalted value is bounded
! by the min max values of the initial field

  IF (PRESENT(KMASKIN)) THEN

    ZMIN=XUNDEF
    ZMAX=XUNDEF

    DO JL2=1,12
      IF (PARIN(JL,JL2)==XUNDEF) CYCLE

      IF ((ZMAX==XUNDEF)) THEN
        ZMAX=PARIN(JL,JL2)
        ZMIN=PARIN(JL,JL2)
      ELSE
        ZMAX=MAX(ZMAX,PARIN(JL,JL2))
        ZMIN=MIN(ZMIN,PARIN(JL,JL2))
      ENDIF

    END DO

    PAROUT(JL) = MAX(MIN(PAROUT(JL),ZMAX),ZMIN)

  ENDIF

END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_3',0,ZHOOK_HANDLE)
WHERE(ABS(PAROUT-XUNDEF)<1.E-6) PAROUT=XUNDEF
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_VALUE_3',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!
END SUBROUTINE HORIBL_SURF_VALUE
