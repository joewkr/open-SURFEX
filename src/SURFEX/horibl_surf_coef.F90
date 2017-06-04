!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE HORIBL_SURF_COEF(KOLEN,OINTERP,OGLOBLON,PILO1,PILO2,POLO,&
                                KO,KINLO,KP,PLOP  )  
!   ###########################################################################
!
!!****  *HORIBL_SURF_COEF* - horitontal bilinear interpolation
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
!
!*      0. DECLARATIONS
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
INTEGER,                   INTENT(IN)  :: KOLEN   ! size of output array
LOGICAL, DIMENSION(:), INTENT(IN)  :: OINTERP ! .true. where physical value is needed
!
LOGICAL, INTENT(IN) :: OGLOBLON  ! True if the map is circular
REAL, INTENT(IN) :: PILO1     ! Longitude of the first data point
REAL, INTENT(IN) :: PILO2     ! Longitude of the last data point
REAL, DIMENSION(:), INTENT(IN) :: POLO
INTEGER, DIMENSION(:,:), INTENT(IN) :: KO
!
INTEGER, DIMENSION(:), INTENT(IN) :: KINLO     ! Extended KINLO
!
! Posiiton in the array of the twelwe surrounding points
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KP  
  ! Latitudes and longitudes of the surrounding points
REAL, DIMENSION(:,:), INTENT(OUT) :: PLOP  
!
!*      0.2. Declaration of local variables
!            
 ! Variables used to perform the interpolation
REAL                               :: ZIDLO    ! Delta longitude
INTEGER, DIMENSION(:), ALLOCATABLE :: IOFS     ! Offset of each parallel in the array
!
INTEGER :: IINLA
INTEGER :: JL, IS1, IS2, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
!*     1. DETERMINATION  of the latitude of the poles (depending of the latitude
!         -------------                                 of the first data point)
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_COEF',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*     3.   LOOP OVER ALL THE POINTS OF THE OUTPUT GRID
!           -------------------------------------------
!
IINLA = SIZE(KINLO)
ALLOCATE (IOFS(IINLA))
IOFS(1) = 1
IF (OGLOBLON) IOFS(1)=IOFS(1)+2
DO JL = 2,IINLA
  IOFS(JL) = IOFS(JL-1) + KINLO(JL-1)
  IF (OGLOBLON) IOFS(JL) = IOFS(JL) + 4
END DO
!
DO JL = 1, KOLEN
  !
  IF (.NOT. OINTERP(JL)) CYCLE
  !
  ! 3.1.2. northern
  ZIDLO = (PILO2 - PILO1) / (KINLO(KO(JL,1)))
  KP(JL,1) = INT((POLO(JL) - PILO1) / ZIDLO)
  KP(JL,2) = KP(JL,1) + 1
  IF (.NOT.OGLOBLON) KP(JL,2) = MIN(KINLO(KO(JL,1))-1, KP(JL,2))
  PLOP(JL,1) = PILO1 + KP(JL,1) * ZIDLO
  PLOP(JL,2) = PLOP(JL,1) + ZIDLO
!
      ! 3.1.3. north
  ZIDLO = (PILO2 - PILO1) / (KINLO(KO(JL,2) ))
  KP(JL,4)   = INT((POLO(JL) - PILO1) / ZIDLO)
  KP(JL,3) = KP(JL,4) - 1
  KP(JL,5) = KP(JL,4) + 1
  KP(JL,6) = KP(JL,4) + 2
  IF (.NOT.OGLOBLON) THEN
    KP(JL,3) = MAX(0,KP(JL,3))
    KP(JL,5:6) = MIN(KINLO(KO(JL,2))-1,KP(JL,5:6))
  ENDIF
  PLOP(JL,4) = PILO1 + KP(JL,4) * ZIDLO
  PLOP(JL,3) = PLOP(JL,4) - ZIDLO
  PLOP(JL,5) = PLOP(JL,4) + ZIDLO
  PLOP(JL,6) = PLOP(JL,4) + 2*ZIDLO
!
      ! 3.1.4. south
  ZIDLO = (PILO2 - PILO1) / (KINLO(KO(JL,3) ))
  KP(JL,8) = INT((POLO(JL) - PILO1) / ZIDLO)
  KP(JL,7)  = KP(JL,8) - 1
  KP(JL,9)  = KP(JL,8) + 1
  KP(JL,10) = KP(JL,8) + 2
  IF (.NOT.OGLOBLON) THEN
    KP(JL,7)  = MAX(0,KP(JL,7))
    KP(JL,9:10) = MIN(KINLO(KO(JL,3))-1,KP(JL,9:10))
  ENDIF
  PLOP(JL,8)  = PILO1 + KP(JL,8) * ZIDLO
  PLOP(JL,7)  = PLOP(JL,8) - ZIDLO
  PLOP(JL,9)  = PLOP(JL,8) + ZIDLO
  PLOP(JL,10) = PLOP(JL,8) + 2*ZIDLO
!
      ! 3.1.5. southern
  ZIDLO = (PILO2 - PILO1) / (KINLO(KO(JL,4)))
  KP(JL,11) = INT((POLO(JL) - PILO1) / ZIDLO) 
  KP(JL,12) = KP(JL,11) + 1 
  IF (.NOT.OGLOBLON) KP(JL,12) = MIN(KINLO(KO(JL,4))-1,KP(JL,12))
  PLOP(JL,11) = PILO1 + KP(JL,11) * ZIDLO
  PLOP(JL,12) = PLOP(JL,11) + ZIDLO
!
      ! 3.1.6. check position of points
  IF (OGLOBLON) THEN 
    IS1 = -2
    IS2 = 1
  ELSE
    IS1 = 0
    IS2 = -1
  ENDIF
  !
  IF ((KP(JL,1) <IS1) .OR. (KP(JL,2) >KINLO(KO(JL,1))+IS2) .OR. &
      (KP(JL,3) <IS1) .OR. (KP(JL,6) >KINLO(KO(JL,2))+IS2) .OR. &
      (KP(JL,7) <IS1) .OR. (KP(JL,10)>KINLO(KO(JL,3))+IS2) .OR. &
      (KP(JL,11)<IS1) .OR. (KP(JL,12)>KINLO(KO(JL,4))+IS2)) THEN  
    CALL ABOR1_SFX('HORIBLE_SURF: INPUT DOMAIN SMALLER THAN OUTPUT ONE - LONGITUDE GLOBAL')
  END IF
!
      ! 3.1.7. add parallel offset
  KP(JL,1:2)  = KP(JL,1:2) + IOFS(KO(JL,1))
  KP(JL,3:6)  = KP(JL,3:6) + IOFS(KO(JL,2))
  KP(JL,7:10) = KP(JL,7:10) + IOFS(KO(JL,3))
  KP(JL,11:12)= KP(JL,11:12) + IOFS(KO(JL,4))
!
END DO
!
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_COEF',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!
END SUBROUTINE HORIBL_SURF_COEF
