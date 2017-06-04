!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE HORIBL_SURF_EXTRAP(PILA1,PILO1,PILA2,PILO2,KINLA,KINLO,KILEN,PARIN, &
                                  KOLEN,KP,PXOUT,PYOUT,PAROUT,KLUOUT,OINTERP,PILATARRAY  )  
!   ###########################################################################
!
!!****  *HORIBL_SURF_EXTRAP* - horitontal bilinear interpolation
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
USE MODI_HOR_EXTRAPOL_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1. Declaration of arguments
!         
REAL,                      INTENT(IN)  :: PILA1   ! Lat. (y) of first input point KDGSA
REAL,                      INTENT(IN)  :: PILO1   ! Lon. (x) of first input point
REAL,                      INTENT(IN)  :: PILA2   ! Lat. (y) of last input point KDGEN
REAL,                      INTENT(IN)  :: PILO2   ! Lon. (x) of last input point
INTEGER,                   INTENT(IN)  :: KINLA   ! Number of parallels
INTEGER, DIMENSION(:), INTENT(IN)  :: KINLO   ! Number of point along a parallel
INTEGER,                   INTENT(IN)  :: KILEN   ! size of input arrays
REAL,    DIMENSION(:,:), INTENT(IN)  :: PARIN   ! input array
INTEGER,                   INTENT(IN)  :: KOLEN   ! size of output array
INTEGER, DIMENSION(:,:), INTENT(IN) :: KP
REAL,    DIMENSION(:), INTENT(IN)  :: PXOUT   ! X (lon.) of output points
REAL,    DIMENSION(:), INTENT(IN)  :: PYOUT   ! Y (lat.) of output points
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PAROUT  ! output array
LOGICAL, DIMENSION(:), INTENT(IN)  :: OINTERP ! .true. where physical value is needed
INTEGER,                   INTENT(IN)  :: KLUOUT  ! output listing logical unit
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PILATARRAY
!
!*      0.2. Declaration of local variables
!          
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif  
 ! Variables implied in the extension procedure
INTEGER :: ICOUNT, JL, INL
 ! Loop counters
INTEGER :: INFOMPI, J, ICPT
!------------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!------------------------------------------------------------------------------
!
!*     1. DETERMINATION  of the latitude of the poles (depending of the latitude
!         -------------                                 of the first data point)
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_2',0,ZHOOK_HANDLE)

INL = SIZE(PAROUT,2)

!* no data point
IF (NRANK==NPIO) ICOUNT = COUNT(PARIN(:,:)/=XUNDEF)
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(ICOUNT,KIND(ICOUNT)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
IF (ICOUNT==0 .AND. LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_2',1,ZHOOK_HANDLE)
IF (ICOUNT==0) RETURN
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_2',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_3',0,ZHOOK_HANDLE)
!
DO JL=1,INL
WRITE(KLUOUT,*) ' Remaining horizontal extrapolations'
WRITE(KLUOUT,*) ' Total number of input data     : ',ICOUNT
WRITE(KLUOUT,*) ' Number of points to interpolate: ',COUNT(PAROUT(:,JL)==XUNDEF .AND. OINTERP(:))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_3',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_4',0,ZHOOK_HANDLE)

!* input grid coordinates
!
!
IF (PRESENT(PILATARRAY)) THEN
  CALL HOR_EXTRAPOL_SURF(KLUOUT,'LALO',KILEN,PILA1,PILA2,PILO1,PILO2,KINLA,KINLO,&
                         KP,PARIN,PYOUT,PXOUT,PAROUT,OINTERP,PILATARRAY)        
ELSE
  CALL HOR_EXTRAPOL_SURF(KLUOUT,'LALO',KILEN,PILA1,PILA2,PILO1,PILO2,KINLA,KINLO,&
                         KP,PARIN,PYOUT,PXOUT,PAROUT,OINTERP)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('HORIBL_SURF_EXTRAP_4',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!
END SUBROUTINE HORIBL_SURF_EXTRAP
