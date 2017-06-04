!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_SPLINES(KLUOUT,KCODE,PX,PY,PFIELD,KREP)
!     #########################################################
!
!!**** *INTERPOL_SPLINES* interpolates with spline f77 programs a 2D field
!!                           from all grid points valid values
!!
!!    PURPOSE
!!    -------
!!
!!    The points are all on only one grid (defined with the coordinates
!!    of all the points). The code to apply for each point is:
!!
!!    KCODE>0 : data point (with field valid for interpolation)
!!    KCODE=-1: point to ignore
!!    KCODE=0 : point to interpolate
!!
!!
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    19/03/95
!!    Modification
!!     25/05/96   (V Masson)  W and IW defined in MODD_SPLINESWORK  
!!     04/07/96   (V Masson)  call with 1d dummy arguments
!!     30/10/96   (V Masson)  add deallocations
!!     05/08/97   (V Masson)  output listing as dummy argument
!!     17/09/97   (V Masson)  routine performs ONLY the splines interpolation
!!     19/12/97   (V Masson)  possibility to use the same splines for collocated
!!                            fields
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
USE MODE_SPLINES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing
INTEGER,DIMENSION(:),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:),  INTENT(IN)     :: PX       ! x of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PY       ! y of each grid mesh.
REAL,   DIMENSION(:,:),INTENT(INOUT)  :: PFIELD   ! pgd field on grid mesh.
INTEGER,               INTENT(OUT)    :: KREP     ! error flag
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER, PARAMETER      :: ND = 2                !
INTEGER                 :: IOPT     ! 0: splines recomputed
!                                   ! 1: splines deduced from W and IW
INTEGER                 :: JK                   ! loop control
INTEGER                 :: JIN                  ! loop control
INTEGER                 :: JOUT                 ! loop control
INTEGER                 :: IVERB = 10           ! verbosity level
INTEGER                 :: IFIELD               ! number of colocated fields
INTEGER, PARAMETER      :: IDATAMAX = 2000      ! maximum number of data per blok
!                                               ! WARNING, if IDATAMAX is modified
!                                               ! MODIFY nmax in all splines.f routines
INTEGER, PARAMETER      :: ISDMAX = 20          ! maximum number of subdomain in one direction
!                                               ! WARNING, if ISDMAX is modified
!                                               ! MODIFY ndmax in all splines.f routines
INTEGER, PARAMETER      :: IMMAX = 3            ! maximum number of monomes (order 2)
INTEGER                 :: IDATA                ! total number of data
REAL, DIMENSION(2,ND)             :: ZXDOM     ! coordinates of the limits of the domain
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZXDATA    ! coordinates of the available data
REAL, DIMENSION(:),   ALLOCATABLE  :: ZDATA     ! available data
INTEGER,DIMENSION(ISDMAX,ISDMAX)   :: NDOMDATA  ! number of data in a subdomain

INTEGER                            :: IOUT      ! total number of points to interpolate
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZXOUT     ! coordinates of these points
REAL, DIMENSION(:),   ALLOCATABLE  :: ZVALOUT   ! values of these points
REAL                               :: ZMINVAL   ! minimum value of the field
REAL                               :: ZMAXVAL   ! maximum value of the field
!
!*    0.3    Declaration for f77 routines
!            ----------------------------
!
INTEGER, PARAMETER    :: IORDER = 2                 ! order of the spline
INTEGER, PARAMETER    :: IM = (IORDER*(IORDER+1))/2 ! number of monomes
INTEGER               :: ISDI,ISDJ                  ! number of subdomains in each
!                                                   ! direction for spline computation
INTEGER               :: IIORDER
INTEGER               :: IIM
INTEGER               :: JSDI,JSDJ                  ! loop controls on subdomains
INTEGER               :: IINTER                     ! inverse ratio of the intersection of
!                                                   ! 2 subdomains over one of the subdomain
REAL, DIMENSION(ND,ND)  :: ZG                         ! matrix of correspondance between
!                                                   ! x and y units
REAL                  :: ZP, ZS2
!
REAL,    DIMENSION(:,:,:), ALLOCATABLE :: ZC        ! coefficients of the spline
REAL,    DIMENSION(IDATAMAX+3*IMMAX)   :: ZWE       ! work array
!
!INTEGER, DIMENSION(2,IM)   :: IW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.     Miscellaneous Initializations
!            -----------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_SPLINES',0,ZHOOK_HANDLE)
IFIELD=SIZE(PFIELD,2)
!
!IW = 0
IIORDER=IORDER
IIM=IM
!-------------------------------------------------------------------------------
!
!*    2.     Splines initializations
!            -----------------------
!
!
!*    2.1    Coordinates of input data points
!            --------------------------------
!
IDATA=COUNT(KCODE>0)
ALLOCATE(ZXDATA(ND,IDATA))
ALLOCATE(ZDATA(IDATA))
!
ZXDATA(1,:)=PACK(PX(:),MASK=(KCODE(:)>0))
ZXDATA(2,:)=PACK(PY(:),MASK=(KCODE(:)>0))
!
!*    2.2    Coordinates of domain limits
!            ----------------------------
!
ZXDOM(1,1)=MINVAL(PX(:),MASK=KCODE(:)>=0)
ZXDOM(2,1)=MAXVAL(PX(:),MASK=KCODE(:)>=0)
ZXDOM(1,2)=MINVAL(PY(:),MASK=KCODE(:)>=0)
ZXDOM(2,2)=MAXVAL(PY(:),MASK=KCODE(:)>=0)
!
!*    2.3    Coordinates of output interpolated points
!            -----------------------------------------
!
IOUT=COUNT(KCODE==0)
ALLOCATE(ZXOUT(ND,IOUT))
ALLOCATE(ZVALOUT(IOUT))
!
ZXOUT(1,:)=PACK(PX(:),MASK=(KCODE(:)==0))
ZXOUT(2,:)=PACK(PY(:),MASK=(KCODE(:)==0))
!
!*    2.4    Matrix of unit correspondance
!            -----------------------------
!
ZG(1,1)=1.
ZG(2,2)=1.
ZG(1,2)=0.
ZG(2,1)=0.
!
!-------------------------------------------------------------------------------
!
!*    3.     Subdomains generation
!            ---------------------
!
IINTER=4
!
ISDI=MAX(1,MIN(INT( SQRT ( IDATA/100+1 -1.E-6 )),ISDMAX))
ISDJ=ISDI
!
!
subdomains: DO
  DO JSDI=1,ISDI
    DO JSDJ=1,ISDJ
      NDOMDATA(JSDI,JSDJ)= COUNT( (KCODE(:)>0) .AND.                                          &
         (      ( PX(:) >= ZXDOM(1,1) + (JSDI-1-0.5/(IINTER-1))*(ZXDOM(2,1)-ZXDOM(1,1))/ISDI)   &
         .AND.  ( PX(:) <= ZXDOM(1,1) + (JSDI  +0.5/(IINTER-1))*(ZXDOM(2,1)-ZXDOM(1,1))/ISDI)   &
         .AND.  ( PY(:) >= ZXDOM(1,2) + (JSDJ-1-0.5/(IINTER-1))*(ZXDOM(2,2)-ZXDOM(1,2))/ISDJ)   &
         .AND.  ( PY(:) <= ZXDOM(1,2) + (JSDJ  +0.5/(IINTER-1))*(ZXDOM(2,2)-ZXDOM(1,2))/ISDJ) )            &
                                                           )  
    END DO
  END DO
  IF ( (ISDI<ISDMAX) .AND. (ISDJ<ISDMAX) .AND. ANY(NDOMDATA(1:ISDI,1:ISDJ) > IDATAMAX/2) ) THEN
    ISDI=ISDI+1
    ISDJ=ISDJ+1
    CYCLE subdomains
  END IF
  EXIT subdomains
END DO subdomains
!
IF ( ANY(NDOMDATA(1:ISDI,1:ISDJ) > IDATAMAX) ) THEN
  WRITE(KLUOUT,*) '**********************************************************'
  WRITE(KLUOUT,*) '* ERROR during data interpolation                        *'
  WRITE(KLUOUT,*) '* interpolation by splines routines is impossible        *'
  WRITE(KLUOUT,*) '* please use an input data file with a better resolution *'
  WRITE(KLUOUT,*) '**********************************************************'
  CALL ABOR1_SFX('INTERPOL_SPLINES: ERROR DURING DATA INTERPOLATION BY SPLINES')
END IF
!
WRITE(KLUOUT,*) '----------------------------------'
!
!-------------------------------------------------------------------------------
!
!*    4.     Computation of the spline in the subdomains (f77 routine)
!            -------------------------------------------
!
!*    4.1    Output printing
!            ---------------
!
  IF (IVERB>=7) THEN
    DO JSDI=1,ISDI
      DO JSDJ=1,ISDJ
        WRITE(KLUOUT,*) '  domain sdi=',JSDI,' sdj=',JSDJ,' : ',     &
                           NDOMDATA(JSDI,JSDJ),' data points available'  
      END DO
    END DO
  END IF
!
!*    4.2    Choice between splines computation or deduction
!            -----------------------------------------------
!
  IF (IVERB>=7) WRITE(KLUOUT,*) ' splines computations:'
!
!*    4.3    Allocations
!            -----------
!
  ALLOCATE(ZC( IDATAMAX+IMMAX , ISDI , ISDJ ))
  IOPT=0
!
!*    4.4    Loop on fields
!            --------------
!
  DO JK=1,IFIELD
!
    ZDATA(:)=PACK(PFIELD(:,JK),MASK=(KCODE(:)>0))
!
!*    4.5    special case of uniform input data
!            -----------------------------
!
    ZMINVAL = MINVAL(ZDATA)
    ZMAXVAL = MAXVAL(ZDATA)
!-------------------------------------------------------------------------------
    IF ( ABS(ZMAXVAL - ZMINVAL) <= 1.E-10 * MAX(ABS(ZMAXVAL),ABS(ZMINVAL)) ) THEN
!-------------------------------------------------------------------------------
!
      ZVALOUT(:)=MAXVAL(ZDATA)
!
!-------------------------------------------------------------------------------
    ELSE
!-------------------------------------------------------------------------------
!
!*    4.6    Call for splines coefficients
!            -----------------------------
!
!
      ZP = 0.
      ZS2 = 0.
!
      CALL SPLB2C(IIORDER,IIM,ZXDATA,ZG,ZDATA,ZS2,ZP,0,IOPT,ISDI,ISDJ,IINTER,ZXDOM,ZC,KREP)
!      CALL SPLB2C(IDATA,ZXDATA,ZG,ZDATA,0,0.,ZP,IORDER,IM,IOPT,ISDI,ISDJ,IINTER,ZXDOM, &
!                   ZC,KREP,IW,W) 
!
      IOPT=1
!
      IF (KREP/=0) THEN
        WRITE(KLUOUT,*) 'Routine INTERPOL_SPLINES: error in SPLB2C, KREP=',KREP
        WRITE(KLUOUT,*) 'The field is interpolated from 3 nearest points'
        IF (ALLOCATED(ZXDATA))  DEALLOCATE(ZXDATA)
        IF (ALLOCATED(ZDATA))   DEALLOCATE(ZDATA)
        IF (ALLOCATED(ZC))      DEALLOCATE(ZC)
        IF (ALLOCATED(ZVALOUT)) DEALLOCATE(ZVALOUT)
        IF (ALLOCATED(ZXOUT))   DEALLOCATE(ZXOUT)
        IF (LHOOK) CALL DR_HOOK('INTERPOL_SPLINES',1,ZHOOK_HANDLE)
        RETURN
      END IF
!
!-------------------------------------------------------------------------------
!
!*    5.     Evaluation of the spline (f77 routine)
!            ------------------------
!
      ZVALOUT(:)=XUNDEF
    !
      IF (IVERB>=7) WRITE(KLUOUT,*) ' splines evaluations'
      !CALL SPLB2E(IORDER,IM,ZG,ISDI,ISDJ,ZC,IOUT,ZXOUT,ZVALOUT,ZWE,IW,W)
      CALL SPLB2E(IIORDER,IIM,ISDI,ISDJ,ZG,ZC,ZXOUT,ZVALOUT)
!
!-------------------------------------------------------------------------------
    END IF
!-------------------------------------------------------------------------------
!
!*    6.     filling grid points
!            -------------------
!
    JOUT=0
    DO JIN=1,SIZE(PFIELD,1)
      IF (KCODE(JIN)/=0) CYCLE
      JOUT=JOUT+1
      PFIELD(JIN,JK) = ZVALOUT(JOUT)
      PFIELD(JIN,JK) = MAX (ZMINVAL, MIN(ZMAXVAL,PFIELD(JIN,JK))) ! control of extrema
    END DO
!
!-------------------------------------------------------------------------------
  END DO
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    8.     Deallocations
!            -------------
!
IF (ALLOCATED(ZXDATA))  DEALLOCATE(ZXDATA)
IF (ALLOCATED(ZDATA))   DEALLOCATE(ZDATA)
IF (ALLOCATED(ZC))      DEALLOCATE(ZC)
IF (ALLOCATED(ZVALOUT)) DEALLOCATE(ZVALOUT)
IF (ALLOCATED(ZXOUT))   DEALLOCATE(ZXOUT)
IF (LHOOK) CALL DR_HOOK('INTERPOL_SPLINES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_SPLINES
