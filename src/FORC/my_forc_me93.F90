SUBROUTINE MY_FORC_ME93(HEXPER,KNI,KNPTS,                          &
                   KYEAR,KMONTH,KDAY,PTIME,                        &
                   PLON, PLAT, PZS, PZREF, PUREF,                  &
                   PTA, PQA, PPS, PWINDSPEED, PWINDDIR,            &
                   PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2       )

!----------------------------
!!
!!    PURPOSE
!!    -------
!!   This subroutine allows the user to build atm. forcing of his(her) run.
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson and P. Lemoigne                 Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     06/06
!!
!
!----------------------------------------------------------------------------
!      
!*    0.     Declaration of dummy arguments
!            ------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!----------------------------------------------------------------------------

CHARACTER(LEN=12), INTENT(IN) :: HEXPER    ! experiment name
INTEGER, INTENT(IN)          :: KNI       ! number of grid cells
INTEGER, INTENT(IN)          :: KNPTS     ! number of forcing instants
INTEGER, INTENT(OUT)         :: KYEAR     ! year  of simulation begining
INTEGER, INTENT(OUT)         :: KMONTH    ! month of simulation begining
INTEGER, INTENT(OUT)         :: KDAY      ! day   of simulation begining
REAL,    INTENT(OUT)         :: PTIME     ! time  of simulation begining (s)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PCO2      ! CO2 concentration (kg/m3) 
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PDIR_SW   ! Solar direct   radiation (W/m2)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PSCA_SW   ! Solar diffused radiation (W/m2)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PLW       ! Longwave radiation (W/m2)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PWINDSPEED! Wind speed (m/s)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PWINDDIR  ! Wind dir. (deg. from N, clockwise)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PRAIN     ! rain rate (kg/m2/s)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PSNOW     ! snow rate (kg/m2/s)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PTA       ! temperature (K)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PQA       ! humidity (kg/kg)
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PPS       ! pressure (Pa)
REAL*4, DIMENSION(KNI),       INTENT(OUT) :: PZREF     ! height of temperature forcing (m)
REAL*4, DIMENSION(KNI),       INTENT(OUT) :: PUREF     ! height of wind forcing (m)
REAL*4, DIMENSION(KNI),       INTENT(OUT) :: PZS       ! orography (m)
REAL, DIMENSION(KNI),       INTENT(OUT) :: PLON      ! longitude (degrees)
REAL, DIMENSION(KNI),       INTENT(OUT) :: PLAT      ! latitude  (degrees)
!
!*    1.     Declaration of user local variables
!            -----------------------------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Input file:
!
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN= '../DATA/me93/ME93_EB_teb_test.txt'
!
CHARACTER(LEN=*), PARAMETER       :: YFILE_GLOBAL = '../DATA/me93/Global_me93.txt'
CHARACTER(LEN=*), PARAMETER       :: YFILE_DIFFUS = '../DATA/me93/Diffuse_me93.txt'
!
!----------------------------------------------------------------------------
! Declare variables to translate forcing from obs to surfex atmospheric forcing
!
REAL :: ZWIND, ZT, ZQ, ZRH, ZE, ZES, ZWORK, ZRATIO, &
        ZK, ZD, ZPRES, ZRAIN, ZSNOW, ZRG

REAL, DIMENSION(10) :: ZL
REAL, DIMENSION(KNPTS) :: ZCOVER


REAL :: z1,z2,z3,z4,z8,z9,z10,z12,z13,z14,z15
!
INTEGER :: I, J ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!      
!*    2.     Initialization of date (UTC)
!            ------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ME93:MY_FORC_ME93',0,ZHOOK_HANDLE)
KDAY    = 01          ! starting day 
KMONTH  = 12          ! starting month
KYEAR   = 1993        ! starting year
PTIME   = 3600.       ! starting time (s)
!
!-----------------------------------------------------------------------------
!      
!      3.    grid definition
!            ---------------
!
PLON(:)   = -99.17
PLAT(:)   = 19.41
!
!----------------------------------------------------------------------------
!      
!        4.    orography definition
!               --------------------
!      
PZS(:)   = 2000.
!        
!-----------------------------------------------------------------------------
!      
!      5.    Forcing height
!            --------------
!
PZREF(:)   = 11.5
PUREF(:)   = 11.5
!
!----------------------------------------------------------------------------
!      
!*      6.   Initialization of forcing variables
!            -----------------------------------
!
!----------------------------------------------------------------------------
!      
!        3.1    reading forcing file
!               --------------------
!
!
print*, 'YFILE_FORCIN=', YFILE_FORCIN
OPEN(UNIT=11, FILE=YFILE_FORCIN, FORM='FORMATTED', STATUS='OLD')
print*, 'YFILE_GLOBAL=', YFILE_GLOBAL
OPEN(UNIT=12, FILE=YFILE_GLOBAL, FORM='FORMATTED', STATUS='OLD')
print*, 'YFILE_DIFFUS=', YFILE_DIFFUS
OPEN(UNIT=13, FILE=YFILE_DIFFUS, FORM='FORMATTED', STATUS='OLD')
!
!
! clouds on day 337
!
ZCOVER(:)      = 0.
ZCOVER(88)     = 0.2
ZCOVER(89:100) = 0.5
ZCOVER(101)    = 0.5
ZCOVER(102)    = 0.5
ZCOVER(103)    = 0.2
!
! loop on forcing instants
!
DO I=1,2
 READ(11,*)
END DO
!
! loop on forcing instants
!
DO I=1,KNPTS
!
  ZRG=0.
  ZD=0.
  DO J=1,60
    IF (I>=1) THEN
      READ(12,*) z1, z2, z3, z4
      ZRG = ZRG + z4/60.
      READ(13,*) z1, z2, z3, z4
      ZD = ZD + z4/60.
    END IF
  END DO
  ZK = ZRG - ZD

  READ(11,*) z1,z2,z3,z4,ZWIND,ZRH,ZT,z8,z9,z10,ZPRES,ZRAIN,z13,z14,z15

! conversion into international system unit

  ZPRES = ZPRES * 1000.
  ZT  = ZT + 273.15
     
! retrieve specific from relative humidity
! (maybe there is confusion because of RH definition with mixing ratios)
!
  ZES = 611. * EXP(2.46E6/461*(1./273.15-1./ZT))
  ZE  = ZRH / 100. * ZES
  ZRATIO  = 0.622 * ZE / ZPRES
  ZQ = 1. / ( 1./ZRATIO + 1.)
!
!-----------------------------------------------------------------------------
!
! estimates Downwards Longwave radiation
! ( ALL clear sky days, or maybe with cirruses )

!* Staley & Jurica (1972)

  ZL(1) = 0.67 * (ZE/100.)**0.08 * 5.67E-8 * ZT**4

!* Angstrom

  ZL(2) = (0.82 - 0.25 * 10.**(-0.07*ZE/100.)) * 5.67E-8 * ZT**4

!* Lonnquist

  ZL(3) = (0.54 + 0.05 *SQRT( 10**(0.295*SQRT(ZE/100.)-0.803) ))* 5.67E-8 * ZT**4

!* Brutsaert (1975)

  ZL(4) = 0.57 * (ZE/100.)**(1./7.) * 5.67E-8 * ZT**4

!* Idso (1981)

  ZL(5) = ( 0.70 + 5.95E-5 * ZE/100. * exp (1500./ZT) ) * 5.67E-8 * ZT**4

!*  Unworth & Monteith (1975)

  ZL(6) = -119. + 1.06 * 5.67E-8 * ZT**4

!*  classical formulation

  ZL(7) = 5.5 * (ZT - 273.16) + 213.
!
!
!*  Prata (1996)
!
  ZWORK = 46.5 * (ZE/100./ZT)
  ZL(8) = (1.-(1.+ZWORK)*EXP(-SQRT(1.2+3*ZWORK))) * 5.67E-8 * ZT**4
!
!
!-----------------------------------------------------------------------------
!
! altitude of Mexico
!
  ZL(:) = ZL(:)  ! - 10.
!
! cloud cover impact
  ZL(:) = (1.-ZCOVER(I)) * ZL(:) + ZCOVER(I) * 5.67E-8 * ZT**4
!-----------------------------------------------------------------------------
!
!        6. Fills Surfex forcing variables
!           ------------------------------
!
  PCO2(I,:)    = 0.000620   ! (kg/m3, equivalent to 350 ppm) 
  PDIR_SW(I,:) = ZK
  PSCA_SW(I,:) = ZD
  PWINDSPEED(I,:) = ZWIND
  PWINDDIR  (I,:) = 180.
  PRAIN(I,:) = ZRAIN
  PSNOW = 0.
  PLW(I,:) = ZL(8)
  PTA(I,:) = ZT
  PPS(I,:) = ZPRES
  PQA(I,:) = ZQ
END DO
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ME93:MY_FORC_ME93',1,ZHOOK_HANDLE)

!----------------------------------------------------------------------------
END SUBROUTINE MY_FORC_ME93
!==============================================================================
