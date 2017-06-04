SUBROUTINE MY_FORC_HAPEX(HEXPER,KNI,KNPTS,              &
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
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN= '../DATA/hapex/HAPEX.DAT.30'
!
!----------------------------------------------------------------------------
! Declare variables to translate forcing from obs to surfex atmospheric forcing
!
!
REAL, DIMENSION(KNPTS,KNI) :: ZTA, ZQA, ZUA, ZVA, ZPS, ZRAT, ZRG, ZPRECIP
!
INTEGER :: I ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    2.     Initialization of date (UTC)
!            ------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_HAPEX:MY_FORC_HAPEX',0,ZHOOK_HANDLE)

KDAY    = 01          ! starting day 

KMONTH  = 01          ! starting month

KYEAR   = 1986        ! starting year

PTIME   =    0.       ! starting time (s)
!
!-----------------------------------------------------------------------------
!      
!      3.    grid definition
!            ---------------
!
PLON(:)   = 1.300
PLAT(:)   = 43.484
!
!----------------------------------------------------------------------------
!      
!        4.    orography definition
!               --------------------
!      
PZS(:)   = 113.
!      
!-----------------------------------------------------------------------------
!      
!      5.    Forcing height
!            --------------
!
PZREF(:)   = 2.
PUREF(:)   = 10.
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
!
DO I=1,2
   READ(11,*)
ENDDO
!
DO I=1,KNPTS
    READ(11,100,END=200)ZRG(I,1),ZRAT(I,1),ZPRECIP(I,1),ZTA(I,1),ZUA(I,1),ZVA(I,1),ZPS(I,1),ZQA(I,1)
ENDDO
!
100 FORMAT(2F7.1,E14.3,3F10.3,F10.1,E12.3)        
!
200 CONTINUE
CLOSE(UNIT=11)
!-----------------------------------------------------------------------------
!
!        6. Fills Surfex forcing variables
!           ------------------------------
!
PCO2(:,:)    = 0.000620   ! (kg/m3, equivalent to 350 ppm) 
PDIR_SW(:,:) = ZRG(:,:)
PSCA_SW(:,:) = 0.
PWINDSPEED(:,:) = SQRT(ZVA(:,:)**2+ZUA(:,:)**2)
PWINDDIR  (:,:) = 180.
PRAIN(:,:) = ZPRECIP(:,:)
PSNOW = 0.
PLW(:,:) = ZRAT(:,:)
PTA(:,:) = ZTA(:,:)
PPS(:,:) = ZPS(:,:)
PQA(:,:) = ZQA(:,:)
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_HAPEX:MY_FORC_HAPEX',1,ZHOOK_HANDLE)

!----------------------------------------------------------------------------
END SUBROUTINE MY_FORC_HAPEX
!==============================================================================
