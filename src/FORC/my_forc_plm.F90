SUBROUTINE MY_FORC_PLM(HEXPER,KNI,KNPTS,              &
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
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_01 = '../DATA/plm/select_TA.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_02 = '../DATA/plm/select_QA.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_03 = '../DATA/plm/select_WIND.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_04 = '../DATA/plm/select_DIR.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_05 = '../DATA/plm/select_RAIN.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_06 = '../DATA/plm/select_SNOW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_07 = '../DATA/plm/select_DIR_SW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_08 = '../DATA/plm/select_PS.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_09 = '../DATA/plm/select_LW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_10 = '../DATA/plm/select_SCA_SW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_11 = '../DATA/plm/select_LON.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN_12 = '../DATA/plm/select_LAT.dat'
!
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_01 = '../DATA/plm/select_TA.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_02 = '../DATA/plm/select_QA.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_03 = '../DATA/plm/select_WIND.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_04 = '../DATA/plm/select_DIR.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_05 = '../DATA/plm/select_RAIN.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_06 = '../DATA/plm/select_SNOW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_07 = '../DATA/plm/select_DIR_SW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_08 = '../DATA/plm/select_PS.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_09 = '../DATA/plm/select_LW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_10 = '../DATA/plm/select_SCA_SW.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_11 = '../DATA/plm/select_LON.dat'
CHARACTER(LEN=*), PARAMETER       :: YFILE_SELECT_12 = '../DATA/plm/select_LAT.dat'
!----------------------------------------------------------------------------
! Declare variables to translate forcing from obs to surfex atmospheric forcing
!
!
REAL, DIMENSION(KNPTS,KNI) :: ZTA, ZQA, ZUA, ZPS, ZRAT, ZRG
REAL, DIMENSION(KNPTS,KNI) :: ZDIR, ZCO2, ZRAIN, ZSNOW, ZSCA_SW
REAL, DIMENSION(KNI) :: ZLON, ZLAT
REAL                       :: ZZ
CHARACTER(LEN=20)          :: YAUX
LOGICAL, DIMENSION(KNI)    :: LSELECT
LOGICAL                    :: LSELECT_AREA=.FALSE.
REAL :: XLONMIN = -15.  !-10.5
REAL :: XLONMAX = 66.   !12.5
REAL :: XLATMIN = 21.   !39.5
REAL :: XLATMAX = 79.   !55.5
!
INTEGER :: I, K ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!      
!*    2.     Initialization of date (UTC)
!            ------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_PLM:MY_FORC_PLM',0,ZHOOK_HANDLE)
KDAY    = 01          ! starting day 
KMONTH  = 01          ! starting month
KYEAR   = 2005        ! starting year
PTIME   =    0.       ! starting time (s)
!
!-----------------------------------------------------------------------------
!      
!      3.    grid definition
!            ---------------
!
!
! longitude
print*, 'YFILE_FORCIN=', YFILE_FORCIN_11
OPEN(UNIT=11, FILE=YFILE_FORCIN_11, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=211)ZLON(:)
211 CONTINUE
CLOSE(UNIT=11)
!
! latitude
print*, 'YFILE_FORCIN=', YFILE_FORCIN_12
OPEN(UNIT=11, FILE=YFILE_FORCIN_12, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=212)ZLAT(:)
212 CONTINUE
CLOSE(UNIT=11)
!
PLON(:)   = ZLON(:)
PLAT(:)   = ZLAT(:)
!
!----------------------------------------------------------------------------
!      
!        4.    orography definition
!               --------------------
!      
PZS(:)   = 0.
!        
!-----------------------------------------------------------------------------
!      
!      5.    Forcing height
!            --------------
!
PZREF(:)   = 10.
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
! temperature
print*, 'YFILE_FORCIN=', YFILE_FORCIN_01
OPEN(UNIT=11, FILE=YFILE_FORCIN_01, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=201)ZTA(:,:)
201 CONTINUE
CLOSE(UNIT=11)

!
! humidity
print*, 'YFILE_FORCIN=', YFILE_FORCIN_02
OPEN(UNIT=11, FILE=YFILE_FORCIN_02, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=202)ZQA(:,:)
202 CONTINUE
CLOSE(UNIT=11)

!
! wind speed
print*, 'YFILE_FORCIN=', YFILE_FORCIN_03
OPEN(UNIT=11, FILE=YFILE_FORCIN_03, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=203)ZUA(:,:)
203 CONTINUE
CLOSE(UNIT=11)

!
! wind direction
print*, 'YFILE_FORCIN=', YFILE_FORCIN_04
OPEN(UNIT=11, FILE=YFILE_FORCIN_04, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=204)ZUA(:,:)
204 CONTINUE
CLOSE(UNIT=11)

!
! rain rate
print*, 'YFILE_FORCIN=', YFILE_FORCIN_05
OPEN(UNIT=11, FILE=YFILE_FORCIN_05, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=205)ZRAIN(:,:)
205 CONTINUE
CLOSE(UNIT=11)

!
! snow rate
print*, 'YFILE_FORCIN=', YFILE_FORCIN_06
OPEN(UNIT=11, FILE=YFILE_FORCIN_06, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=206)ZSNOW(:,:)
206 CONTINUE
CLOSE(UNIT=11)

!
! direct sw
print*, 'YFILE_FORCIN=', YFILE_FORCIN_07
OPEN(UNIT=11, FILE=YFILE_FORCIN_07, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=207)ZRG(:,:)
207 CONTINUE
CLOSE(UNIT=11)

!
! pressure
print*, 'YFILE_FORCIN=', YFILE_FORCIN_08
OPEN(UNIT=11, FILE=YFILE_FORCIN_08, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=208)ZPS(:,:)
208 CONTINUE
CLOSE(UNIT=11)

!
! long wave
print*, 'YFILE_FORCIN=', YFILE_FORCIN_09
OPEN(UNIT=11, FILE=YFILE_FORCIN_09, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=209)ZRAT(:,:)
209 CONTINUE
CLOSE(UNIT=11)

!
! scattered sw
print*, 'YFILE_FORCIN=', YFILE_FORCIN_10
OPEN(UNIT=11, FILE=YFILE_FORCIN_10, FORM='FORMATTED', STATUS='OLD')
READ(11,*,END=210)ZSCA_SW(:,:)
210 CONTINUE
CLOSE(UNIT=11)

!-----------------------------------------------------------------------------
IF (LSELECT_AREA) THEN
!        
OPEN(UNIT=11, FILE=YFILE_SELECT_11, FORM='FORMATTED')
OPEN(UNIT=12, FILE=YFILE_SELECT_12, FORM='FORMATTED')
do i=1,kni
   lselect(i)=(plon(i)>=xlonmin .and. plon(i)<=xlonmax .and. plat(i)>=xlatmin .and. plat(i)<=xlatmax)
   if (lselect(i)) then
      write(11,*)plon(i)
      write(12,*)plat(i)
   endif
enddo
CLOSE(UNIT=11)
CLOSE(UNIT=12)
!
! temperature
OPEN(UNIT=12, FILE=YFILE_SELECT_01, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zta(:,i)
enddo
CLOSE(UNIT=12)
! humidity
OPEN(UNIT=12, FILE=YFILE_SELECT_02, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zqa(:,i)
enddo
CLOSE(UNIT=12)
! wind speed
OPEN(UNIT=12, FILE=YFILE_SELECT_03, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zua(:,i)
enddo
CLOSE(UNIT=12)
! wind direction
OPEN(UNIT=12, FILE=YFILE_SELECT_04, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zdir(:,i)
enddo
CLOSE(UNIT=12)
! rain rate
OPEN(UNIT=12, FILE=YFILE_SELECT_05, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zrain(:,i)
enddo
CLOSE(UNIT=12)
! snow rate
OPEN(UNIT=12, FILE=YFILE_SELECT_06, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zsnow(:,i)
enddo
CLOSE(UNIT=12)
! direct sw
OPEN(UNIT=12, FILE=YFILE_SELECT_07, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zrg(:,i)
enddo
CLOSE(UNIT=12)
! pressure
OPEN(UNIT=12, FILE=YFILE_SELECT_08, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zps(:,i)
enddo
CLOSE(UNIT=12)
! long-wave
OPEN(UNIT=12, FILE=YFILE_SELECT_09, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zrat(:,i)
enddo
CLOSE(UNIT=12)
! scattered sw
OPEN(UNIT=12, FILE=YFILE_SELECT_10, FORM='FORMATTED')
do i=1,kni
   if (lselect(i)) write(12,*)zsca_sw(:,i)
enddo
CLOSE(UNIT=12)


        PRINT*,' SELECTION TERMINEE'
        STOP
ENDIF
!
!        6. Fills Surfex forcing variables
!           ------------------------------
!
PCO2(:,:)    = 0.000620   ! (kg/m3, equivalent to 350 ppm) 
PDIR_SW(:,:) = ZRG(:,:)
PSCA_SW(:,:) = ZSCA_SW
PWINDSPEED(:,:) = ZUA(:,:)
PWINDDIR  (:,:) = ZDIR(:,:)
PRAIN(:,:) = ZRAIN(:,:)
PSNOW = ZSNOW(:,:)
PLW(:,:) = ZRAT(:,:)
PTA(:,:) = ZTA(:,:)
PPS(:,:) = ZPS(:,:)
PQA(:,:) = ZQA(:,:)
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_PLM:MY_FORC_PLM',1,ZHOOK_HANDLE)

!----------------------------------------------------------------------------
END SUBROUTINE MY_FORC_PLM
!==============================================================================
