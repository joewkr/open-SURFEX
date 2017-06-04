!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_DATES_NETCDF

! Module to read correctly time in a netcdf file

! Author : Matthieu Lafaysse
! Creation : 2012-11-12

! Modifications

USE MODD_TYPE_DATE_SURF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

INTEGER,PARAMETER::JP_GREGOIRE=1582 !Année où le pape Grégoire XIII décida de passer du calendrier julien au calendrier grégorien
!Avant cette date tous les années multiples de 4 sont des années bissextiles
!Au-delà de cette date les années multiples de 100 ne sont pas bissextiles sauf les années multiples de 400 qui le sont
!De plus les journées du 5 au 14 octobre 1582 ont été supprimées pour compenser le retard accumulé


CONTAINS

!----------------------------------------------------------------------------------------------------------------


SUBROUTINE NETCDF2DATE(PTIME,HUNITS,PDATETIME)
!
! Conversion de la date au format netcdf vers le format année mois jour heure selon dimension de itv_date
REAL,DIMENSION(:),INTENT(IN) :: PTIME
 CHARACTER(*),INTENT(IN) :: HUNITS
TYPE (DATE_TIME),DIMENSION(:),INTENT(OUT) :: PDATETIME
!
INTEGER,DIMENSION(SIZE(PTIME)) :: ITIMEHOURS
REAL,DIMENSION(SIZE(PTIME)) :: ZREST
INTEGER :: IYEARUNITS,IMONTHUNITS,IDAYUNITS,IHOURUNITS
!
 CHARACTER(LEN=14) :: YHEADER
 CHARACTER(LEN=100) :: YFMT 
 CHARACTER :: YC1,YC2,YC3
!
LOGICAL :: GHEADER
!
INTEGER,DIMENSION(4),PARAMETER :: ITV_DATEREF = (/1900,1,1,0/)
INTEGER,DIMENSION(4),PARAMETER :: ITV_DATEREFBIS = (/1850,1,1,0/)
!
INTEGER,DIMENSION(4) :: ITV_DATETEMPO
INTEGER :: ICARACUNITS,ICARACHOUR,ICARACDAY,ICARACMONTH,ICARACYEAR
INTEGER :: IERROR
INTEGER :: JTIME
INTEGER :: IV_DIFFREF
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:NETCDF2DATE',0,ZHOOK_HANDLE)
!
SELECT CASE (HUNITS(1:4))
  CASE ("days")
    ICARACUNITS = 11
    ITIMEHOURS = INT(PTIME*24.)
    ZREST = PTIME*24.-ITIMEHOURS
  CASE ("hour")
    ICARACUNITS = 12
    ITIMEHOURS = INT(PTIME)
    ZREST = PTIME-ITIMEHOURS
  CASE ("minu")
    ICARACUNITS = 14
    ITIMEHOURS = INT(PTIME/60.)
    ZREST = (PTIME/60.)-ITIMEHOURS
  CASE ("seco")
    ICARACUNITS = 14
    ITIMEHOURS = INT(PTIME/3600.)
    ZREST = (PTIME/3600.)-ITIMEHOURS
  CASE DEFAULT
    PRINT*, "ERROR date_netcdf.F90 : Can't read time units :"
    PRINT*,TRIM(HUNITS)
    STOP "Error units time"
END SELECT

SELECT CASE (HUNITS(1:4))
  !CASE ("days")
  !  !Read reference date
  !  boucles_days:DO ICARACDAY=1,2 !Day written by 1 or 2 char
  !    DO ICARACMONTH=1,2 !Month written by 1 or 2 char
  !      DO ICARACYEAR=1,4 !Year written by 1 to 4 char
  !        WRITE(YFMT,FMT='("(A",I2,",",2("I",I1,",A1,"),"I",I1,",X,I",I1,",A1)")')&
  !          ICARACUNITS,ICARACYEAR,ICARACMONTH,ICARACDAY,ICARACHOUR
  !        READ(HUNITS,FMT=YFMT,IOSTAT=IERROR)YHEADER,IYEARUNITS,YC1,IMONTHUNITS,YC2,IDAYUNITS,IHOURUNITS,YC3
  !        IHOURUNITS=0
  !        IF (IERROR==0) THEN
  !          IF ((TRIM(YHEADER)=='days since').AND.(YC1=='-').AND.(YC2=='-').AND.(YC3==':')) THEN
  !            ! PRINT*,"DATE DE REFERENCE LUE :"
  !            ! PRINT*,itv_oldref
  !            EXIT boucles_days
  !          END IF
  !        END IF
  !      END DO
  !    END DO
  !  END DO boucles_days
  CASE ("days","hour","minu","seco")
    !Read reference date
    boucles_hours:DO ICARACHOUR=1,2 !Hour written by 1 or 2 char
      DO ICARACDAY=1,2 !Day written by 1 or 2 char
        DO ICARACMONTH=1,2 !Month written by 1 or 2 char
          DO ICARACYEAR=1,4 !Year written by 1 to 4 char
            WRITE(YFMT,FMT='("(A",I2,",",2("I",I1,",A1,"),"I",I1,",X,I",I1,",A1)")')&
              ICARACUNITS,ICARACYEAR,ICARACMONTH,ICARACDAY,ICARACHOUR
            READ(HUNITS,FMT=YFMT,IOSTAT=IERROR)YHEADER,IYEARUNITS,&
              YC1,IMONTHUNITS,YC2,IDAYUNITS,IHOURUNITS,YC3
            IF (IERROR==0) THEN
              GHEADER=(TRIM(YHEADER)=='hours since') .OR. (TRIM(YHEADER)=='minutes since') .OR. &
                      (TRIM(YHEADER)=='seconds since') .OR. (TRIM(YHEADER)=='days since')
              IF (GHEADER.AND.(YC1=='-').AND.(YC2=='-').AND.(YC3==':')) THEN
                ! PRINT*,"DATE DE REFERENCE LUE :"
                ! PRINT*,itv_oldref
                EXIT boucles_hours
              END IF
            END IF
          END DO
        END DO
      END DO
    END DO boucles_hours
  CASE DEFAULT
    STOP "Error units time"
END SELECT



!Check successful reading
IF (IERROR>0) THEN
  PRINT*, "ERROR date_netcdf.F90 : Can't read time units :"
  PRINT*,TRIM(HUNITS)
  STOP "Error units time"
END IF

DO JTIME=1,SIZE(PTIME)
  !Initialiaze the date to the reference date
  PDATETIME(JTIME)%TDATE%YEAR = IYEARUNITS
  PDATETIME(JTIME)%TDATE%MONTH = IMONTHUNITS
  PDATETIME(JTIME)%TDATE%DAY = IDAYUNITS
  PDATETIME(JTIME)%TIME = IHOURUNITS+ZREST(JTIME)
  CALL ADDHOURS(PDATETIME(JTIME),ITIMEHOURS(JTIME))
END DO

IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:NETCDF2DATE',1,ZHOOK_HANDLE)

END SUBROUTINE NETCDF2DATE
!----------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------
LOGICAL FUNCTION LEAPYEAR (PYEAR)
INTEGER, INTENT(IN) :: PYEAR ! Is the year a leap year ?
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:LEAPYEAR',0,ZHOOK_HANDLE)

IF (PYEAR>JP_GREGOIRE) THEN
  LEAPYEAR = (((MOD(PYEAR,4)==0).AND.(MOD(PYEAR,100)/=0)).OR.(MOD(PYEAR,400)==0))
ELSE
  LEAPYEAR = (MOD(PYEAR,4)==0)
ENDIF

IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:LEAPYEAR',1,ZHOOK_HANDLE)

END FUNCTION LEAPYEAR
!--------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------
SUBROUTINE ADDHOURS (TPTIME,KNHOURS)
!Add the number of hours to a date
TYPE(DATE_TIME),INTENT(INOUT)::TPTIME
INTEGER,INTENT(IN)::KNHOURS ! number of hours
INTEGER,DIMENSION(12)::INBDM !Number of days per months
INTEGER::IREMAININGDAYS !Number of remaining days to add
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:ADDHOURS',0,ZHOOK_HANDLE)

!special case of 1582
IF (TPTIME%TDATE%YEAR==JP_GREGOIRE) THEN
  INBDM=(/31,28,31,30,31,30,31,31,30,21,30,31/)
ELSE
  IF (LEAPYEAR(TPTIME%TDATE%YEAR)) THEN
    INBDM=(/31,29,31,30,31,30,31,31,30,31,30,31/)
  ELSE
    INBDM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  ENDIF
ENDIF

IF (KNHOURS>=0) THEN
  !Increase hour
  IF ((INT(TPTIME%TIME)+KNHOURS)<=23) THEN
    TPTIME%TIME=TPTIME%TIME+KNHOURS
  ELSE
    IREMAININGDAYS=(INT(TPTIME%TIME)+KNHOURS)/24 !Number of remaining days
    TPTIME%TIME=MOD(INT(TPTIME%TIME)+KNHOURS,24)+TPTIME%TIME-INT(TPTIME%TIME) ! new hour (+minutes)
    !Increase days
    DO
      IF ((TPTIME%TDATE%DAY+IREMAININGDAYS)<=INBDM(TPTIME%TDATE%MONTH)) THEN
        TPTIME%TDATE%DAY=TPTIME%TDATE%DAY+IREMAININGDAYS
        EXIT
      ELSE
        IREMAININGDAYS=IREMAININGDAYS-(INBDM(TPTIME%TDATE%MONTH)-TPTIME%TDATE%DAY+1)
        IF (TPTIME%TDATE%MONTH<12) THEN
          TPTIME%TDATE%MONTH=TPTIME%TDATE%MONTH+1 !Month change
          TPTIME%TDATE%DAY=1
        ELSE
          !Year change
          TPTIME%TDATE%YEAR=TPTIME%TDATE%YEAR+1
          TPTIME%TDATE%MONTH=1
          TPTIME%TDATE%DAY=1
          !Update february number of days
          IF (LEAPYEAR(TPTIME%TDATE%YEAR)) INBDM(2)=29
          IF (LEAPYEAR(TPTIME%TDATE%YEAR-1)) INBDM(2)=28
          !Update october month for 1582 and 1583
          IF (TPTIME%TDATE%YEAR==JP_GREGOIRE) INBDM(10)=21
          IF (TPTIME%TDATE%YEAR==JP_GREGOIRE+1) INBDM(10)=31
        ENDIF
      ENDIF
    ENDDO
  ENDIF
ELSE
  IF ((INT(TPTIME%TIME)+KNHOURS)>=0) THEN
    TPTIME%TIME=TPTIME%TIME+KNHOURS
  ELSE
    IREMAININGDAYS=(INT(TPTIME%TIME)-KNHOURS)/24+1
    TPTIME%TIME=MOD(INT(TPTIME%TIME)-KNHOURS,24)+TPTIME%TIME-INT(TPTIME%TIME) ! new hour
    ! decrease days
    DO
      IF  ((TPTIME%TDATE%DAY-IREMAININGDAYS)>=1) THEN
        TPTIME%TDATE%DAY=TPTIME%TDATE%DAY-IREMAININGDAYS
        EXIT
      ELSE
        IREMAININGDAYS=IREMAININGDAYS-TPTIME%TDATE%DAY
        IF (TPTIME%TDATE%MONTH>=1) THEN
          TPTIME%TDATE%MONTH=TPTIME%TDATE%MONTH-1 !Month change
          TPTIME%TDATE%DAY=INBDM(TPTIME%TDATE%MONTH)
        ELSE
          !Year change
          TPTIME%TDATE%YEAR=TPTIME%TDATE%YEAR-1
          TPTIME%TDATE%MONTH=12
          TPTIME%TDATE%DAY=INBDM(TPTIME%TDATE%MONTH)
          !Update february number of days
          IF (LEAPYEAR(TPTIME%TDATE%YEAR)) INBDM(2)=29
          IF (LEAPYEAR(TPTIME%TDATE%YEAR+1)) INBDM(2)=28
          !Update october month for 1582 and 1583
          IF (TPTIME%TDATE%YEAR==JP_GREGOIRE) INBDM(10)=21
          IF (TPTIME%TDATE%YEAR==JP_GREGOIRE-1) INBDM(10)=31
        END IF
      END IF
    END DO
  END IF
END IF

IF (LHOOK) CALL DR_HOOK('MODE_DATES_NETCDF:ADDHOURS',1,ZHOOK_HANDLE)

END SUBROUTINE ADDHOURS
!--------------------------------------------------------------------------------------------------------------

END MODULE MODE_DATES_NETCDF
