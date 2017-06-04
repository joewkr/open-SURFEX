SUBROUTINE MY_FORC(HEXPER,HYEAR,HYEAR2,KNI,KNPTS,             &
                   KYEAR,KMONTH,KDAY,PTIME,                   &
                   PLON, PLAT, PZS, PZREF, PUREF,             &
                   PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                   PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )

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
USE MODI_MY_FORC_MA01
USE MODI_MY_FORC_HAPEX
USE MODI_MY_FORC_PLM
USE MODI_MY_FORC_ALQUEVA0206
USE MODI_MY_FORC_ALP_FOR_0203
USE MODI_MY_FORC_ME93
USE MODI_MY_FORC_VL92
USE MODI_MY_FORC_CDP9697
USE MODI_MY_FORC_SAFRAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!----------------------------------------------------------------------------

CHARACTER(LEN=12), INTENT(IN):: HEXPER    ! experiment name
CHARACTER(LEN=*), INTENT(IN) :: HYEAR     ! year of forcing
CHARACTER(LEN=*), INTENT(IN) :: HYEAR2    ! year of forcing
INTEGER, INTENT(IN)          :: KNI       ! number of grid cells
INTEGER, INTENT(IN)          :: KNPTS     ! number of forcing instants
INTEGER, INTENT(INOUT)       :: KYEAR     ! year  of simulation begining
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC:MY_FORC',0,ZHOOK_HANDLE)
SELECT CASE (HEXPER)
!
        CASE ('MA01        ')
                CALL MY_FORC_MA01(HEXPER,KNI,KNPTS,                          &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('HAPEX       ')
                CALL MY_FORC_HAPEX(HEXPER,KNI,KNPTS,                         &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('PLM         ')
                CALL MY_FORC_PLM  (HEXPER,KNI,KNPTS,                         &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('ALQUEVA0206 ')
                CALL MY_FORC_ALQUEVA0206(HEXPER,KNI,KNPTS,                   &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('ALP_FOR_0203')
                CALL MY_FORC_ALP_FOR_0203(HEXPER,KNI,KNPTS,                  &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('ME93        ')
                CALL MY_FORC_ME93(HEXPER,KNI,KNPTS,                          &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('VL92        ')
                CALL MY_FORC_VL92(HEXPER,KNI,KNPTS,                          &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('cdp9697     ')
                CALL MY_FORC_CDP9697(HEXPER,KNI,KNPTS,                       &
                                  KYEAR,KMONTH,KDAY,PTIME,                   &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )
        CASE ('SAFRAN     ') 
                CALL MY_FORC_SAFRAN(HEXPER,HYEAR,HYEAR2,KNI,KNPTS,              &
                                  KMONTH,KDAY,PTIME,                         &
                                  PLON, PLAT, PZS, PZREF, PUREF,             &
                                  PTA, PQA, PPS, PWINDSPEED, PWINDDIR,       &
                                  PDIR_SW, PSCA_SW, PLW, PRAIN, PSNOW, PCO2  )                              
        CASE DEFAULT
                PRINT*,' HEXPER = ',HEXPER
                PRINT*,' EXPERIMENT HEXPER NOT DEFINED '
                IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC:MY_FORC',1,ZHOOK_HANDLE)
                STOP

END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC:MY_FORC',1,ZHOOK_HANDLE)
!
!==============================================================================
END SUBROUTINE MY_FORC
!==============================================================================
