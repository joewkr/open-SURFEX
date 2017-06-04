
SUBROUTINE MY_FORC_ALP_FOR_0203(HEXPER,KNI,KNPTS,                  &
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
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN= '../DATA/Alp_for_0203/met_Alp_for_0203.txt'
!
!----------------------------------------------------------------------------
! Declare variables to translate forcing from obs to surfex atmospheric forcing
!
!
REAL, DIMENSION(KNPTS,KNI) :: ZTA, ZQA, ZUA, ZVA, ZPS, ZRAT, ZRG, ZPRECIP,ZSNOW,ZRAIN,ZHR
REAL                       :: ZZ,ZPRES,ZE1,ZE,ZMR,ZT
!
INTEGER :: I ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!      
!*    2.     Initialization of date (UTC)
!            ------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ALP_FOR_0203:MY_FORC_ALP_FOR_0203',0,ZHOOK_HANDLE)
KDAY    = 01          ! starting day 
KMONTH  = 10          ! starting month
KYEAR   = 2002        ! starting year
PTIME   =    0.       ! starting time (s)
!
!-----------------------------------------------------------------------------
!      
!      3.    grid definition
!            ---------------
!
PLON(:)   = 8.7167
PLAT(:)   = 47.05
!
!----------------------------------------------------------------------------
!      
!        4.    orography definition
!               --------------------
!      
PZS(:)   = 1185.
!        
!-----------------------------------------------------------------------------
!      
!      5.    Forcing height
!            --------------
!
PZREF(:)   = 35.
PUREF(:)   = 35.
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
    READ(11,*,END=200)ZZ, ZZ, ZRG(I,1),ZRAT(I,1), ZSNOW(I,1), ZRAIN (I,1), ZTA(I,1),ZUA(I,1),ZHR(I,1)
    ! conversion of Humidity
     ZPRES=88000.
     ZT=ZTA(I,1)
     ZE=ZEW(ZT)
     ZE1=(ZHR(I,1)/100.)*ZE
     ZMR=0.62198*ZE1/(ZPRES-ZE1)
     ZQA(I,1)=1./(1+1/ZMR)
ENDDO
!
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
PWINDSPEED(:,:) = ZUA(:,:)
PWINDDIR  (:,:) = 0.
PRAIN(:,:) = ZRAIN(:,:)
PSNOW = ZSNOW(:,:)
PLW(:,:) = ZRAT(:,:)
PTA(:,:) = ZTA(:,:)
PPS(:,:) = ZPRES
PQA(:,:) = ZQA(:,:)
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ALP_FOR_0203:MY_FORC_ALP_FOR_0203',1,ZHOOK_HANDLE)

CONTAINS
!----------------------------------------------------------------------------
      FUNCTION ZEW(T)
!
!*  CALCUL DE LA TENSION DE VAPEUR D EAU SATURANTE (EW) PAR LA
!*  FORMULE DE GOFF ET GRATCH
!*
!*    VARIABLE DE SORTIE : EW(Pa)
!*    VARIABLE D ENTREE  :T(K)
!
      REAL T,ZEW
      REAL A,B,C,D,E,F,G,T1,T2
      REAL TST1,T1ST,Z
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      DATA A,B,C,D,E,F,G/10.79574,1.50475E-4,-8.2969,0.42873E-3,   &
      4.76955,0.78614,5.028/,T1,T2/273.16,273.15/
!
      IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ALP_FOR_0203:ZEW',0,ZHOOK_HANDLE)
      T=T-273.15
      TST1=(T+T2)/T1
      T1ST=T1/(T+T2)
      Z=ALOG(10.)*(A*(1.-T1ST)+B*(1.-EXP(ALOG(10.)*C*(TST1-1.)))   &
      +D*(EXP(ALOG(10.)*E*(1.-T1ST))-1.)+F)+G*ALOG(T1ST)
      ZEW=100.* EXP(Z)
      IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_ALP_FOR_0203:ZEW',1,ZHOOK_HANDLE)
      END FUNCTION ZEW

!----------------------------------------------------------------------------
END SUBROUTINE MY_FORC_ALP_FOR_0203
!==============================================================================
