SUBROUTINE MY_FORC_CDP9697(HEXPER,KNI,KNPTS,                       &
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
REAL*4, DIMENSION(KNPTS,KNI), INTENT(OUT) :: PQA       ! humidity (/Fkg/kg)
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
CHARACTER(LEN=*), PARAMETER       :: YFILE_FORCIN = '../DATA/cdp9697/MET_CDP_9697.txt'
!
!----------------------------------------------------------------------------
! Declare variables to translate forcing from obs to surfex atmospheric forcing
!
REAL :: ZWIND, ZT, ZQ, ZK, ZD, ZPRES, ZRG, ZWINDDIR, ZRAT, &
        ZPH,ZH,ZRR,ZMR, ZE,ZE1
REAL :: z1,z2,z3,z4,z5,z6

REAL*4, DIMENSION(KNPTS,KNI) :: ZU, ZV ! wind components
!
INTEGER :: I ! loop counters
!
REAL    :: XPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!      
!*    2.     Initialization of date (UTC)
!            ------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:MY_FORC_CDP9697',0,ZHOOK_HANDLE)
KDAY    = 06          ! starting day 
KMONTH  = 10          ! starting month
KYEAR   = 1996        ! starting year
PTIME   = 0.          ! starting time (s)
!
XPI=4.*ATAN(1.)
!-----------------------------------------------------------------------------
!      
!      3.    grid definition
!            ---------------
!
PLON(:)   =  5.77
PLAT(:)   = 45.30
!
!----------------------------------------------------------------------------
!      
!        4.    orography definition
!               --------------------
!      
PZS(:)   = 1340.
!        
!-----------------------------------------------------------------------------
!      
!      5.    Forcing height
!            --------------
!
PZREF(:)   = 2.
PUREF(:)   = 2.
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
! loop on forcing instants
!
DO I=1,KNPTS
!
! READ(11,*) z1,z2,z3,z4,z5,ZT,ZQ,ZWIND,ZWINDDIR,ZPRES,ZRG,ZRAT,z6
  READ(11,*) z1,z2,ZRG,ZRAT,ZRR,ZPH,ZT,ZWIND,ZH

! conversion of Humidity
     ZPRES=84000.
     Z1=ZT
     ZE=ZEW(Z1)
     ZE1=(ZH/100.)*ZE
print*, 'ZEW=',ZE, 'ZH=',ZH, 'ZE=',ZE1
    ZMR=0.62198*ZE1/(ZPRES-ZE1)
    ZQ=1./(1+1/ZMR)

! wind direction not known, set to 0
ZWINDDIR=0.
     
!-----------------------------------------------------------------------------
!
ZK = -999.
ZD = -999.
IF (ZRG/=-999.) THEN
   ZK = 0.8 * ZRG
   ZD = 0.2 * ZRG
ENDIF        
!
!-----------------------------------------------------------------------------
!
!        6. Fills Surfex forcing variables
!           ------------------------------
!

  PCO2(I,:)    = 0.000620   ! (kg/m3, equivalent to 350 ppm) 
  PDIR_SW(I,:) = ZK
  PSCA_SW(I,:) = ZD
  PWINDSPEED(I,:) = ZWIND
  PWINDDIR  (I,:) = ZWINDDIR
  PRAIN(I,:) = ZRR *(1-ZPH)
  PSNOW(I,:) = ZRR *ZPH
  PLW(I,:) = ZRAT
  PTA(I,:) = ZT
  PPS(I,:) = ZPRES
  PQA(I,:) = ZQ
END DO
!
!-----------------------------------------------------------------------------
!
!        7. Removes missing data
!           --------------------
!
!temporal interpolation of wind components, in case of missing data
ZU = PWINDSPEED * SIN(XPI*PWINDDIR/180.)
ZV = PWINDSPEED * COS(XPI*PWINDDIR/180.)
WHERE(PWINDDIR==-999. .OR. PWINDSPEED==-999.) ZU = -999.
WHERE(PWINDDIR==-999. .OR. PWINDSPEED==-999.) ZV = -999.
CALL FILL_DATA(ZU)
CALL FILL_DATA(ZV)
PWINDSPEED = SQRT(ZU**2+ZV**2)
PWINDDIR   = ATAN2(ZU,ZV) * 180. / XPI
WHERE(PWINDDIR<0.) PWINDDIR = PWINDDIR + 360.

!temporal interpolation of other variables
CALL FILL_DATA(PDIR_SW)
CALL FILL_DATA(PSCA_SW)
CALL FILL_DATA(PLW)
CALL FILL_DATA(PTA)
CALL FILL_DATA(PPS)
CALL FILL_DATA(PQA)
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:MY_FORC_CDP9697',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
CONTAINS 
!----------------------------------------------------------------------------
SUBROUTINE FILL_DATA(PF)
REAL*4, DIMENSION(:,:), INTENT(INOUT) :: PF
INTEGER :: JI ! loop counter on points
INTEGER :: JL ! loop counter on time instants
INTEGER :: JM ! loop counter on missing points
INTEGER :: IMISS ! counter of successive missing data
REAL    :: ZDF   ! increment
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:FILL_DATA',0,ZHOOK_HANDLE)
DO JI=1,SIZE(PF,2)
  IMISS=0
  DO JL=1,SIZE(PF,1)
    IF (PF(JL,JI)==-999.) THEN
      IMISS=IMISS+1
    ELSE
      IF (IMISS>0) THEN
        ZDF=(PF(JL,JI)-PF(JL-1-IMISS,JI))/(IMISS+1)
        DO JM=1,IMISS
          PF(JL-1-IMISS+JM,JI) = PF(JL-2-IMISS+JM,JI) + ZDF
        END DO
        IMISS=0
      END IF
    END IF 
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:FILL_DATA',1,ZHOOK_HANDLE)
END SUBROUTINE FILL_DATA
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
      IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:ZEW',0,ZHOOK_HANDLE)
      T=T-273.15
      TST1=(T+T2)/T1
      T1ST=T1/(T+T2)
      Z=ALOG(10.)*(A*(1.-T1ST)+B*(1.-EXP(ALOG(10.)*C*(TST1-1.)))   &
      +D*(EXP(ALOG(10.)*E*(1.-T1ST))-1.)+F)+G*ALOG(T1ST)
      ZEW=100.* EXP(Z)
      IF (LHOOK) CALL DR_HOOK('MODI_MY_FORC_CDP9697:ZEW',1,ZHOOK_HANDLE)
      END FUNCTION ZEW

!----------------------------------------------------------------------------
END SUBROUTINE MY_FORC_CDP9697
!==============================================================================
