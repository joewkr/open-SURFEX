!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_EMISSION_SNAP_n (CHN, &
                                     HPROGRAM,KSIZE,PSIMTIME,PSUNTIME, &
                                    KYEAR,KMONTH,KDAY,PRHOA,PLON      )
!     ######################################################################
!!
!!***  *CH_EMISSION_SNAP_n* - 
!!
!!    PURPOSE
!!    -------
!!      Return a time-dependent emission flux based on tabulated values
!!
!!**  METHOD
!!    ------
!!
!!    AUTHOR
!!    ------
!!    S. Queguiner
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/2011
!!
!!    A. Alias     07/2016  gmkpack problem : name of the internal subroutine modified 
!!                          because exist already (view SURFEX/day_of_week.F90 )
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
!
USE MODD_CSTS,        ONLY: XDAY
!
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_SUBSTRACT_TO_DATE_SURF
USE MODI_CH_CONVERSION_FACTOR
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1  declaration of arguments
!
!
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
!
 CHARACTER(LEN=6),       INTENT(IN)  :: HPROGRAM! program calling surf. schemes
INTEGER,                INTENT(IN)  :: KSIZE   ! number of points
REAL,                   INTENT(IN)  :: PSIMTIME! time of simulation in sec UTC
                                               ! (counting from midnight of
                                               ! the current day)
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PSUNTIME! Solar time (s since midnight)
INTEGER,                INTENT(IN)  :: KYEAR,KMONTH,KDAY ! UTC year, month, day
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PRHOA   ! Air density
REAL, DIMENSION(KSIZE), INTENT(IN)  :: PLON    ! Longitude (deg, from Greenwich)
!                                              ! (must be between -180deg and 180deg)
!
!*       0.2  declaration of local variables
!
REAL,   DIMENSION(KSIZE) :: ZLON   ! Longitude centered in Greenwich meridian
REAL,   DIMENSION(KSIZE) :: ZTIME0
INTEGER,DIMENSION(KSIZE,2) :: IYEAR ! Year        at the begining of current hour
INTEGER,DIMENSION(KSIZE,2) :: IMONTH! Month       at the begining of current hour
INTEGER,DIMENSION(KSIZE,2) :: IDAY  ! Day         at the begining of current hour
INTEGER,DIMENSION(KSIZE,2) :: IDOW  ! Day of Week at the begining of current hour
INTEGER,DIMENSION(KSIZE,2) :: IHOUR ! Entire hour at the begining of current hour
REAL,   DIMENSION(KSIZE,2) :: ZTIME ! time (s)    at the begining of current hour
INTEGER                  :: JSPEC ! Loop counter on chemical species
INTEGER                  :: JSNAP ! Loop counter on snap categories
INTEGER                  :: JI, JT    ! Loop counter on points
REAL,DIMENSION(KSIZE,2)    :: ZE  ! Emissions at beginning and end of the hour
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_SNAP_N',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*  1.  Updates Conversion Factor (may depends on air density)
!       ------------------------------------------------------
!
 CALL CH_CONVERSION_FACTOR(CHN%XCONVERSION, CHN%CCONVERSION, PRHOA(:))
!
!------------------------------------------------------------------------------
!
!*  2.  time and date for each point depending on Time reference
!       --------------------------------------------------------
!
!*  2.1 time at the beginning of current hour
!
IDAY  (:,1)=KDAY
IMONTH(:,1)=KMONTH
IYEAR (:,1)=KYEAR
!
SELECT CASE (CHN%CSNAP_TIME_REF)
  CASE ('UTC  ')
    ZTIME0(:)=PSIMTIME
  CASE ('SOLAR')
    ZLON(:)=PLON(:)
    WHERE(PLON(:)>  180.) ZLON(:)=PLON(:)-360.
    WHERE(PLON(:)<=-180.) ZLON(:)=PLON(:)+360.
    !*  retrieves solar date and time
    ZTIME0(:)=PSIMTIME + ZLON(:)*240. ! first guess is approximated solar time.
                                     ! The suntime should be close to this.
    DO JI=1,KSIZE
      IF (ZTIME0(JI)>PSUNTIME(JI)+XDAY/2.) THEN
        ZTIME0(JI) = PSUNTIME(JI) + XDAY
      ELSEIF (ZTIME0(JI)<PSUNTIME(JI)-XDAY/2.) THEN
        ZTIME0(JI) = PSUNTIME(JI) - XDAY
      ELSE
        ZTIME0(JI) = PSUNTIME(JI)
      END IF
      CALL ADD_FORECAST_TO_DATE_SURF(IYEAR(JI,1),IMONTH(JI,1),IDAY(JI,1),ZTIME0(JI))
      CALL SUBSTRACT_TO_DATE_SURF   (IYEAR(JI,1),IMONTH(JI,1),IDAY(JI,1),ZTIME0(JI))
    ENDDO
    
  CASE ('LEGAL')
    ZTIME0(:)=PSIMTIME + CHN%XDELTA_LEGAL_TIME(:) * 3600.
    DO JI=1,KSIZE
      CALL ADD_FORECAST_TO_DATE_SURF(IYEAR(JI,1),IMONTH(JI,1),IDAY(JI,1),ZTIME0(JI))
      CALL SUBSTRACT_TO_DATE_SURF   (IYEAR(JI,1),IMONTH(JI,1),IDAY(JI,1),ZTIME0(JI))
    ENDDO

END SELECT
!
 CALL DAY_OF_WEEK_CH(IDAY(:,1), IMONTH(:,1), IYEAR(:,1), IDOW(:,1))
!
IHOUR(:,1) = INT((ZTIME0(:)+1.E-10)/3600.)! 1.E-10 and the where condition after are
WHERE (IHOUR(:,1)==24) IHOUR(:,1)=23      ! set to avoid computer precision problems
ZTIME(:,1) =  IHOUR(:,1)    * 3600.
!
!*   2.2 time at the end       of current hour
!
IDAY  (:,2)=IDAY  (:,1)
IMONTH(:,2)=IMONTH(:,1)
IYEAR (:,2)=IYEAR (:,1)
!
ZTIME(:,2) = (IHOUR(:,1)+1) * 3600.
!
DO JI=1,KSIZE
  CALL ADD_FORECAST_TO_DATE_SURF(IYEAR(JI,2),IMONTH(JI,2),IDAY(JI,2),ZTIME(JI,2))
ENDDO
!
 CALL DAY_OF_WEEK_CH(IDAY(:,2), IMONTH(:,2), IYEAR(:,2), IDOW(:,2))
!
IHOUR(:,2)=NINT(ZTIME(:,2))/3600
!
!------------------------------------------------------------------------------
!
!*  3.  Emission at the begining of the current hour
!       --------------------------------------------
!
 CHN%XEMIS_FIELDS(:,:)=0.
!
DO JSPEC=1,CHN%NEMIS_NBR
  !
  ZE(:,:) = 0.
  !
  DO JSNAP=1,CHN%NEMIS_SNAP
    !
    DO JT=1,2
      !
      DO JI=1,KSIZE
        !
        ZE(JI,JT) = ZE(JI,JT) +  CHN%XEMIS_FIELDS_SNAP(JI,JSNAP,JSPEC) &
                      *CHN%XSNAP_MONTHLY(IMONTH(JI,JT)  ,JSNAP,JSPEC) &
                      *CHN%XSNAP_DAILY  (IDOW  (JI,JT)  ,JSNAP,JSPEC) &
                      *CHN%XSNAP_HOURLY (IHOUR (JI,JT)+1,JSNAP,JSPEC) &
                      *CHN%XCONVERSION(JI)
      ENDDO
      !
    ENDDO
    !
  ENDDO
!
!*  5.  Temporal interpolation within the current hour
!       ----------------------------------------------
!
  CHN%XEMIS_FIELDS(:,JSPEC) = ZE(:,1) + (ZE(:,2)-ZE(:,1))/3600.*(ZTIME0(:)-IHOUR(:,1)*3600.)

END DO
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_SNAP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE DAY_OF_WEEK_CH(DATE, MONTH, YEAR, DOW)
!!    AUTHOR
!!    ------
!!    J.Arteta 
!!    Original   August 2010
!!
!!
!!    MODifICATIONS
!!    -------------
!!    S. Queguiner 10/2011  DAY:Monday->Sunday => DOW:1->7
!!    A. Alias     07/2016  gmkpack problem : name of the internal subroutine modified 
!!                          because exist already (view SURFEX/day_of_week.F90 )
!!
!
IMPLICIT NONE
INTEGER, DIMENSION(:), INTENT(IN) :: DATE, MONTH, YEAR
INTEGER, DIMENSION(:), INTENT(OUT):: DOW
INTEGER, DIMENSION(SIZE(DOW))     :: DAY, YR, MN, N1, N2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_SNAP_N:DAY_OF_WEEK_CH',0,ZHOOK_HANDLE)
!
YR = YEAR
MN = MONTH
!
WHERE (MN.LE.2)
  MN = MN + 12
  YR = YR -1
END WHERE
!
N1 = (26 * (MN + 1)) /10
N2 = (125 * YR) / 100
DAY = (DATE + N1 + N2 - (YR / 100) + (YR / 400) - 1)
!
DOW = MOD(DAY,7) + 7
WHERE (DOW.GT.7) DOW = DOW - 7
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_SNAP_N:DAY_OF_WEEK_CH',1,ZHOOK_HANDLE)
END SUBROUTINE DAY_OF_WEEK_CH
!
END SUBROUTINE CH_EMISSION_SNAP_n
