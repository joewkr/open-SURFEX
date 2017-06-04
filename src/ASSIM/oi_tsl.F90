!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_TSL(KDAT,KSSSSS,PLAT,PLON,PMU0,PMU0M,KH)
!-----------------------------------------------------------------------
!
!     Computation of solar zenith angle 
!     ---------------------------------
!
!     INPUT PARAMETERS :
!
!     IDAT    : DATE in the following form => YYYYMMDD
!     NSSSSS  : TIME of the day in seconds
!     PLAT    : LATITUDE  in Degrees
!     PLON    : LONGITUDE in Degrees
!
!     OUTPUT PARAMETERS :
!
!     PMU0    : Cosine of solar zenith angle
!     PMU0M   : Cosine of solar zenith angle (mean value)
!     IH      : local time (hour)
!
!
!     J.F. Mahfouf (4/12/97) from IFS/ARPEGE routines
!                 
!
!     23/05/2009 : Fortran 90 recoding (IMPLICIT NONE + FUNCTIONS)
!
!-----------------------------------------------------------------------
!
! - Astronomical functions
!   you will find the description in the annex 1 of the documentation
!   RRS is the distance Sun-Earth
!   RDS is the declination of the Earth
!   RET is the equation of time
! 
USE MODD_CSTS,  ONLY : XPI
USE MODD_ASSIM, ONLY : XREPSM, XRCDTR, NITRAD
!
!
USE YOMHOOK,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: KDAT
INTEGER, INTENT(IN)  :: KSSSSS 
REAL, INTENT(IN)     :: PLAT, PLON
REAL, INTENT(OUT)    :: PMU0, PMU0M
INTEGER, INTENT(OUT) :: KH
!
REAL :: ZGEMU, ZGELAM, ZTIME, ZTETA, ZRDECLI, ZREQTIM, ZRHGMT, ZRSOVR, &
        ZRWSOVR, ZRCODEC, ZRSIDEC, ZRCOVSR, ZRSIVSR, ZRTIMTRM, ZTETAM,   &
        ZRDECLIM, ZREQTIMM, ZRHGMTM, ZRSOVRM, ZRWSOVRM, ZRCODECM,        &
        ZRSIDECM, ZRCOVSRM, ZRSIVSRM, ZT  
INTEGER :: ID, IM, IA, INSSSSS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!  Angle conversions 
!      
IF (LHOOK) CALL DR_HOOK('OI_TSL',0,ZHOOK_HANDLE)
!
ZGEMU  = SIN(PLAT*XPI/180.)      ! sinus of latitude
ZGELAM = PLON*XPI/180.          ! longitude
!
ID = MOD(KDAT,100)
IM = MOD((KDAT-ID)/100,100)
IA = KDAT/10000

ZTIME = RTIME(IA,IM,ID,KSSSSS)
!
CALL GET_MU0(ZGEMU,ZGELAM,ZTIME,KSSSSS,PMU0)
!
!
!     Mean angle over the previous 6 hours
!     ------------------------------------
!  
ZTIME = RTIME(IA,IM,ID,KSSSSS-NITRAD)
!
IF ( (KSSSSS-NITRAD).LT.0 ) THEN
  INSSSSS = KSSSSS + 86400
ELSE
  INSSSSS = KSSSSS
ENDIF  
!
CALL GET_MU0(ZGEMU,ZGELAM,ZTIME,INSSSSS-NITRAD,PMU0M)
!
! 
!  Local time in hours
!  Should be inside [1,24]
!
ZT = (KSSSSS + PLON*XRCDTR*3600.)/3600.
IF (ZT < 0.0) THEN
  ZT = ZT + 24.
ELSEIF (ZT > 24.0) THEN
  ZT = ZT - 24.
ENDIF
!
KH = INT(ZT)
IF ( KH==0 ) KH = 24 
!
IF (LHOOK) CALL DR_HOOK('OI_TSL',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE GET_MU0(PGEMU,PGELAM,PTIME,KSSSSS,PMU)
!
USE MODD_CSTS,  ONLY : XDAY
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PGEMU
REAL, INTENT(IN) :: PGELAM
REAL, INTENT(IN) :: PTIME
INTEGER, INTENT(IN) :: KSSSSS
REAL, INTENT(OUT) :: PMU
!
REAL :: ZTETA, ZDECLI, ZEQTIM, ZHGMT, ZWSOVR
REAL :: ZINTER
!
ZTETA   = PTIME/(XDAY*365.25)
!
ZDECLI = RDS(ZTETA)             ! declinaison
!
ZEQTIM = RET(ZTETA)
ZHGMT  = REAL( MOD(KSSSSS,NINT(XDAY)) )
ZWSOVR = (ZEQTIM + ZHGMT) * 2. * XPI/XDAY      ! hour angle
!
ZINTER = SQRT(1.-PGEMU**2)
PMU = MAX( SIN(ZDECLI) * PGEMU + COS(ZDECLI) * ZINTER * &
           ( SIN(ZWSOVR)*SIN(PGELAM) - COS(ZWSOVR)*COS(PGELAM) ) , 0. ) 
!
IF (PMU.GT.0.) PMU = SQRT(1224.*PMU*PMU +1.)/35. ! Magnification factor 
!
END SUBROUTINE GET_MU0
!
FUNCTION RDS(PT)
 
USE MODD_ASSIM, ONLY : XREPSM

IMPLICIT NONE

REAL, INTENT(IN) :: PT
REAL             :: RDS, ZLS, ZEL
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('RDS',0,ZHOOK_HANDLE)
!
ZEL = 1.7535 + 6.283076 * PT
ZLS = 4.8952 + 6.283320 * PT &
     - 0.0075*SIN(ZEL) - 0.0326*COS(ZEL) &
     - 0.0003*SIN(2.*ZEL) + 0.0002*COS(2.*ZEL)  
!
RDS   = ASIN(SIN(XREPSM)*SIN(ZLS))
!
IF (LHOOK) CALL DR_HOOK('RDS',1,ZHOOK_HANDLE)

END FUNCTION RDS
! 
FUNCTION RET(PT)
 
IMPLICIT NONE

REAL, INTENT(IN) :: PT
REAL             :: RET, ZEM, ZLS
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('RET',0,ZHOOK_HANDLE)

ZEM  = 6.240075 + 6.283020 * PT
ZLS  = 4.8951   + 6.283076 * PT
RET  =  591.8*SIN(2.*ZLS) - 12.7 * SIN(4.*ZLS) &
      - 459.4*SIN(ZEM) - 4.8 *SIN(2.*ZEM) &
      + 39.5 * SIN(ZEM) * COS(2.*ZLS) 
          
IF (LHOOK) CALL DR_HOOK('RET',1,ZHOOK_HANDLE)

END FUNCTION RET
!
FUNCTION RTIME(KA,KM,KD,KS)

USE MODD_CSTS, ONLY : XDAY

IMPLICIT NONE 

INTEGER, INTENT(IN) :: KA, KM, KD, KS
REAL                :: RTIME, ZJ
INTEGER             :: IA, IM
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('RTIME',0,ZHOOK_HANDLE)

IM  = KM + 6*(1-ISIGN(1,KM-3))
IA  = KA - ( (1-ISIGN(1,KM-3))/2 )
!
ZJ = 1720994.5 + &
     FLOAT( 2 - IA/100 + (IA/100)/4 + &
           INT(365.25*FLOAT(IA)) + INT(30.601*FLOAT(IM+1)) + KD )  
!
RTIME  = (ZJ-2451545.)*XDAY + FLOAT(KS)
!
IF (LHOOK) CALL DR_HOOK('RTIME',1,ZHOOK_HANDLE)

END FUNCTION RTIME

END SUBROUTINE OI_TSL




