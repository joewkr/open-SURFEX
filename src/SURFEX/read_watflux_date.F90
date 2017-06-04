!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_WATFLUX_DATE (HPROGRAM,HINIT,KLUOUT,HATMFILE,HATMFILETYPE,&
                                    KYEAR,KMONTH,KDAY,PTIME,TPTIME              )  
!     #######################################################
!
!!****  *READ_WATFLUX_DATE* - initializes the date TTIME of MODD_WATFLUX
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      P. Le Moigne 10/2005, Phasage Arome
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_TYPE_DATE_SURF
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_READ_PRE_WATF_DAT_CONF
USE MODI_READ_PRE_SURFA_DAT_CONF
USE MODI_READ_PREP_WATFLUX_CONF
USE MODI_READ_PREP_FILE_DATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HINIT     ! fields to initialize 'ALL', 'PRE', 'PGD'
 CHARACTER(LEN=28), INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HATMFILETYPE! atmospheric file type
INTEGER,           INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,           INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,           INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,              INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
TYPE (DATE_TIME),  INTENT(OUT) ::TPTIME    ! time and date
INTEGER,           INTENT(IN)  :: KLUOUT      ! logical unit of output listing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28)              :: YFILE       ! file name
 CHARACTER(LEN=6)               :: YFILETYPE   ! file type
 CHARACTER(LEN=28)              :: YFILEPGD       ! file name
 CHARACTER(LEN=6)               :: YFILEPGDTYPE   ! file type
 CHARACTER(LEN=28)              :: YFILEPGDIN       ! file name
 CHARACTER(LEN=6)               :: YFILEPGDINTYPE   ! file type
!
LOGICAL                        :: GUNIF       ! flag for prescribed uniform field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_DATE',0,ZHOOK_HANDLE)
YFILE     = '                         '
YFILETYPE = '      '
!
YFILEPGDIN     = '                         '
YFILEPGDINTYPE = '      '
!
!-------------------------------------------------------------------------------
!
!* look for a date in the namelist NAM_PREP_WATFLUX or NAM_PREP_SURF_ATM
!-----------------------------------------------------------------------
!
 CALL READ_PRE_WATF_DAT_CONF(HPROGRAM,KLUOUT,TPTIME)
!
IF (TPTIME%TDATE%YEAR==NUNDEF.OR.TPTIME%TDATE%MONTH==NUNDEF &
      .OR.TPTIME%TDATE%DAY==NUNDEF.OR.TPTIME%TIME==XUNDEF) THEN  
  CALL READ_PRE_SURFA_DAT_CONF(HPROGRAM,KLUOUT,TPTIME)
END IF
!
!* If no date in the namelist, look for a file
!  ---------------
!
IF (TPTIME%TDATE%YEAR==NUNDEF.OR.TPTIME%TDATE%MONTH==NUNDEF &
      .OR.TPTIME%TDATE%DAY==NUNDEF.OR.TPTIME%TIME==XUNDEF) THEN  
  !
  CALL READ_PREP_WATFLUX_CONF(HPROGRAM,'DATE   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                              HATMFILE,HATMFILETYPE,YFILEPGDIN,YFILEPGDINTYPE,KLUOUT,GUNIF)
  !
  IF (LEN_TRIM(YFILETYPE)/=0) &
    CALL READ_PREP_FILE_DATE(HPROGRAM,YFILE,YFILETYPE,TPTIME,KLUOUT)  
  !
END IF
!
!* If no file and no date in the namelist, test if atmospheric date
!------------------------------------------------------------------
!
IF (LEN_TRIM(YFILE)==0 .AND. (TPTIME%TDATE%YEAR==NUNDEF.OR.TPTIME%TDATE%MONTH==NUNDEF &
                               .OR.TPTIME%TDATE%DAY==NUNDEF.OR.TPTIME%TIME==XUNDEF)) THEN  
!
  IF (KYEAR /= NUNDEF .AND. KMONTH /= NUNDEF .AND. KDAY /= NUNDEF .AND. PTIME /= XUNDEF) THEN
    TPTIME%TDATE%YEAR = KYEAR
    TPTIME%TDATE%MONTH= KMONTH
    TPTIME%TDATE%DAY  = KDAY
    TPTIME%TIME = PTIME
  ELSE
!
!* If no file, no date in the namelist and no atmospheric date : stop
!-----------------------------------------------------------------------
!
    CALL ABOR1_SFX('READ_WATFLUX_DATE: DATE NOT SET')
  END IF
ENDIF
!
!* Test of date coherence?
!------------------------ 
!
IF (KYEAR /= NUNDEF .AND. KMONTH /= NUNDEF .AND. KDAY /= NUNDEF .AND. PTIME /= XUNDEF) THEN
  IF (KYEAR /= TPTIME%TDATE%YEAR .OR. KMONTH /= TPTIME%TDATE%MONTH .OR. KDAY /= TPTIME%TDATE%DAY .AND. PTIME /= TPTIME%TIME) THEN
     WRITE(UNIT=KLUOUT, FMT=*)'WARNING in READ_WATFLUX_DATE'
     WRITE(UNIT=KLUOUT, FMT=*)'ATMOSPHERIC AND SURFACE DATES ARE NOT THE SAME'

     WRITE(UNIT=KLUOUT, FMT=*)'ATMOSPHERIC DATE:'
     WRITE(UNIT=KLUOUT, FMT='(" YEAR=",I4)') KYEAR
     WRITE(UNIT=KLUOUT, FMT='(" MONTH=",I4)') KMONTH
     WRITE(UNIT=KLUOUT, FMT='(" DAY=",I4)') KDAY
     WRITE(UNIT=KLUOUT, FMT='(" TIME=",E13.6)') PTIME
     WRITE(UNIT=KLUOUT, FMT=*)'SURFACE DATE:'
     WRITE(UNIT=KLUOUT, FMT='(" YEAR=",I4)') TPTIME%TDATE%YEAR
     WRITE(UNIT=KLUOUT, FMT='(" MONTH=",I4)') TPTIME%TDATE%MONTH
     WRITE(UNIT=KLUOUT, FMT='(" DAY=",I4)') TPTIME%TDATE%DAY
     WRITE(UNIT=KLUOUT, FMT='(" TIME=",E13.6)') TPTIME%TIME
  ELSE
     WRITE(UNIT=KLUOUT, FMT=*)'SAME ATMOSPHERIC AND SURFACE DATES'
     WRITE(UNIT=KLUOUT, FMT=*)'DATE in READ_WATFLUX_DATE'
      WRITE(UNIT=KLUOUT, FMT='(" YEAR=",I4," MONTH=",I4," DAY=",I4)') &
                                        KYEAR,KMONTH,KDAY  
     WRITE(UNIT=KLUOUT, FMT='(" TIME=",E13.6)') PTIME
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_DATE',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_WATFLUX_DATE
