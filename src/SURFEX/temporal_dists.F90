!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TEMPORAL_DISTS(KYEARF, KMONTHF, KDAYF, PSECF,     &
                                 KYEARI, KMONTHI, KDAYI, PSECI,     &
                                 PDIST                              )  
!     #############################################################
!
!!****  *TEMPORAL_DISTS* - finds the number of secunds between 2 dates
!!
!!    PURPOSE
!!    -------
!!
!!                                WARNING
!!
!!      -----> Only correct for dates between 19900301 and 21000228   <-----
!!
!!  The correct test should be:
!! IF( ((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)) .OR. (MOD(KYEAR,400)==0))THEN
!!
!!**  METHOD
!!    ------
!!
!!      A comparison term by term of the elements of the 2 dates is performed.
!!    and the temporal distance between the 2 dates is then deduced.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Book 2
!!
!!    AUTHOR
!!    ------
!!
!     J.Stein  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/01/96
!!    PP. 06/08: Add case where differents years and same month 
!!    for more-than-1year simulations
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
INTEGER, INTENT(IN) :: KYEARF  ! year of Final date
INTEGER, INTENT(IN) :: KMONTHF ! month of Final date
INTEGER, INTENT(IN) :: KDAYF   ! day of Final date
REAL,    INTENT(IN) :: PSECF   ! number of seconds since date at 00 UTC
                               ! of Final date
INTEGER, INTENT(IN) :: KYEARI  ! year of Initial date
INTEGER, INTENT(IN) :: KMONTHI ! month of Initial date
INTEGER, INTENT(IN) :: KDAYI   ! day of Initial date
REAL,    INTENT(IN) :: PSECI   ! number of seconds since date at 00 UTC
                               ! of Initial date
REAL,    INTENT(OUT):: PDIST   ! temporal distance in secunds between the final 
                               ! and initial date
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IDAYS  ! number of days between the two dates
INTEGER :: JMONTH,JYEAR ! loop index on months or years 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    SAME YEARS AND SAME MONTHS
!              --------------------------
!
IF (LHOOK) CALL DR_HOOK('TEMPORAL_DISTS',0,ZHOOK_HANDLE)
IF ( (KYEARF==KYEARI) .AND. (KMONTHF==KMONTHI) ) THEN
  PDIST = ( KDAYF-KDAYI) * 86400. + PSECF - PSECI
  ! check chronological order
  IF (PDIST < 0.) PDIST=XUNDEF
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    SAME YEARS AND DIFFERENT MONTHS
!              -------------------------------
!
IF ( (KYEARF==KYEARI) .AND. (KMONTHF/=KMONTHI) ) THEN
  ! check chronological order
  IF ( KMONTHF < KMONTHI ) THEN
    PDIST=XUNDEF
    IF (LHOOK) CALL DR_HOOK('TEMPORAL_DISTS',1,ZHOOK_HANDLE)
    RETURN
  END IF
  !
  ! cumulate the number of days for the months in between KMONTHF-1 and 
  ! KMONTHI
  IDAYS = 0
  DO JMONTH = KMONTHI, KMONTHF-1
    SELECT CASE (JMONTH)
      CASE(4,6,9,11)
        IDAYS=IDAYS+30
      CASE(1,3,5,7:8,10,12)
        IDAYS=IDAYS+31
      CASE(2)
        IF (MOD(KYEARI,4)==0) THEN 
          IDAYS=IDAYS+29
        ELSE
          IDAYS=IDAYS+28
        ENDIF
    END SELECT
  END DO  
  !
  ! compute the temporal distance
  PDIST = ( IDAYS + KDAYF - KDAYI) * 86400. + PSECF - PSECI
  !
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.    DIFFERENT YEARS AND DIFFERENT MONTHS
!              ------------------------------------
!
IF ( (KYEARF/=KYEARI) .AND. (KMONTHF/=KMONTHI) ) THEN
  ! check chronological order
  IF ( KYEARF < KYEARI ) THEN
    PDIST=XUNDEF
    IF (LHOOK) CALL DR_HOOK('TEMPORAL_DISTS',1,ZHOOK_HANDLE)
    RETURN
  END IF
  !
  ! cumulate the number of days for the months in between KMONTHI and 
  ! December
  IDAYS = 0
  DO JMONTH = KMONTHI, 12
    SELECT CASE (JMONTH)
      CASE(4,6,9,11)
        IDAYS=IDAYS+30
      CASE(1,3,5,7:8,10,12)
        IDAYS=IDAYS+31
      CASE(2)
        IF (MOD(KYEARI,4)==0) THEN 
          IDAYS=IDAYS+29
        ELSE
          IDAYS=IDAYS+28
        ENDIF
    END SELECT
  END DO  
  DO JMONTH = 1,KMONTHF-1
    SELECT CASE (JMONTH)
      CASE(4,6,9,11)
        IDAYS=IDAYS+30
      CASE(1,3,5,7:8,10,12)
        IDAYS=IDAYS+31
      CASE(2)
        IF (MOD(KYEARF,4)==0) THEN 
          IDAYS=IDAYS+29
        ELSE
          IDAYS=IDAYS+28
        ENDIF
    END SELECT
  END DO  
  ! add the number of days corresponding to full years between the two dates
  DO JYEAR=KYEARI+1, KYEARF-1
    IF (MOD(JYEAR,4)==0) THEN 
      IDAYS=IDAYS+366
    ELSE
      IDAYS=IDAYS+365
    END IF
  END DO
  !
  ! compute the temporal distance
  PDIST = ( IDAYS + KDAYF - KDAYI) * 86400. + PSECF - PSECI
  !
END IF
!
!
!!            4. SUPPLEMENTARY CASE FOR DIFFERENT YEARS AND SAME MONTH 
!           ------------------------------------------------------------
IF ( (KYEARF/=KYEARI) .AND. (KMONTHF==KMONTHI) ) THEN
  ! check chronological order
  IF ( KYEARF < KYEARI ) THEN
    PDIST=XUNDEF
    IF (LHOOK) CALL DR_HOOK('TEMPORAL_DISTS',1,ZHOOK_HANDLE)
    RETURN
  END IF
  !
  ! cumulate the number of days for the months in between KMONTHI and 
  ! December => IDAYS = 0 here
  IDAYS = 0
  ! add the number of days corresponding to full years between the two dates
  DO JYEAR=KYEARI+1, KYEARF-1
    IF (MOD(JYEAR,4)==0) THEN 
      IDAYS=IDAYS+366
    ELSE
      IDAYS=IDAYS+365
    END IF
  END DO
  !
  ! compute the temporal distance
  PDIST = ( IDAYS + KDAYF - KDAYI) * 86400. + PSECF - PSECI
  !
END IF
IF (LHOOK) CALL DR_HOOK('TEMPORAL_DISTS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEMPORAL_DISTS
