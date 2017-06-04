!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE SUBSTRACT_TO_DATE_SURF(KYEAR,KMONTH,KDAY,PSEC)
!     #######################################################
!
!!****  *SUBSTRACT_TO_DATE_SURF* - finds the current date and hour from a date
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
!!      A recursive method is used, removing one day ofter the other.
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
!!    Book 2 (add_forecast_to_date)
!!
!!    AUTHOR
!!    ------
!!
!     G.Jaubert Meteo-France (from add_forecast_to_date)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/07/01
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
INTEGER, INTENT(INOUT) :: KYEAR  ! year of date
INTEGER, INTENT(INOUT) :: KMONTH ! month of date
INTEGER, INTENT(INOUT) :: KDAY   ! day of date
REAL,    INTENT(INOUT) :: PSEC   ! number of seconds since date at 00 UTC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SUBSTRACT_TO_DATE_SURF',0,ZHOOK_HANDLE)
!
!*       1.    Return condition: PSEC >0
!              -------------------------
!
DO 
  IF (PSEC >= 0.) EXIT
!
!-------------------------------------------------------------------------------
!
!*       2.    remove one day
!              --------------
!
  PSEC=PSEC+86400.
!
!
!*       2.1   first day of the month
!              ---------------------
!
  IF (KDAY==1) THEN
    IF (KMONTH==1) THEN
      KDAY=31
      KMONTH=12
      KYEAR=KYEAR-1
    ELSE
      KMONTH=KMONTH-1
      SELECT CASE (KMONTH)
        CASE(4,6,9,11)
          KDAY=30
        CASE(1,3,5,7:8,10,12)
          KDAY=31
        CASE(2)
          IF( ((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)) .OR. (MOD(KYEAR,400)==0))THEN
            KDAY=29
          ELSE
            KDAY=28
          ENDIF
      END SELECT
    ENDIF
! 
!*       2.2   Other days
!              ----------
  ELSE
    KDAY=KDAY-1
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.    Recursive call
!              --------------
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SUBSTRACT_TO_DATE_SURF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUBSTRACT_TO_DATE_SURF
