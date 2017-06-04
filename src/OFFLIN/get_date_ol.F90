!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_DATE_OL(TPTIME,PTSTEP,HDATE)
!     #######################################################
!!****  *GET_DATE_OL* - gets the initial date of the simulation to write in
!                       netcdf file
!!
!!    PURPOSE
!!    -------
!!
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
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010 
!-------------------------------------------------------------------------------
!
USE MODD_TYPE_DATE_SURF, ONLY: DATE_TIME
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(DATE_TIME), INTENT(IN)      :: TPTIME      ! current date and time
REAL,            INTENT(IN)      :: PTSTEP
 CHARACTER(LEN=*), INTENT(OUT)    :: HDATE
!
INTEGER, DIMENSION(3)            :: ITIME, IDATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('GET_DATE_OL',0,ZHOOK_HANDLE)
ITIME(1)=FLOOR(TPTIME%TIME/3600.)
ITIME(2)=FLOOR((TPTIME%TIME-ITIME(1)*3600)/60.)
ITIME(3)=TPTIME%TIME-ITIME(1)*3600-ITIME(2)*60
!
IF (PTSTEP == FLOOR(PTSTEP/86400.)*86400) THEN 
  HDATE='days since '
ELSEIF (PTSTEP == FLOOR(PTSTEP/3600.)*3600) THEN
  HDATE='hours since '
ELSEIF (PTSTEP == FLOOR(PTSTEP/60.)*60) THEN
  HDATE='minutes since '
ELSE
  HDATE='seconds since '
ENDIF
!
IDATE(1) = TPTIME%TDATE%YEAR
IDATE(2) = TPTIME%TDATE%MONTH
IDATE(3) = TPTIME%TDATE%DAY
!
 CALL WRITE_TIME(IDATE(1),1,"-",HDATE)
 CALL WRITE_TIME(IDATE(2),0,"-",HDATE)
 CALL WRITE_TIME(IDATE(3),0,"",HDATE)
 CALL WRITE_TIME(ITIME(1),1,":",HDATE)
 CALL WRITE_TIME(ITIME(2),0,":",HDATE)
 CALL WRITE_TIME(ITIME(3),0,"",HDATE)

IF (LHOOK) CALL DR_HOOK('GET_DATE_OL',1,ZHOOK_HANDLE)
CONTAINS


SUBROUTINE WRITE_TIME(ITIME,ISPACE,HSEP,HTDATE)
!
INTEGER, INTENT(IN)             :: ITIME
INTEGER, INTENT(IN)             :: ISPACE
 CHARACTER(LEN=*), INTENT(IN)    :: HSEP
 CHARACTER(LEN=*), INTENT(INOUT) :: HTDATE
 CHARACTER(LEN=10)               :: YPAS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!      
!
IF (LHOOK) CALL DR_HOOK('GET_DATE_OL:WRITE_TIME',0,ZHOOK_HANDLE)
IF (ITIME.LT.10) THEN
  WRITE(YPAS,'(i1)') ITIME
  IF (ISPACE==1) THEN
    HTDATE=trim(HTDATE)//" 0"//trim(YPAS)//HSEP
  ELSE
    HTDATE=trim(HTDATE)//"0"//trim(YPAS)//HSEP
  ENDIF
ELSE
  IF (ITIME.LT.100) THEN
    WRITE(YPAS,'(i2)') ITIME
  ELSE
    WRITE(YPAS,'(i4)') ITIME
  ENDIF
  IF (ISPACE==1) THEN
    HTDATE=trim(HTDATE)//" "//trim(YPAS)//HSEP
  ELSE
    HTDATE=trim(HTDATE)//trim(YPAS)//HSEP
  ENDIF  
ENDIF
IF (LHOOK) CALL DR_HOOK('GET_DATE_OL:WRITE_TIME',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_TIME


END SUBROUTINE GET_DATE_OL
