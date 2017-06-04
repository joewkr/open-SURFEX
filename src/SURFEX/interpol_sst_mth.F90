!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_SST_MTH (S,HFLAG)
!     #######################################################
!
!!****  *INTERPOL_SST_MTH* - Interpolation of monthly SST, SSS, SIT or SIC
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
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
!!    AUTHOR
!!    ------
!!      
!     B.Decharme  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/01/10
!!      Modified    02/2014   S. Senesi : allow to work on SSS, SIT and SIC fields
!!      Modified    07/2015   B. Decharme : new linear interpolation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!------------------------
! 
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
CHARACTER(LEN=1), INTENT(IN) :: HFLAG  ! 'T' for SST, 'S' for SSS, 'H' for SIT, 'C' for SIC

!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL            :: ZDAT   ! current day in the current month
REAL            :: ZNDAT  ! number of days in the current month
INTEGER         :: IMTH0  ! previous month
INTEGER         :: IMTH1  ! current month 
INTEGER         :: IMTH2  ! next month
INTEGER         :: INDAYS ! number of days in KMONTH
!
INTEGER         :: IDELTA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.    Number of days in a month
!              -------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_SST_MTH',0,ZHOOK_HANDLE)
IF ( (HFLAG/='S') .AND. (HFLAG/='T') .AND. (HFLAG/='H') .AND. (HFLAG/='C') )THEN
   CALL ABOR1_SFX('FATAL ERROR in INTERPOL_SST_MTH : HFLAG not S nor T nor C nor H. !')
ENDIF
SELECT CASE (S%TTIME%TDATE%MONTH)
    CASE(4,6,9,11)
      INDAYS=30
    CASE(1,3,5,7:8,10,12)
      INDAYS=31
    CASE(2)
      IF( ((MOD(S%TTIME%TDATE%YEAR,4)==0).AND.(MOD(S%TTIME%TDATE%YEAR,100)/=0)) &
          .OR. (MOD(S%TTIME%TDATE%YEAR,400)==0))THEN
        INDAYS=29
      ELSE
        INDAYS=28
      ENDIF
END SELECT
!
!
!-------------------------------------------------------------------------------
!
!*       2.    SST or SSS  Interpolation using previous, current and next month
!              --------------------------------------------------------
!
ZDAT = REAL(S%TTIME%TDATE%DAY)
ZNDAT= REAL(INDAYS)
!
! The current month correspond to the indice 2 (or 3 if next month)
!
IF (S%TTIME%TDATE%MONTH==S%TZTIME%TDATE%MONTH) THEN 
   IDELTA=0
ELSE
   IDELTA=1
END IF
!
IMTH0=1+IDELTA
IMTH1=2+IDELTA
IMTH2=3+IDELTA
!
IF (HFLAG =='T') THEN 
   CALL INTERPOL_LOCAL(S%CINTERPOL_SST,S%XSST_MTH(:,IMTH0),S%XSST_MTH(:,IMTH1),S%XSST_MTH(:,IMTH2),S%XSST)
ELSEIF (HFLAG =='S') THEN 
   CALL INTERPOL_LOCAL(S%CINTERPOL_SSS,S%XSSS_MTH(:,IMTH0),S%XSSS_MTH(:,IMTH1),S%XSSS_MTH(:,IMTH2),S%XSSS)
   S%XSSS(:) = MAX(0.0,S%XSSS(:))
ELSEIF (HFLAG =='H') THEN 
   CALL INTERPOL_LOCAL(S%CINTERPOL_SIT,S%XSIT_MTH(:,IMTH0),S%XSIT_MTH(:,IMTH1),S%XSIT_MTH(:,IMTH2),S%XFSIT)
   S%XFSIT(:) = MAX(0.0,S%XFSIT(:))
ELSEIF (HFLAG =='C') THEN
   CALL INTERPOL_LOCAL(S%CINTERPOL_SIC,S%XSIC_MTH(:,IMTH0),S%XSIC_MTH(:,IMTH1),S%XSIC_MTH(:,IMTH2),S%XFSIC)
   S%XFSIC(:) = MAX(0.0,MIN(1.0,S%XFSIC(:)))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_SST_MTH',1,ZHOOK_HANDLE)
!
!=======================================================================================
!
CONTAINS
!
!=======================================================================================
!
SUBROUTINE INTERPOL_LOCAL(HMETHOD,PMTH0,PMTH1,PMTH2,POUT)
!
USE MODI_INTERPOL_QUADRA
USE MODI_INTERPOL_LINEAR
!
IMPLICIT NONE
!
CHARACTER(LEN=6),    INTENT(IN) :: HMETHOD
REAL, DIMENSION(:) , INTENT(IN) :: PMTH0
REAL, DIMENSION(:) , INTENT(IN) :: PMTH1
REAL, DIMENSION(:) , INTENT(IN) :: PMTH2
REAL, DIMENSION(:), INTENT(OUT) :: POUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_SST_MTH:INTERPOL_LOCAL',0,ZHOOK_HANDLE)
!
IF(HMETHOD=='QUADRA')THEN
  CALL INTERPOL_QUADRA(ZDAT,ZNDAT,PMTH0,PMTH1,PMTH2,POUT)
ELSEIF(HMETHOD=='LINEAR')THEN
  CALL INTERPOL_LINEAR(ZDAT,ZNDAT,PMTH0,PMTH1,PMTH2,POUT)
ELSEIF(HMETHOD=='UNIF')THEN
  POUT(:) = PMTH1(:)
ELSE
  CALL ABOR1_SFX('INTERPOL_SST_MTH:INTERPOL_LOCAL: interpolation method not supported')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_SST_MTH:INTERPOL_LOCAL',1,ZHOOK_HANDLE)
!
END SUBROUTINE INTERPOL_LOCAL
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_SST_MTH
