!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE INIT_PGD_SURF_ATM (YSC, HPROGRAM,HINIT,HATMFILE,HATMFILETYPE, &
                               KYEAR, KMONTH, KDAY, PTIME            )  
!     #################################################################################
!
!!****  *INIT_PGD_SURF_ATM* - Call surface initialization for PGD fields only
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme  04/2013 new coupling variables
!!------------------------------------------------------------------
!
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_INIT_SURF_ATM_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! fields to initialize 'ALL', 'PRE', 'PGD'
 CHARACTER(LEN=28), INTENT(IN)   :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),  INTENT(IN)   :: HATMFILETYPE! type of the Atmospheric file
INTEGER,           INTENT(IN)   :: KYEAR       ! year
INTEGER,           INTENT(IN)   :: KMONTH      ! month
INTEGER,           INTENT(IN)   :: KDAY        ! day
REAL,              INTENT(IN)   :: PTIME       ! time
!
!
!*      0.2    declarations of local variables
!
TYPE(DATE) :: TDATE_END
 CHARACTER(LEN=6), DIMENSION(0)  :: YSV       ! name of all scalar variables
REAL,             DIMENSION(0)  :: ZCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(0)  :: ZRHOA     ! air density (kg/m3)
REAL,             DIMENSION(0)  :: ZZENITH   ! solar zenithal angle
REAL,             DIMENSION(0)  :: ZAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(1)  :: ZSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(0,1):: ZDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(0,1):: ZSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(0)  :: ZEMIS     ! emissivity
REAL,             DIMENSION(0)  :: ZTSRAD    ! radiative temperature
REAL,             DIMENSION(0)  :: ZTSURF    ! radiative temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!* initialization of PGD fields of output domain
!
IF (LHOOK) CALL DR_HOOK('INIT_PGD_SURF_ATM',0,ZHOOK_HANDLE)
!
TDATE_END%YEAR = KYEAR
TDATE_END%MONTH = KMONTH
TDATE_END%DAY = KDAY
!
 CALL INIT_SURF_ATM_n(YSC, HPROGRAM,HINIT,.FALSE.,               &
                      0,0,1,YSV,ZCO2,ZRHOA,                      &
                      ZZENITH,ZAZIM,ZSW_BANDS,ZDIR_ALB,ZSCA_ALB, &
                      ZEMIS,ZTSRAD,ZTSURF,                       &
                      KYEAR, KMONTH, KDAY, PTIME, TDATE_END,     &
                      HATMFILE,HATMFILETYPE, 'OK'                )  
!
IF (LHOOK) CALL DR_HOOK('INIT_PGD_SURF_ATM',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE INIT_PGD_SURF_ATM
