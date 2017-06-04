!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE SST_UPDATE (DTS, S, PSST)
!   ###############################################################
!!****  *SST_UPDATE*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of sst
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Le Moigne          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODI_TEMPORAL_DISTS
USE MODI_TEMPORAL_LTS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PSST    ! sst
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: JXP
REAL, DIMENSION(SIZE(PSST))              :: ZSST, ZSST0
REAL                                     :: ZSDTJX
REAL                                     :: ZDT, ZALPHA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SST_UPDATE',0,ZHOOK_HANDLE)
!
IF (.NOT.S%LTZTIME_DONE) THEN
   S%LTZTIME_DONE = .TRUE.
   S%JSX = 1
   S%TZTIME%TDATE%YEAR  = S%TTIME%TDATE%YEAR
   S%TZTIME%TDATE%MONTH = S%TTIME%TDATE%MONTH
   S%TZTIME%TDATE%DAY   = S%TTIME%TDATE%DAY
   S%TZTIME%TIME        = S%TTIME%TIME
ENDIF
!
ZSST0(:) = S%XSST_INI(:)
!
IF ( TEMPORAL_LTS ( S%TTIME, DTS%TDATA_SST(1) ) ) THEN
  !
  CALL TEMPORAL_DISTS ( DTS%TDATA_SST(1)%TDATE%YEAR,DTS%TDATA_SST(1)%TDATE%MONTH, &
                        DTS%TDATA_SST(1)%TDATE%DAY ,DTS%TDATA_SST(1)%TIME,        &
                        S%TZTIME%TDATE%YEAR   ,S%TZTIME%TDATE%MONTH,          &
                        S%TZTIME%TDATE%DAY    ,S%TZTIME%TIME,                 &
                        ZSDTJX                                            )  
  !
  CALL TEMPORAL_DISTS ( S%TTIME%TDATE%YEAR   ,S%TTIME%TDATE%MONTH,           &
                        S%TTIME%TDATE%DAY    ,S%TTIME%TIME,                  &
                        S%TZTIME%TDATE%YEAR  ,S%TZTIME%TDATE%MONTH,          &
                        S%TZTIME%TDATE%DAY   ,S%TZTIME%TIME,                 &
                        ZDT                                              )  
  !
  ZALPHA = ZDT / ZSDTJX
  !
  ZSST(:)= ZSST0(:)+(DTS%XDATA_SST(:,1)-ZSST0(:))*ZALPHA
  !
ELSE IF ( .NOT. TEMPORAL_LTS ( S%TTIME, DTS%TDATA_SST(DTS%NTIME) ) ) THEN
  !
  ZSST(:) = DTS%XDATA_SST(:,DTS%NTIME)
  !
ELSE
  !
  DO
    JXP = S%JSX + 1
    IF ( TEMPORAL_LTS( S%TTIME, DTS%TDATA_SST(JXP)) ) EXIT
    S%JSX = S%JSX + 1
  ENDDO
  !  
  CALL TEMPORAL_DISTS ( DTS%TDATA_SST(JXP)%TDATE%YEAR,DTS%TDATA_SST(JXP)%TDATE%MONTH,   &
                        DTS%TDATA_SST(JXP)%TDATE%DAY ,DTS%TDATA_SST(JXP)%TIME,          &
                        DTS%TDATA_SST(S%JSX)%TDATE%YEAR ,DTS%TDATA_SST(S%JSX)%TDATE%MONTH,  &
                        DTS%TDATA_SST(S%JSX)%TDATE%DAY  ,DTS%TDATA_SST(S%JSX)%TIME,         &
                        ZSDTJX                                            )  
  !
  CALL TEMPORAL_DISTS ( S%TTIME%TDATE%YEAR   ,S%TTIME%TDATE%MONTH,                  &
                        S%TTIME%TDATE%DAY    ,S%TTIME%TIME,                         &
                        DTS%TDATA_SST(S%JSX)%TDATE%YEAR,DTS%TDATA_SST(S%JSX)%TDATE%MONTH,   &
                        DTS%TDATA_SST(S%JSX)%TDATE%DAY ,DTS%TDATA_SST(S%JSX)%TIME,          &
                        ZDT                                             )  
  !
  ZALPHA = ZDT / ZSDTJX

  !
  ZSST(:)= DTS%XDATA_SST(:,S%JSX)+(DTS%XDATA_SST(:,JXP)-DTS%XDATA_SST(:,S%JSX))*ZALPHA
  !                
END IF
!
PSST(:) = ZSST(:)
!
IF (LHOOK) CALL DR_HOOK('SST_UPDATE',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE SST_UPDATE
