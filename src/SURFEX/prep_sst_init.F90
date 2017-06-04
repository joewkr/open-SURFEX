!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE PREP_SST_INIT (DTS, TPTIME, KSX, PSST)
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
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
!
USE MODD_TYPE_DATE_SURF
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
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
!
TYPE(DATE_TIME), INTENT(IN) :: TPTIME
INTEGER, INTENT(INOUT) :: KSX
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PSST    ! sst
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: IDECADE  ! decade of simulation
INTEGER                                  :: JTIME    ! decade of simulation
INTEGER, SAVE                            :: JI
INTEGER                                  :: JXP
REAL, DIMENSION(SIZE(PSST))              :: ZSST
REAL, SAVE                               :: ZSDTJX
REAL                                     :: ZDT, ZALPHA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('PREP_SST_INIT',0,ZHOOK_HANDLE)
LOOP: DO JI = DTS%NTIME-1,1,-1
         KSX = JI
         IF (.NOT.TEMPORAL_LTS(TPTIME,DTS%TDATA_SST(KSX))) EXIT LOOP
      ENDDO LOOP

IF ( TEMPORAL_LTS ( TPTIME, DTS%TDATA_SST(KSX) ) ) THEN
   ZSST(:) = DTS%XDATA_SST(:,KSX)     
ELSE IF ( .NOT. TEMPORAL_LTS ( TPTIME, DTS%TDATA_SST(DTS%NTIME) ) ) THEN
  ZSST(:) = DTS%XDATA_SST(:,DTS%NTIME)
ELSE

   CALL TEMPORAL_DISTS ( DTS%TDATA_SST(KSX+1)%TDATE%YEAR,DTS%TDATA_SST(KSX+1)%TDATE%MONTH,   &
                         DTS%TDATA_SST(KSX+1)%TDATE%DAY ,DTS%TDATA_SST(KSX+1)%TIME,          &
                         DTS%TDATA_SST(KSX)%TDATE%YEAR,DTS%TDATA_SST(KSX)%TDATE%MONTH,       &
                         DTS%TDATA_SST(KSX)%TDATE%DAY ,DTS%TDATA_SST(KSX)%TIME,              &
                         ZSDTJX                                                      )  

   CALL TEMPORAL_DISTS ( TPTIME%TDATE%YEAR   ,TPTIME%TDATE%MONTH,                      &
                         TPTIME%TDATE%DAY    ,TPTIME%TIME,                             &
                         DTS%TDATA_SST(KSX)%TDATE%YEAR,DTS%TDATA_SST(KSX)%TDATE%MONTH,       &
                         DTS%TDATA_SST(KSX)%TDATE%DAY ,DTS%TDATA_SST(KSX)%TIME,              &
                         ZDT                                                         )  
!
    ZALPHA = ZDT / ZSDTJX
!
    ZSST(:)= DTS%XDATA_SST(:,KSX)+(DTS%XDATA_SST(:,KSX+1)-DTS%XDATA_SST(:,KSX))*ZALPHA
                       
END IF

PSST(:) = ZSST(:)
IF (LHOOK) CALL DR_HOOK('PREP_SST_INIT',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE PREP_SST_INIT
