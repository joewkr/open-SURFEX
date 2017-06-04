!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_ECO2_IRRIG (&
                                   DTCO, &
                                  HPROGRAM)
!     #######################
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODI_READ_SURF
!
USE MODD_DATA_COVER,     ONLY : TDATA_SEED, TDATA_REAP, XDATA_WATSUP, XDATA_IRRIG,&
                                  LDATA_IRRIG 
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVT_IRR
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
!
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YRECFM     ! Name of the article to be read
INTEGER           :: IRESP      ! reading return code
!
INTEGER           :: IVERSION   ! surface version
INTEGER           :: IBUGFIX    ! surface bugfix
!
INTEGER           :: JCOVER     ! loop counter
!
REAL, DIMENSION(6) :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_ECO2_IRRIG',0,ZHOOK_HANDLE)
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<4 .OR. IVERSION==4 .AND. IBUGFIX<2) THEN
  LDATA_IRRIG  = .FALSE.
  IF (LHOOK) CALL DR_HOOK('READ_ECO2_IRRIG',1,ZHOOK_HANDLE)
  RETURN
END IF
!
YRECFM='DATA_IRRIG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,LDATA_IRRIG,IRESP)
!
IF (.NOT. LDATA_IRRIG .AND. LHOOK) CALL DR_HOOK('READ_ECO2_IRRIG',1,ZHOOK_HANDLE)
IF (.NOT. LDATA_IRRIG) RETURN
!
DO JCOVER=1,JPCOVER
  IF (DTCO%XDATA_VEGTYPE(JCOVER,NVT_IRR)==0.) CYCLE
  WRITE(YRECFM,FMT='(A6,I3.3)') 'IRRIG_',JCOVER
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK,IRESP,HDIR='-')
  TDATA_SEED  (JCOVER,NVT_IRR)%TDATE%MONTH = NINT(ZWORK(1))
  TDATA_SEED  (JCOVER,NVT_IRR)%TDATE%DAY   = NINT(ZWORK(2))
  TDATA_REAP  (JCOVER,NVT_IRR)%TDATE%MONTH = NINT(ZWORK(3))
  TDATA_REAP  (JCOVER,NVT_IRR)%TDATE%DAY   = NINT(ZWORK(4))
  XDATA_WATSUP(JCOVER,NVT_IRR)             = ZWORK(5)
  XDATA_IRRIG (JCOVER,NVT_IRR)             = ZWORK(6)
  TDATA_SEED  (JCOVER,NVT_IRR)%TDATE%YEAR  = 9999
  TDATA_SEED  (JCOVER,NVT_IRR)%TIME        = 0.
  TDATA_REAP  (JCOVER,NVT_IRR)%TDATE%YEAR  = 9999
  TDATA_REAP  (JCOVER,NVT_IRR)%TIME        = 0.
END DO
IF (LHOOK) CALL DR_HOOK('READ_ECO2_IRRIG',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_ECO2_IRRIG
