!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_ISBA_UPDATE_SNOW (IO, NP, NPE, HPROGRAM, KI, PSWE, PSWE_ORIG, OINITSNOW, OINC, HTEST )

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Routine to update snow field for ISBA
!  Trygve Aspelien, Separating IO  06/2013
!
!
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NPE_t, ISBA_NP_t, ISBA_PE_t, ISBA_P_t
!
USE MODD_CSTS,        ONLY : XTT
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_SNOW_PAR,    ONLY : XANSMIN, XANSMAX, XRHOSMIN, XRHOSMAX
!
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK,          ONLY : LHOOK,DR_HOOK
USE PARKIND1,         ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN)    :: KI
REAL, DIMENSION(KI), INTENT(IN)    :: PSWE
REAL, DIMENSION(KI), INTENT(INOUT) :: PSWE_ORIG
LOGICAL,             INTENT(IN)    :: OINITSNOW
LOGICAL,             INTENT(IN)    :: OINC
CHARACTER(LEN=2),    INTENT(IN)    :: HTEST     ! must be equal to 'OK'
!
!    Declarations of local variables
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
REAL, DIMENSION(KI) :: ZSWE     ! Snow before update
REAL, DIMENSION(KI) :: ZSWEINC
REAL, DIMENSION(KI) :: ZTS
!    Addtional snow fields with D95 snow scheme 
REAL, DIMENSION(KI) :: ZSNR     ! Snow density 
REAL, DIMENSION(KI) :: ZSNA     ! Snow albedo 
INTEGER  :: JL,JP,JI,IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ----------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_ISBA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF ( NPE%AL(1)%TSNOW%SCHEME=='D95' ) THEN
  JL = 1
  JP = 1
  PK => NP%AL(1)
  PEK => NPE%AL(1)
  IF ( IO%NPATCH > 1 ) CALL ABOR1_SFX("Update of snow is only implemented for D95 and one patch")
ELSE
  CALL ABOR1_SFX("Update of snow is only implemented for D95")
ENDIF
!
ZTS(:) = XUNDEF
ZSWE(:) = XUNDEF
!
IF ( OINITSNOW ) THEN
  !
  DO JI = 1,PK%NSIZE_P
   !
   IMASK = PK%NR_P(JI)
   !
   PSWE_ORIG(IMASK) = PEK%TSNOW%WSNOW(JI,JL)
   !
   ZTS(IMASK) = PEK%XTG(JI,1)
   !
   ZSWE(IMASK) = PSWE(IMASK)
   ! Set snow=0 where 1. guess = 0 and Ts>0, to avoid that the snow analysis introduce snow where it is no snow.
   IF ( PSWE(IMASK)/=XUNDEF .AND. PSWE(IMASK)<1.0E-10 .AND. ZTS(IMASK)>XTT ) THEN
     ZSWE(IMASK)   = 0.0
   ENDIF
   !
   PEK%TSNOW%WSNOW(JI,JL) = ZSWE(IMASK)
   !
  ENDDO
  !
ENDIF


! Update snow
IF ( OINC ) THEN

 DO JI = 1,PK%NSIZE_P
  !
  IMASK = PK%NR_P(JI)

  ZSWE(IMASK) = PEK%TSNOW%WSNOW(JI,JL)  
  ZSNA(IMASK) = PEK%TSNOW%ALB  (JI)
  ZSNR(IMASK) = PEK%TSNOW%RHO  (JI,JL)
  !
 ENDDO

  ! If we only do second step, we must set working SWE as input SWE
  IF ( .NOT. OINITSNOW ) ZSWE(:) = PSWE(:)
 
  ! Calculate increments
  ZSWEINC(:) = ZSWE(:) - PSWE_ORIG(:)
  WRITE(*,'("  SURFRESERV.NEIGE - min, mean, max: ",3E13.4)') MINVAL(ZSWE),MAXVAL(ZSWE),SUM(ZSWE)/KI
  WRITE(*,*) 'Mean SN increments over NATURE ',SUM(ZSWEINC)/KI

  ! Snow albedo and density are given initial values in points  
  ! which get initial snow in the snow analysis 
  WHERE ( PSWE_ORIG(:) < 1.0E-10 .AND. ZSWE(:)>= 1.0E-10 ) 
    ZSNA(:)    = 0.5 * ( XANSMIN + XANSMAX ) 
    ZSNR(:)    = 0.5 * ( XRHOSMIN + XRHOSMAX )
  END WHERE 
  !
 DO JI = 1,PK%NSIZE_P
  !
  IMASK = PK%NR_P(JI)
  !
  PEK%TSNOW%WSNOW(JI,JL) = ZSWE(IMASK)
  PEK%TSNOW%ALB  (JI)    = ZSNA(IMASK)
  PEK%TSNOW%RHO  (JI,JL) = ZSNR(IMASK)
  !
 ENDDO
 !
ENDIF
!
! -------------------------------------------------------------------------------------
 IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',1,ZHOOK_HANDLE)
 END SUBROUTINE ASSIM_ISBA_UPDATE_SNOW

