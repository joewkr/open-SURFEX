!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE CLS_TQ( PTA, PQA, PPA, PPS, PHT, PCD, PCH, PRI, &
                          PTS, PHU, PZ0H, PH, PTNM, PQNM, PHUNM  )  
!     #####################################################################
!
!!****  *PARAMCLS*  
!!
!!    PURPOSE
!!    -------
!
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    USE MODD_CST
!!    USE MODD_GROUND_PAR
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    26/10/98
!!      S. Riette   06/2009 CLS_2M becomes CLS_TQ, height now is an argument
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XG, XCPD, XKARMAN
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODE_THERMOS
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
REAL, DIMENSION(:), INTENT(IN)       :: PTA    ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA    ! atmospheric humidity (kg/kg)
REAL, DIMENSION(:), INTENT(IN)       :: PPA    ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height (temp)
REAL, DIMENSION(:), INTENT(IN)       :: PCD    ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PCH    ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN)       :: PRI    ! Richardson number
REAL, DIMENSION(:), INTENT(IN)       :: PTS    ! surface temperature
REAL, DIMENSION(:), INTENT(IN)       :: PHU    ! near-surface humidity (%)
REAL, DIMENSION(:), INTENT(IN)       :: PZ0H   ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN)       :: PH     ! height of diagnostic
!
REAL, DIMENSION(:), INTENT(OUT)      :: PTNM   ! temperature at n meters
REAL, DIMENSION(:), INTENT(OUT)      :: PQNM   ! specific humidity at n meters
REAL, DIMENSION(:), INTENT(OUT)      :: PHUNM  ! relative humidity at n meters
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZBNH,ZBH,ZRS
REAL, DIMENSION(SIZE(PTA)) :: ZLOGS,ZCORS,ZIV
REAL, DIMENSION(SIZE(PTA)) :: ZQSATA, ZHUA
REAL, DIMENSION(SIZE(PTA)) :: ZQSATNM, ZPNM, ZQS, ZQSATS
 CHARACTER(LEN=2)           :: YHUMIDITY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLS_TQ',0,ZHOOK_HANDLE)
PTNM (:) = XUNDEF
PQNM(:)  = XUNDEF
PHUNM(:) = XUNDEF
!
ZBNH   (:) = 0.
ZBH    (:) = 0.
ZRS    (:) = 0.
ZLOGS  (:) = 0.
ZCORS  (:) = 0.
ZIV    (:) = 0.
ZQSATA (:) = 0.
ZHUA   (:) = 0.
ZQSATS (:) = 0.
ZPNM   (:) = 0.
ZQSATNM(:) = 0.
ZQS    (:) = 0.
!
!*      1.     preparatory calculations
!              ------------------------
!
ZBNH(:)=LOG( PHT(:)/PZ0H(:))
!
ZBH(:)=XKARMAN*SQRT( PCD(:) )/PCH(:) 
!
ZRS(:)=MIN(PH/PHT(:),1.)
!
ZLOGS(:)=LOG(1.+ZRS(:)*(EXP(ZBNH(:)) -1.))
!
!*      2.     Stability effects
!              -----------------
!
WHERE (PRI(:)>=0.)
  ZCORS(:)=ZRS(:)*(ZBNH(:)-ZBH(:))
END WHERE
!
WHERE (PRI(:)< 0.)
  ZCORS(:)=LOG(1.+ZRS(:)*(EXP(MAX(0.,ZBNH(:)-ZBH(:)))-1.))
END WHERE
!
!*      3.     Interpolation of thermodynamical variables
!              ------------------------------------------
!
ZIV=MAX(0.,MIN(1.,(ZLOGS(:)-ZCORS(:))/ZBH(:)))
PTNM(:)=PTS(:)+ZIV(:)*(PTA(:)-PTS(:))
!
!*      4.     Interpolation of relative humidity
!              ----------------------------------
!
!* choice of interpolated variable
!
YHUMIDITY='Q '
!
ZPNM(:) = PPS(:) + PH/PHT(:) * (PPA(:)-PPS(:))
ZQSATNM(:) = QSAT(PTNM(:),ZPNM(:))
!
IF (YHUMIDITY=='Q ') THEN
!        
  ZQSATS(:) = QSAT(PTS(:),PPS(:))
  ZQS(:)    = PHU(:)*ZQSATS(:)
  PQNM(:)   = ZQS(:)+ZIV(:)*(PQA(:)-ZQS(:))
  PQNM(:)   = MIN (ZQSATNM(:),PQNM(:)) !must be below saturation
  PHUNM(:)  = PQNM(:) / ZQSATNM(:)
!
ELSE IF (YHUMIDITY=='HU') THEN
!        
  ZQSATA(:) = QSAT(PTA(:),PPA(:))
  ZHUA(:)   = PQA(:) / ZQSATA(:)
  PHUNM(:)  = PHU(:)+ZIV(:)*(ZHUA(:)-PHU(:))
  PQNM(:)   = PHUNM(:) * ZQSATNM(:)
!
END IF
IF (LHOOK) CALL DR_HOOK('CLS_TQ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLS_TQ
