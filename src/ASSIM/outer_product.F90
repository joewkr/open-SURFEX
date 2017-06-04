!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OUTER_PRODUCT(KENS,KX,KY,PX,PY,PA,PC,OPB_CORRELATIONS,HVAR,HOBS)
!---------------------------------------------------------
!
! Computes the outer product of two vectors X and Y
! to produce a matrix A = XY**T
!
!
!                          Jean-Francois MAHFOUF (11/06)
!--------------------------------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KENS, KX, KY
LOGICAL, INTENT(IN) :: OPB_CORRELATIONS
CHARACTER(LEN=3), DIMENSION(KX), INTENT(IN) :: HVAR
CHARACTER(LEN=3), DIMENSION(KY), INTENT(IN) :: HOBS
REAL, DIMENSION(KX,KENS), INTENT(IN) :: PX
REAL, DIMENSION(KY,KENS), INTENT(IN) :: PY
REAL, DIMENSION(KX,KY),  INTENT(OUT) :: PA
REAL, DIMENSION(KY,KY),  INTENT(OUT) :: PC
!
REAL, DIMENSION(KX,KENS) :: ZXPERT
REAL, DIMENSION(KY,KENS) :: ZYPERT
REAL, DIMENSION(KENS,KY) :: ZYT
REAL, DIMENSION(KX) :: ZXM
REAL, DIMENSION(KY) :: ZYM
!
INTEGER :: I, K, L
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('OUTER_PRODUCT',0,ZHOOK_HANDLE)
!
DO L = 1,KX
  !
  ZXM(L) = SUM(PX(L,:))/REAL(KENS)!MEAN
  ZXPERT(L,:) = PX(L,:)-ZXM(L)     !ANOMALY
  !
ENDDO
!
DO K = 1,KY
  ZYM(K) = SUM(PY(K,:))/REAL(KENS)
  ZYPERT(K,:) = PY(K,:)-ZYM(K) 
ENDDO
!
IF (OPB_CORRELATIONS) THEN
  !For 2D EnKF
  PA(:,:)=MATMUL(ZXPERT(:,:),TRANSPOSE(ZYPERT(:,:)))
  PC(:,:)=MATMUL(ZYPERT(:,:),TRANSPOSE(ZYPERT(:,:)))
  !
ELSE
  !For 1D EnKF
  DO L = 1,KX
    DO K = 1,KY
      IF (HVAR(L).EQ.HOBS(K)) THEN
        PA(L,K) = DOT_PRODUCT(ZXPERT(L,:),ZYPERT(K,:))
      ELSE
        PA(L,K) = 0.0        
      ENDIF
    ENDDO
  ENDDO
ENDIF
!
PA = PA / REAL(KENS - 1)
PC = PC / REAL(KENS - 1)
!
IF (LHOOK) CALL DR_HOOK('OUTER_PRODUCT',1,ZHOOK_HANDLE)
!
END SUBROUTINE OUTER_PRODUCT
