!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE VERTICAL_GRID_NAT(PDG1,PT1,PDG2,PT2)
!##########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PDG1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1  ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN)  :: PDG2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
REAL,    DIMENSION(SIZE(PDG1,1),SIZE(PDG1,2)) :: ZDZG1
REAL,    DIMENSION(SIZE(PDG2,1),SIZE(PDG2,2)) :: ZSUM
!
REAL,    DIMENSION(SIZE(PDG1,1),SIZE(PDG1,2),SIZE(PDG2,2)) :: ZWGHT
REAL,    DIMENSION(SIZE(PDG1,1),SIZE(PDG1,2)) :: ZSUM_WGHT
!
REAL :: ZWORK
!
INTEGER :: INI, INL1, INL2
INTEGER :: JL1, JL2, JI ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('VERTICAL_GRID_NAT',0,ZHOOK_HANDLE)
!
INI =SIZE(PT1,1)
INL1=SIZE(PT1,2)
INL2=SIZE(PT2,2)
!
PT2      (:,:)   = 0.0
ZSUM     (:,:)   = 0.0
ZSUM_WGHT(:,:)   = 0.0
ZWGHT    (:,:,:) = 0.0
!
ZDZG1(:,1)=PDG1(:,1)
DO JL1=2,INL1
   ZDZG1(:,JL1)=PDG1(:,JL1)-PDG1(:,JL1-1)
END DO
!
DO JL2=1,INL2
   DO JL1=1,INL1
      DO JI=1,INI
!
         IF(PT1(JI,JL1)/=XUNDEF)THEN
!
           ZWGHT(JI,JL1,JL2)=MIN(ZDZG1(JI,JL1),MAX(0.0,PDG2(JI,JL2)-PDG1(JI,JL1)+ZDZG1(JI,JL1)))
           ZWGHT(JI,JL1,JL2)=MAX(0.0,ZWGHT(JI,JL1,JL2)-ZSUM_WGHT(JI,JL1))
!
           PT2 (JI,JL2)=PT2 (JI,JL2)+ZWGHT(JI,JL1,JL2)*PT1(JI,JL1)
           ZSUM(JI,JL2)=ZSUM(JI,JL2)+ZWGHT(JI,JL1,JL2)
!
           ZSUM_WGHT(JI,JL1)=ZSUM_WGHT(JI,JL1)+ZWGHT(JI,JL1,JL2)
!
         ENDIF
!
      END DO
   END DO
END DO
!
WHERE(ZSUM(:,:)>0.0)
      PT2(:,:)=PT2(:,:)/ZSUM(:,:)
ELSEWHERE
      PT2(:,:)=XUNDEF
ENDWHERE
!
!SIMPLE EXTRAPOLATION
!
DO JL2=2,INL2
   DO JI=1,INI
      IF(PT2(JI,1)/=XUNDEF.AND.PT2(JI,JL2)==XUNDEF)THEN
         PT2(JI,JL2)=PT2(JI,JL2-1)
      ENDIF
   END DO
END DO
!
IF (LHOOK) CALL DR_HOOK('VERTICAL_GRID_NAT',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------
END SUBROUTINE VERTICAL_GRID_NAT
