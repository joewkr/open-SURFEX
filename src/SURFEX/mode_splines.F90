!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
MODULE MODE_SPLINES
!-------------------------------------------------------------------------------
!!    MODIFICATIONS
!!    -------------
!!
!!      J.Escobar     06/2013  for REAL4/8 add EPSILON management
!!
!-------------------------------------------------------------------------------
USE MODI_ABOR1_SFX
USE MODD_SPLINES
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODD_SURF_PAR , ONLY : XSURF_HUGE , XSURF_TINY
!
INTERFACE SSCOPY
        MODULE PROCEDURE SSCOPY_1
        MODULE PROCEDURE SSCOPY_2
END INTERFACE
INTERFACE MXIDML
        MODULE PROCEDURE MXIDML
END INTERFACE
INTERFACE MTXAXM
        MODULE PROCEDURE MTXAXM
END INTERFACE
INTERFACE MXMSPL
        MODULE PROCEDURE MXMSPL_1
        MODULE PROCEDURE MXMSPL_2
        MODULE PROCEDURE MXMSPL_3
END INTERFACE
INTERFACE SMXINV
        MODULE PROCEDURE SMXINV
END INTERFACE
!génération de matrices
INTERFACE SPLIE
        MODULE PROCEDURE SPLIE
END INTERFACE
INTERFACE SPLK
        MODULE PROCEDURE SPLK_1
        MODULE PROCEDURE SPLK_2
END INTERFACE
INTERFACE SPLBFIN
        MODULE PROCEDURE SPLBFIN
END INTERFACE
INTERFACE SPLT
        MODULE PROCEDURE SPLT_1
        MODULE PROCEDURE SPLT_2
END INTERFACE
INTERFACE SPLE
        MODULE PROCEDURE SPLE
END INTERFACE
INTERFACE SPLB2E1
        MODULE PROCEDURE SPLB2E1
END INTERFACE
INTERFACE SPLB2E
        MODULE PROCEDURE SPLB2E
END INTERFACE
INTERFACE TRED2
        MODULE PROCEDURE TRED2
END INTERFACE
INTERFACE TQL2_2
        MODULE PROCEDURE TQL2_2
END INTERFACE
INTERFACE EISRS1
        MODULE PROCEDURE EISRS1
END INTERFACE
INTERFACE SPLV
        MODULE PROCEDURE SPLV
END INTERFACE 
INTERFACE SPLS2VI
        MODULE PROCEDURE SPLS2VI
END INTERFACE
INTERFACE SPLBVM
        MODULE PROCEDURE SPLBVM
END INTERFACE
INTERFACE SPLS2V
        MODULE PROCEDURE SPLS2V
END INTERFACE
INTERFACE SPLDS2V
        MODULE PROCEDURE SPLDS2V
END INTERFACE
INTERFACE SPLPS2V
        MODULE PROCEDURE SPLPS2V
END INTERFACE
INTERFACE SPLRI
        MODULE PROCEDURE SPLRI
END INTERFACE
INTERFACE SPLRS
        MODULE PROCEDURE SPLRS
END INTERFACE
INTERFACE SPLDRS
        MODULE PROCEDURE SPLDRS
END INTERFACE
INTERFACE SPLPR
        MODULE PROCEDURE SPLPR
END INTERFACE
INTERFACE SPLDV
        MODULE PROCEDURE SPLDV
END INTERFACE
INTERFACE SPLD2V
        MODULE PROCEDURE SPLD2V
END INTERFACE
INTERFACE SPLPV
        MODULE PROCEDURE SPLPV
END INTERFACE
INTERFACE SPLP
        MODULE PROCEDURE SPLP
END INTERFACE
INTERFACE SPLTT
        MODULE PROCEDURE SPLTT
END INTERFACE
INTERFACE SPLR
        MODULE PROCEDURE SPLR_1
        MODULE PROCEDURE SPLR_2
END INTERFACE
INTERFACE SPLU
        MODULE PROCEDURE SPLU
END INTERFACE
INTERFACE SPLW
        MODULE PROCEDURE SPLW
END INTERFACE
INTERFACE SPLVPQ
        MODULE PROCEDURE SPLVPQ
END INTERFACE
INTERFACE SPLC
        MODULE PROCEDURE SPLC 
END INTERFACE
INTERFACE SPLM
        MODULE PROCEDURE SPLM
END INTERFACE
INTERFACE SP0NOP
        MODULE PROCEDURE SP0NOP
END INTERFACE
INTERFACE SP0CVQ
        MODULE PROCEDURE SP0CVQ
END INTERFACE
INTERFACE SPLBSD
        MODULE PROCEDURE SPLBSD
END INTERFACE
INTERFACE SPLBSEL
        MODULE PROCEDURE SPLBSEL
END INTERFACE
INTERFACE SPLB2C
        MODULE PROCEDURE SPLB2C
END INTERFACE

CONTAINS

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!COPIE D'UNE MATRICE DANS UNE AUTRE
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!1. copie de matrices potentiellement de tailles différentes
!SPLR
SUBROUTINE SSCOPY_1(A,B,IA,IB)
!
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: A
INTEGER, INTENT(IN) :: IA
INTEGER, INTENT(IN) :: IB
REAL, DIMENSION(:), INTENT(OUT) :: B

INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SSCOPY_1',0,ZHOOK_HANDLE)
J = IB
K = IA
DO I=1,SIZE(A)
  B(J) = A(K)
  J = J + IB
  K = K + IA
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SSCOPY_1',1,ZHOOK_HANDLE)

END SUBROUTINE SSCOPY_1
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!1. copie de matrices potentiellement de tailles différentes
SUBROUTINE SSCOPY_2(A,B,IA,IB)
!
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: A
INTEGER, INTENT(IN) :: IA
INTEGER, INTENT(IN) :: IB
REAL, DIMENSION(:,:), INTENT(OUT) :: B

REAL,DIMENSION(SIZE(A,1)*SIZE(A,2)) :: A1
REAL,DIMENSION(SIZE(B,1)*SIZE(B,2)) :: B1

INTEGER :: N
INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SSCOPY_2',0,ZHOOK_HANDLE)
N=SIZE(A,1)*SIZE(A,2)

DO J=1,SIZE(A,2)
  A1((J-1)*SIZE(A,1)+1:J*SIZE(A,1))=A(:,J)
ENDDO

J = IB
K = IA
DO I=1,N-1
  B1(J) = A1(K)
  J = J + IB
  K = K + IA
ENDDO

DO J=1,SIZE(B,2)
  B(:,J)=B1((J-1)*SIZE(B,1)+1:J*SIZE(B,1))
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SSCOPY_2',1,ZHOOK_HANDLE)

END SUBROUTINE SSCOPY_2
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!OPERATIONS SUR LES MATRICES
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!2. A(I) B(I*J) R(I*J) R(:,K)=B(:,K)/A(:)
SUBROUTINE MXIDML(A,B,R)
!SPLTT, SPLR
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: A
REAL, DIMENSION(:,:), INTENT(IN) :: B
REAL, DIMENSION(:,:), INTENT(OUT):: R

INTEGER :: K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXIDML',0,ZHOOK_HANDLE)
IF (SIZE(B,1).EQ.0 .OR. SIZE(B,2).EQ.0 .AND. LHOOK) &
        CALL DR_HOOK('MODE_SPLINES:MXIDML',1,ZHOOK_HANDLE)
IF (SIZE(B,1).EQ.0 .OR. SIZE(B,2).EQ.0) RETURN

DO K=1,SIZE(B,2)
  R(:,K)=B(:,K)/A(:)
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXIDML',1,ZHOOK_HANDLE)

END SUBROUTINE MXIDML
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!3. produit bizarre de matrices
SUBROUTINE MTXAXM(A,B,R)
!SPLR,SPLU,SPLVPQ
IMPLICIT NONE

REAL, DIMENSION(:,:),INTENT(IN) :: A
REAL, DIMENSION(:,:),INTENT(IN) :: B
REAL, DIMENSION(:,:),INTENT(OUT)::R

INTEGER :: I,J,K, L
REAL :: RIJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MTXAXM',0,ZHOOK_HANDLE)
R(:,:)=0.
DO I=1,SIZE(A,2)
  DO J=1,SIZE(A,1)
    RIJ=0.
    DO K=1,SIZE(A,1)
        RIJ=RIJ+B(J,K)*A(K,I)
    ENDDO
    DO L=1,I
        R(L,I)=R(L,I)+RIJ*A(J,L)
        R(I,L)=R(L,I)
    ENDDO
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MTXAXM',1,ZHOOK_HANDLE)

END SUBROUTINE MTXAXM
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!4. produit simple de matrices (mettre les deux dans la même interface)
SUBROUTINE MXMSPL_1(A,B,R)
!SPLE, SPLC, SPLTT, SPLR, SPLW, SPLVPQ
IMPLICIT NONE

REAL, DIMENSION(:,:),INTENT(IN) :: A
REAL, DIMENSION(:,:),INTENT(IN) :: B
REAL, DIMENSION(:,:),INTENT(OUT)::R

INTEGER :: I,J,K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_1',0,ZHOOK_HANDLE)
R(:,:)=0.

DO I=1,SIZE(A,1)
  DO J=1,SIZE(B,2)
    DO K=1,SIZE(A,2)
      R(I,J)=R(I,J)+A(I,K)*B(K,J)
    ENDDO
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_1',1,ZHOOK_HANDLE)

END SUBROUTINE MXMSPL_1
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!4. produit simple de matrices 2
SUBROUTINE MXMSPL_2(A,B,R)
!
IMPLICIT NONE

REAL, DIMENSION(:,:),INTENT(IN) :: A
REAL, DIMENSION(:),INTENT(IN) :: B
REAL, DIMENSION(:),INTENT(OUT)::R

INTEGER :: I,J
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_2',0,ZHOOK_HANDLE)
R(:)=0.

DO I=1,SIZE(A,1)
  DO J=1,SIZE(A,2)
    R(I)=R(I)+A(I,J)*B(J)
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_2',1,ZHOOK_HANDLE)

END SUBROUTINE MXMSPL_2    
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!4. produit simple de matrices 3
SUBROUTINE MXMSPL_3(A,B,RES)
!
IMPLICIT NONE

REAL, DIMENSION(:),INTENT(IN) :: A
REAL, DIMENSION(:),INTENT(IN) :: B
REAL,INTENT(OUT)::RES

INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_3',0,ZHOOK_HANDLE)
RES=0.

DO I=1,SIZE(A)
  RES=RES+A(I)*B(I)
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:MXMSPL_3',1,ZHOOK_HANDLE)

END SUBROUTINE MXMSPL_3
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!5. inversion de la matrice A.
SUBROUTINE SMXINV(A,IREP)
!SPLTT
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(INOUT) :: A
INTEGER, INTENT(OUT) :: IREP

REAL, DIMENSION(SIZE(A,1)*SIZE(A,2)) :: A2
INTEGER, DIMENSION(SIZE(A,1)) :: INDEX
REAL, DIMENSION(SIZE(A,1)) :: RI
INTEGER :: N, NP, NP1, I, J, JJ, K, L, KK, KJ, JL
INTEGER :: IJ0, JI0, IJ, JI
REAL :: PIVOT, ELM
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SMXINV',0,ZHOOK_HANDLE)
IREP=0
N=SIZE(A,1)
NP1=N+1

INDEX(:)=1

DO J=1,N
 !les colonnes sont à la suite
  A2((J-1)*N+1:J*N)=A(:,J)
ENDDO

DO I=1,N
  !find pivot
  PIVOT=0.
  JJ=1
  !dans la diagonale, on retient le plus grand élément
  DO J=1,N
    IF(INDEX(J).NE.0) THEN   
      ELM=ABS(A2(JJ))
      IF (ELM.GT.PIVOT) THEN
        PIVOT=ELM
        K=J !numéro de la ligne / colonne
        !numéro de l'élément dans A2
        KK=JJ
      ENDIF
    ENDIF
    JJ=JJ+NP1
  ENDDO
  INDEX(K)=0
  IF (PIVOT.EQ.0.) THEN 
    IREP=-1
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SMXINV',1,ZHOOK_HANDLE)
    RETURN 
  ENDIF
  PIVOT=-A2(KK)
  !elimination
  KJ=K
  NP=N
  DO J=1,N
    !KJ=K, KJ=K+N, KJ=K+2N ... : avant l'élément pivot, 
    !on se déplace sur les colonnes
    !J=K=3: KJ=K+2N+1, KJ=K+2N+2....!une fois qu'on a passé
    !le pivot, on se déplace sur la ligne, à droite de l'élément pivot
    !si on est sur le premier élément de la colonne pivot
    IF (J==K) THEN
      !KJ réfère à l'élément diagonal du pivot
      A2(KJ)=1./PIVOT
      RI(J)=0.
      NP=1
    ELSE
      !si on est sur un autre élément de du pivot
      ELM=-A2(KJ)
      RI(J)=ELM/PIVOT
      IF (ELM.NE.0.) THEN
        JL=J
        !JL prend pour valeur les élements de la colonne 
        !du premier en haut au diagonal -1
        DO L=1,J
          !à chaque fois A2 de cet élément est incrémenté
          A2(JL)=A2(JL)+ELM*RI(L)
          JL=JL+N
        ENDDO
      ENDIF
      !élement véritablement en cours
      A2(KJ)=RI(J)
    ENDIF
    !KJ=K+JN si on n'a pas encore passé l'élément de la colonne pivot
    !KJ=K+J0N+J si on l'a passé
    KJ=KJ+NP
  ENDDO
ENDDO

!change the sign and provisional fill-up
DO J=1,N
  A(:,J)= A2((J-1)*N+1:J*N)
ENDDO

DO I=1,N
  DO J=1,I
    A(I,J)=-A(I,J)
    A(J,I)=A(I,J)
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SMXINV',1,ZHOOK_HANDLE)

END SUBROUTINE SMXINV

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! CALCULS PLUS SPECIFIQUES
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!6. IE(ND,M): calcul de la matrice IE en fonction de NDEG.
SUBROUTINE SPLIE(NDEG,IE)
!SPLE, SPLR
IMPLICIT NONE

INTEGER, INTENT(IN) :: NDEG
INTEGER, DIMENSION(:,:), INTENT(OUT) :: IE

INTEGER, DIMENSION(SIZE(IE,1)) :: NV
INTEGER :: I, J, K, T, N, N0, NC
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLIE',0,ZHOOK_HANDLE)
NV(:)=1
IE(:,1)=0
N=1
N0=1

!boucle d'itération
DO T=1,NDEG
!boucle sur les indices
  DO I=1,SIZE(IE,1)
    NC=0
    !NV(I) varie d'itération T en itération
    DO K=NV(I),N0
      N=N+1
      NC=NC+1
      !boucle sur les indices
      DO J=1,SIZE(IE,1)
        IE(J,N)=IE(J,K)
        IF (J.EQ.I) IE(J,N)=IE(J,N)+1
      ENDDO
    ENDDO
    NV(I)=N-NC+1
  ENDDO
  N0=N
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLIE',1,ZHOOK_HANDLE)

END SUBROUTINE SPLIE
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!7. calcul de RK en fonction des autres RK(N1,N2)
SUBROUTINE SPLK_1(NORD,X1,X2,G,RK)
!SPLE,SPLU,
IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
REAL, DIMENSION(:), INTENT(IN) :: X1
REAL, DIMENSION(:,:), INTENT(IN) :: X2
REAL, DIMENSION(:,:), INTENT(IN) :: G
REAL, DIMENSION(:), INTENT(OUT):: RK

INTEGER :: I, ID, JD, EXP, ISIGNE
INTEGER :: ND, N, DI, DJ
REAL :: D2
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLK_1',0,ZHOOK_HANDLE)
ND=SIZE(X1)
N=SIZE(X2,2)

!Exposant du noyau
EXP=(2*NORD-ND)/2.

IF (MOD(ND,2).EQ.0) THEN
  !Calcul de K dans le cas d'un espace de dimension paire
  ISIGNE=(-1)**(1+NORD+ND/2)
ELSE
  !Calcul de K dans le cas d'un espace de dimension impair 
  ISIGNE=(-1)**(NORD+ND/2)
ENDIF

RK(:)=0.

DO I=1,N
  D2=0.
  DO ID=1,ND
    DO JD=1,ND
      DI=X1(ID)-X2(ID,I)
      DJ=X1(JD)-X2(JD,I)
      D2=D2+G(ID,JD)*DI*DJ
    ENDDO
  ENDDO
  IF (D2.NE.0.) THEN
    RK(I) = ISIGNE*D2**EXP
    IF (MOD(ND,2).EQ.0) RK(I)=RK(I)*0.5*LOG(D2)
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLK_1',1,ZHOOK_HANDLE)

END SUBROUTINE SPLK_1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!7. calcul de RK en fonction des autres RK(N1,N2)
SUBROUTINE SPLK_2(NORD,X1,X2,G,RK)

IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
REAL, DIMENSION(:,:), INTENT(IN) :: X1
REAL, DIMENSION(:,:), INTENT(IN) :: X2
REAL, DIMENSION(:,:), INTENT(IN) :: G
REAL, DIMENSION(:,:), INTENT(OUT):: RK

INTEGER :: I1, I2, ID, JD, ISIGNE
INTEGER :: ND, N1, N2
REAL :: EXP, D2, DI, DJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLK_2',0,ZHOOK_HANDLE)
ND=SIZE(X1,1)
N1=SIZE(X1,2)
N2=SIZE(X2,2)

!Exposant du noyau
EXP=(2*NORD-ND)/2.

IF (MOD(ND,2).EQ.0) THEN
  !Calcul de K dans le cas d'un espace de dimension paire
  ISIGNE=(-1)**(1+NORD+ND/2)
ELSE
  !Calcul de K dans le cas d'un espace de dimension impair 
  ISIGNE=(-1)**(NORD+ND/2)
ENDIF

RK(:,:)=0.

DO I1=1,N1
  DO I2=1,N2
    D2=0.
    DO ID=1,ND
      DO JD=1,ND
        DI=X1(ID,I1)-X2(ID,I2)
        DJ=X1(JD,I1)-X2(JD,I2)
        D2=D2+G(ID,JD)*DI*DJ
      ENDDO
    ENDDO
    IF (D2.NE.0.) THEN
      RK(I1,I2) = ISIGNE*D2**EXP
      IF (MOD(ND,2).EQ.0) RK(I1,I2)=RK(I1,I2)*0.5*LOG(D2)
    ENDIF
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLK_2',1,ZHOOK_HANDLE)

END SUBROUTINE SPLK_2
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!8. on cherche l'indice I de la 3ème dimension de AI tel que XE(ID) soit compris 
!entre AI(ID,1,I) et AI(ID,2,I+1). Il est compris entre 1 et NSD(ID), 
!par défaut il est égal à NDSD(ID)
SUBROUTINE SPLBFIN(ID,NSD,XE,AI,IDX,IDX1)
!SPLB2E
IMPLICIT NONE

INTEGER, INTENT(IN) :: ID
INTEGER, DIMENSION(:), INTENT(IN) :: NSD
REAL, DIMENSION(:), INTENT(IN) :: XE
REAL, DIMENSION(:,:,:), INTENT(IN) :: AI
INTEGER, INTENT(OUT) :: IDX
INTEGER, INTENT(OUT) :: IDX1

!INTEGER, PARAMETER: NSDMAX=20
INTEGER :: I, I1
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBFIN',0,ZHOOK_HANDLE)
IDX1=0
DO I=1,NSD(ID)-1
  IF (XE(ID).LE.AI(ID,2,I)) THEN
    IDX=I
    I1=I+1
    IF (XE(ID).GT.AI(ID,1,I1)) IDX1=I1
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBFIN',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ENDDO
IDX=NSD(ID)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBFIN',1,ZHOOK_HANDLE)
     
END SUBROUTINE SPLBFIN
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!9. X(ND,N) IE(ND,M) : T(N,M) T=produit(X**IE) quand IE est non nul
SUBROUTINE SPLT_1(X,IE,T)
!SPLE, SPLTT
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: X
INTEGER, DIMENSION(:,:), INTENT(IN) :: IE
REAL, DIMENSION(:), INTENT(OUT) :: T
!
INTEGER :: J,K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLT_1',0,ZHOOK_HANDLE)
T(:)=1.

DO J=1,SIZE(IE,2)
    DO K=1,SIZE(X,1)
      IF (IE(K,J).NE.0) T(J)=T(J)*X(K)**IE(K,J)
    ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLT_1',1,ZHOOK_HANDLE)

END SUBROUTINE SPLT_1
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!9. X(ND,N) IE(ND,M) : T(N,M) T=produit(X**IE) quand IE est non nul
SUBROUTINE SPLT_2(X,IE,T)
!
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: X
INTEGER, DIMENSION(:,:), INTENT(IN) :: IE
REAL, DIMENSION(:,:), INTENT(OUT) :: T
!
INTEGER :: I,J,K
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLT_2',0,ZHOOK_HANDLE)
T(:,:)=1.

DO J=1,SIZE(IE,2)
  DO I=1,SIZE(X,2)
    DO K=1,SIZE(X,1)
      IF (IE(K,J).NE.0) T(I,J)=T(I,J)*X(K,I)**IE(K,J)
    ENDDO
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLT_2',1,ZHOOK_HANDLE)

END SUBROUTINE SPLT_2

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! PARTIE "E"
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!10.
SUBROUTINE SPLE(NORD,X,G,C,XE,ZE)
!SPLB2E1
IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
REAL, DIMENSION(:,:),INTENT(IN) :: X !ND,N
REAL, DIMENSION(:,:), INTENT(IN) :: G !ND,ND
REAL, DIMENSION(:),INTENT(IN) :: C ! N+M
REAL, DIMENSION(:),INTENT(IN) :: XE !ND
REAL, INTENT(OUT) :: ZE

INTEGER, DIMENSION(SIZE(X,1),SIZE(C)-SIZE(X,2)) :: IW !ND,M
REAL, DIMENSION(SIZE(X,2)) :: WE1
REAL, DIMENSION(SIZE(IW,2)) :: WE2
REAL :: ZOUT1,ZOUT2
INTEGER :: M, N, NE, M1, M2, M3
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLE',0,ZHOOK_HANDLE)
N=SIZE(X,2)
M=SIZE(C)-N

! Calcul de la matrice Kxe
 CALL SPLK(NORD,XE,X,G,WE1)
! Calcul de Kxe.c
 CALL MXMSPL(WE1,C(1:N),ZOUT1)

IF (M.NE.0) THEN
  ! Generation des exposants des monomes
  CALL SPLIE(NORD-1,IW) !IW(ND,M)
  ! Calcul de la matrice Txe
  CALL SPLT(XE,IW,WE2)
  ! Calcul Txe.d
  CALL MXMSPL(WE2,C(N+1:N+M),ZOUT2)
  ! Calcul de Txe.d + Kxe.c
  ZE = ZOUT1+ZOUT2
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLE',1,ZHOOK_HANDLE)

END SUBROUTINE SPLE
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!11.
SUBROUTINE SPLB2E1(NORD,M,G,C,XE,IDX,IDX1,IDY,IDY1,ZE)
!SPLB2E

IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
INTEGER, INTENT(IN) :: M
REAL, DIMENSION(:,:),INTENT(IN) :: G
REAL, DIMENSION(:,:,:), INTENT(IN) :: C
REAL, DIMENSION(:), INTENT(IN) :: XE
INTEGER, INTENT(IN) :: IDX,IDX1,IDY,IDY1
REAL, INTENT(OUT) :: ZE

INTEGER :: NN,NM
REAL :: RX,RY,ALPHAX,ALPHAY,SUM_AX,SUM_AY,SUM_A
REAL :: ZXY, ZX1Y, ZXY1, ZX1Y1
REAL(KIND=JPRB) :: ZHOOK_HANDLE


IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2E1',0,ZHOOK_HANDLE)

RX=0
RY=0
IF (IDX1.NE.0) RX=(XE(1)-AI(1,1,IDX1))/(AI(1,2,IDX)-AI(1,1,IDX1))
IF (IDY1.NE.0) RY=(XE(2)-AI(2,1,IDY1))/(AI(2,2,IDY)-AI(2,1,IDY1))
ALPHAX=(RX-1.)*(RX-1.)*(2*RX+1.)
ALPHAY=(RY-1.)*(RY-1.)*(2*RY+1.)
SUM_AX=0.
SUM_AY=0.
SUM_A=0.
ZXY=0.
ZX1Y=0.
ZXY1=0.
ZX1Y1=0.

NM=0

IF (NS(IDX,IDY).GE.M) THEN
  NM=1
  SUM_AX=SUM_AX+ALPHAX
  SUM_AY=SUM_AY+ALPHAY
  SUM_A=SUM_A+ALPHAX*ALPHAY
  NN=NS(IDX,IDY)
  !N=NN
  CALL SPLE(NORD,XS(:,1:NN,IDX,IDY),G,C(1:NN+M,IDX,IDY),XE,ZXY)
ENDIF

IF (IDX1.NE.0) THEN
  IF (NS(IDX1,IDY).GE.M) THEN
    NM=1
    SUM_AX=SUM_AX+1.-ALPHAX
    SUM_A=SUM_A+(1-ALPHAX)*ALPHAY
    NN=NS(IDX1,IDY)
    CALL SPLE(NORD,XS(:,1:NN,IDX1,IDY),G,C(1:NN+M,IDX1,IDY),XE,ZX1Y)
  ENDIF
ENDIF

IF (IDY1.NE.0) THEN
  IF (NS(IDX,IDY1).GE.M) THEN
    NM=1
    SUM_AY=SUM_AY+1.-ALPHAY
    SUM_A=SUM_A+ALPHAX*(1-ALPHAY)
    NN=NS(IDX,IDY1)
    CALL SPLE(NORD,XS(:,1:NN,IDX,IDY1),G,C(1:NN+M,IDX,IDY1),XE,ZXY1)
  ENDIF
ENDIF

IF (IDX1.NE.0 .AND. IDY1.NE.0) THEN
  IF (NS(IDX1,IDY1).GE.M) THEN
    NM=1
    SUM_A=SUM_A+(1-ALPHAX)*(1-ALPHAY)
    NN=NS(IDX1,IDY1)
    CALL SPLE(NORD,XS(:,1:NN,IDX1,IDY1),G,C(1:NN+M,IDX1,IDY1),XE,ZX1Y1)
  ENDIF
ENDIF

IF (NM==1) THEN
  IF (IDX1.NE.0 .AND. IDY1.EQ.0) THEN
    ZE=(ALPHAX*ZXY+(1-ALPHAX)*ZX1Y)/SUM_AX
  ELSEIF (IDX1.EQ.0 .AND. IDY1.NE.0) THEN
    ZE=(ALPHAY*ZXY+(1-ALPHAY)*ZXY1)/SUM_AY
  ELSEIF (IDX1.NE.0 .AND. IDY1.NE.0) THEN
    ZE=(ALPHAX*ALPHAY    *ZXY  + (1-ALPHAX)*    ALPHAY*ZX1Y + &
         ALPHAX*(1-ALPHAY)*ZXY1 + (1-ALPHAX)*(1-ALPHAY)*ZX1Y1) / SUM_A 
  ELSE
    ZE=ZXY
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2E1',1,ZHOOK_HANDLE)

END SUBROUTINE SPLB2E1
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  APPELEE PAR INTERPOL_SPLINES.F90
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!12. 
SUBROUTINE SPLB2E(NORD,M,NSDI,NSDJ,G,C,XE,ZE)

IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
INTEGER, INTENT(IN) :: M
INTEGER, INTENT(IN) :: NSDI,NSDJ
REAL, DIMENSION(:,:), INTENT(IN) :: G
REAL,DIMENSION(:,:,:), INTENT(IN) :: C
REAL, DIMENSION(:,:), INTENT(IN) :: XE
REAL, DIMENSION(:), INTENT(OUT) :: ZE

INTEGER, DIMENSION(2) :: NSD
INTEGER :: J, ID, NE
INTEGER :: IDX, IDX1, IDY, IDY1
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2E',0,ZHOOK_HANDLE)
NE=SIZE(XE,2)

NSD(1)=NSDI
NSD(2)=NSDJ

DO J=1,NE
  ID=1
  CALL SPLBFIN(ID,NSD,XE(:,J),AI,IDX,IDX1)
  ID=2
  CALL SPLBFIN(ID,NSD,XE(:,J),AI,IDY,IDY1)
  CALL SPLB2E1(NORD,M,G,C,XE(:,J),IDX,IDX1,IDY,IDY1,ZE(J))
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2E',1,ZHOOK_HANDLE)

END SUBROUTINE SPLB2E

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         PARTIE "C"
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! Calculs EISRS1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!13. 
SUBROUTINE TRED2(A,D,E,Z)
!EISRS1
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: A
REAL, DIMENSION(:), INTENT(OUT)   :: D
REAL, DIMENSION(:), INTENT(OUT)  :: E
REAL, DIMENSION(:,:), INTENT(OUT):: Z

INTEGER :: N, I, II, J, K, L, JP1
REAL :: F, G, H, SCALE, HH
REAL(KIND=JPRB) :: ZHOOK_HANDLE


IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:TRED2',0,ZHOOK_HANDLE)
N=SIZE(A,1)

!Z=A sur la partie gauceh jusqu'à la diagonale
DO I=1,N
  DO J=1,I
    Z(I,J)=A(I,J)
  ENDDO
ENDDO

IF (N.NE.1) THEN
  DO II=2,N
    !I varie de N à 2
    I=N+2-II
    !L varie de N-1 à 1
    L=I-1
    H=0.0
    SCALE=0.0
    IF (L.GE.2) THEN
      !scale = somme des éléments de la ligne à l'exclusion du diagonal
      DO K=1,L
        SCALE=SCALE+ABS(Z(I,K))
      ENDDO
      IF (SCALE.NE.0.0) THEN
        !normalisation de Z par la valeur de la ligne
        DO K=1,L
          Z(I,K)=Z(I,K)/SCALE
          !H = somme des carrés de Z / (somme des ABS(Z)) au carré
          H=H+Z(I,K)**2
        ENDDO
        F=Z(I,L) 
        !SQRT(H) signé par lélément diagonal-1 de la ligne
        G=-SIGN(SQRT(H),F)
        !moyenne des éléments au carrés de la ligne, signée
        E(I)=SCALE*G
        H=H-F*G
        Z(I,L)=F-G
        F=0.0
        DO J=1,L
          Z(J,I)=Z(I,J)/(SCALE*H)
          G=0.0
          DO K=1,J
            G=G+Z(J,K)*Z(I,K)
          ENDDO
          JP1=J+1
          IF (L.GE.JP1) THEN
            DO K=JP1,L
              G=G+Z(K,J)*Z(I,K)
            ENDDO
          ENDIF
          E(J)=G/H
          F=F+E(J)*Z(I,J)
        ENDDO
        HH=F/(H+H)
        DO J=1,L
          F=Z(I,J)
          G=E(J)-HH*F
          E(J)=G
          DO K=1,J
            Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
          ENDDO
        ENDDO
        DO K=1,L
          Z(I,K)=SCALE*Z(I,K)
        ENDDO
      ELSE
        E(I)=Z(I,L)
      ENDIF
    ELSE
      E(I)=Z(I,L)
    ENDIF
    D(I)=H
  ENDDO
ENDIF

D(1)=0.0
E(1)=0.0
DO I=1,N
  L=I-1
  IF (D(I).NE.0.0) THEN
    DO J=1,L
      G=0.0
      DO K=1,L
        G=G+Z(I,K)*Z(K,J)
      ENDDO
      DO K=1,L
        Z(K,J)=Z(K,J)-G*Z(K,I)
      ENDDO
    ENDDO
  ENDIF
  D(I)=Z(I,I)
  Z(I,I)=1.0
  IF (L.GE.1) THEN
    DO J=1,L
      Z(I,J)=0.0
      Z(J,I)=0.0
    ENDDO
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:TRED2',1,ZHOOK_HANDLE)

END SUBROUTINE TRED2
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!14. 
SUBROUTINE TQL2_2(D,E,Z,IERR)
!EISRS1
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(INOUT) :: D
REAL, DIMENSION(:), INTENT(INOUT) :: E
REAL, DIMENSION(:,:), INTENT(OUT) :: Z
INTEGER, INTENT(OUT) :: IERR

INTEGER :: I, II, J, K, L, M, N, MML
REAL :: B, C, F, G, H, P, R, S
REAL :: MACHEP
REAL(KIND=JPRB) :: ZHOOK_HANDLE


IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:TQL2_2',0,ZHOOK_HANDLE)
MACHEP=2.**(-52)
IERR=0
N=SIZE(D)

IF (N.NE.1) THEN
  DO I=2,N
    E(I-1)=E(I)
  ENDDO
  F=0.0
  B=0.0
  E(N)=0.0
  DO L=1,N
    J=0
    H=MACHEP*(ABS(D(L))+ABS(E(L)))
    IF (B.LT.H) B=H
    DO M=L,N
      IF (ABS(E(M)).LE.B) EXIT
    ENDDO
    IF (M.NE.L) THEN
      DO WHILE (ABS(E(L)).GT.B) 
        IF (J.EQ.30) THEN
          IERR=L
          IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:TQL2_2',1,ZHOOK_HANDLE)
          RETURN
        ENDIF
        J=J+1
        P=(D(L+1)-D(L)) / (2.0 * E(L))
        R=SQRT(P*P+1.0)
        H=D(L)-E(L) / (P+SIGN(R,P))
        DO I=L,N
          D(I)=D(I)-H
        ENDDO
        F=F+H
        P=D(M)
        C=1.0
        S=0.0
        MML=M-L
        DO II=1,MML
          I=M-II
          G=C*E(I)
          H=C*P
          IF (ABS(P).GE.ABS(E(I))) THEN
            C=E(I)/P
            R=SQRT(C*C+1.0)
            E(I+1)=S*P*R
            S=C/R
            C=1.0/R
          ELSE
            C=P/E(I)
            R=SQRT(C*C+1.0)
            E(I+1)=S*E(I)*R
            S=1.0/R
            C=C*S
          ENDIF
          P=C*D(I)-S*G
          D(I+1)=H+S*(C*G+S*D(I))
          DO K=1,N
            H=Z(K,I+1)
            Z(K,I+1)=S*Z(K,I)+C*H
            Z(K,I)=C*Z(K,I)-S*H
          ENDDO
        ENDDO
        E(L)=S*P
        D(L)=C*P
      ENDDO
    ENDIF
    D(L)=D(L)+F
  ENDDO
  DO II=2,N
    I=II-1
    K=I
    P=D(I)
    DO J=II,N
      IF (D(J).LT.P) THEN
        K=J
        P=D(J)
      ENDIF
    ENDDO
    IF (K.NE.I) THEN
      D(K)=D(I)
      D(I)=P
      DO J=1,N
        P=Z(J,I)
        Z(J,I)=Z(J,K)
        Z(J,K)=P
      ENDDO
    ENDIF
  ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:TQL2_2',1,ZHOOK_HANDLE)

END SUBROUTINE TQL2_2
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!15. SPLR,SPLU
SUBROUTINE EISRS1(AR,WR,ZR,WORK,IERR)
!
!     all eigenvalues and corresponding eigenvectors of a real
!     symmetric matrix
!
IMPLICIT NONE
!toutes les dimensions sont N: ok
REAL, DIMENSION(:,:), INTENT(IN) :: AR
REAL, DIMENSION(:), INTENT(OUT) :: WR
REAL, DIMENSION(:,:), INTENT(OUT) :: ZR
REAL, DIMENSION(:), INTENT(OUT) :: WORK
INTEGER, INTENT(OUT) :: IERR
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:EISRS1',0,ZHOOK_HANDLE)
 CALL TRED2(AR,WR,WORK,ZR)
 CALL TQL2_2(WR,WORK,ZR,IERR)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:EISRS1',1,ZHOOK_HANDLE)

END SUBROUTINE EISRS1

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!        GROUPES DES PROCEDURES V
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!16.
SUBROUTINE SPLV(B,W,N,P,RES)
!SP0NOP
IMPLICIT NONE

REAL,DIMENSION(:),INTENT(IN) :: B
REAL,DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES
!
INTEGER :: A, D, I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLV',0,ZHOOK_HANDLE)
A=0.
RES=0.
DO I=1,SIZE(B)
  D=B(I)+N*P
  A=A+1./D
  RES=RES+W(I)**2 / D**2
ENDDO
RES=N*RES/A**2
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLV',1,ZHOOK_HANDLE)

END SUBROUTINE SPLV
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!              GROUPE DES PROCEDURES P
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!PROCEDURES INTERMEDIAIRES POUR SPLPS2V
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!17. S2VI=somme(W(i)**2)/size(W)
SUBROUTINE SPLS2VI(W,RES)
!
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: W
REAL, INTENT(OUT) :: RES

INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLS2VI',0,ZHOOK_HANDLE)
RES = 0.
DO I=1,SIZE(W)
  RES = RES + W(I)**2
ENDDO
RES=RES/SIZE(W)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLS2VI',1,ZHOOK_HANDLE)

END SUBROUTINE SPLS2VI
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!18. calcul de BN en fonction de W, B, N et P
SUBROUTINE SPLBVM(B,W,N,P,RES)
!SPLS2V, SP0VPQ
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B, W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES
!
REAL :: A
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBVM',0,ZHOOK_HANDLE)
RES=0.
A=0.
DO I=1,SIZE(B)
  RES=RES+(W(I)**2)/(B(I)+N*P)**2
  A=A+1./(B(I)+N*P)
ENDDO
RES=RES/A
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBVM',1,ZHOOK_HANDLE)

END SUBROUTINE SPLBVM
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!19. 
SUBROUTINE SPLS2V(BI,WI,N,P,RES)
!SPLPS2V, SPLP
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: BI !dimension N-M
REAL, DIMENSION(:), INTENT(IN) :: WI !dimension N-M
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLS2V',0,ZHOOK_HANDLE)
 CALL SPLBVM(BI,WI,N,P,RES)
RES=N*P*RES
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLS2V',1,ZHOOK_HANDLE)

END SUBROUTINE SPLS2V
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!20.
SUBROUTINE SPLDS2V(B,W,N,P,RES)
!SPLPS2V
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES

REAL :: DA, DB, A1, A2, A3, B2, C1, C2
INTEGER :: J, I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDS2V',0,ZHOOK_HANDLE)
DA=0.
DB=0.
DO I=1,SIZE(B)
  A1=1./(B(I)+N*P)
  A2=A1**2
  A3=A1**3
  DA=DA+A1
  B2=W(J)**2
  DO J=1,SIZE(B)
    C1=1./(B(J)+N*P)
    C2=C1**2
    DB=DB+B2*(A2*C1+N*P*(-2*A3*C1+A2*C2))
  ENDDO
ENDDO
RES=(N/(DA**2))*DB
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDS2V',1,ZHOOK_HANDLE)

END SUBROUTINE SPLDS2V
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!21. SPLPS2V procedure 
SUBROUTINE SPLPS2V(B,W,N,P0,S2,P,IREP)
!SPLP
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(INOUT) :: P0
REAL, INTENT(IN) :: S2
REAL, INTENT(OUT) :: P
INTEGER, INTENT(OUT) :: IREP

INTEGER, PARAMETER :: EPS=1.E-2
REAL :: P1, RINF, RS, DRS
INTEGER :: NITER
REAL(KIND=JPRB) :: ZHOOK_HANDLE

! Calcul de la valeur du s2 de Wahba pour p=infini
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPS2V',0,ZHOOK_HANDLE)
 CALL SPLS2VI(W,RINF)
IF (RINF.LE.S2) THEN
  IREP=-6
  P0=0.
  WRITE(*,FMT='(A53,G15.5)') &
    "SPLPS2V : S2 > VALEUR DE LA FONCTION POUR P INFINI = ",RINF
  P=(B(SIZE(B))**2)/B(1)
  IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPS2V',1,ZHOOK_HANDLE)
  RETURN
ENDIF

P1=0.
! Iterations de Newton
DO NITER=1,NMAX
  P0=P1
  CALL SPLS2V(B,W,N,P0,RS)
  CALL SPLDS2V(B,W,N,P0,DRS)

  P1=P0+(S2-RS)/DRS

  IF (ABS(P1-P0)/P0.LT.EPS) THEN
    IF (P1.GE.0.) THEN
      IREP=0
      P=P1
      P0=P1
    ELSE
      IREP=-3
      P=0.
      P0=0.
      WRITE(*,FMT='(A34,G15.5)') &
        "SPLPS2V : SOLUTION NEGATIVE : P = ",P
    ENDIF
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPS2V',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ENDDO

IREP=-5
P=P1
P0=0.
WRITE(*,FMT='(A48,A23,G15.5)') &
  "SPLPS2V : NOMBRE MAXIMAL D ITERATIONS ATTEINT : ",&
  "VALEUR DE P ATTEINTE : ",P1
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPS2V',1,ZHOOK_HANDLE)

END SUBROUTINE SPLPS2V
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!PROCEDURES INTERMEDIAIRES POUR SPLPR
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!22. Comme SPLS2VI mais on ne divise pas par SIZE(W) mais par N
SUBROUTINE SPLRI(W,N,RES)
!SPLPR
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(OUT) :: RES

INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLRI',0,ZHOOK_HANDLE)
RES=0.
DO I=1,SIZE(W)
  RES = RES + W(I)**2
ENDDO
RES=RES/N
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLRI',1,ZHOOK_HANDLE)

END SUBROUTINE SPLRI
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!23. RS=SQRT(S2/(SOMME((W(I)/B(I))**2)*N)
SUBROUTINE SPLPR0(B,W,N,S2,P)
!SPLPR
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: S2
REAL, INTENT(OUT) :: P

REAL :: RES
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR0',0,ZHOOK_HANDLE)
RES=0.
DO I=1,SIZE(W)
  RES=RES+(W(I)/B(I))**2
ENDDO
P=SQRT(S2/(RES*N))
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR0',1,ZHOOK_HANDLE)

END SUBROUTINE SPLPR0   
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!24. RS=N*SOMME((W(I)/(B(I)+N*P))**2)*P**2
SUBROUTINE SPLRS(B,W,N,P,RES)
!SPLP, SPLPR, SP0CVQ 
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT):: RES

INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLRS',0,ZHOOK_HANDLE)
RES=0.
DO I=1,SIZE(B)
  RES=RES+(W(I)/(B(I)+N*P))**2
ENDDO
RES=N*RES*P**2
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLRS',1,ZHOOK_HANDLE)

END SUBROUTINE SPLRS
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!25. DRS=2*N*P*(SOMME(W(I)**2*B(I)/(B(I)+N*P)**3)
SUBROUTINE SPLDRS(B,W,N,P,RES)
!SPLPR
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES
!
INTEGER :: J
REAL :: D
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDRS',0,ZHOOK_HANDLE)
RES=0.
DO J=1,SIZE(B)
  D=B(J)+N*P
  RES=RES+W(J)**2*B(J)/(D**3)
ENDDO
RES=2.*N*P*RES
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDRS',1,ZHOOK_HANDLE)

END SUBROUTINE SPLDRS   
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!26. SPLPR procedure: parallèle de splps2v
SUBROUTINE SPLPR(N,W,B,P0,S2,P,IREP)
!SPLP
IMPLICIT NONE

INTEGER, INTENT(IN) :: N
REAL, DIMENSION(:), INTENT(IN) :: W !N-M
REAL, DIMENSION(:), INTENT(IN) :: B !N-M
REAL, INTENT(INOUT) :: P0
REAL, INTENT(INOUT) :: S2
REAL, INTENT(INOUT) :: P
INTEGER, INTENT(OUT) :: IREP

INTEGER, PARAMETER :: EPS=1.E-2
REAL :: RS, DRS, P1, RINF
INTEGER :: NITER
REAL(KIND=JPRB) :: ZHOOK_HANDLE

! Calcul de la valeur de la fonction de Reinsch pour p=infini

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR',0,ZHOOK_HANDLE)
 CALL SPLRI(W,N,RINF)
IF (RINF.LE.S2) THEN
  IREP=-6
  P0=0.
  WRITE(*,FMT='(A51,G15.5)') &
    "SPLPR : S2 > VALEUR DE LA FONCTION POUR P INFINI = ",RINF
  P=(B(SIZE(B))**2)/B(1)
  IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR',1,ZHOOK_HANDLE)
  RETURN
ENDIF

! Recherche du point de depart de l'iteration
IF (P0.EQ.0.) CALL SPLPR0(B,W,N,S2,P0)
P1=P0

! Iterations de Newton
DO NITER=1,NSDMAX
  P0=P1
  CALL SPLRS(B,W,N,P0,RS)
  CALL SPLDRS(B,W,N,P0,DRS)

  P1=P0+(S2-RS)/DRS

  IF (ABS(P1-P0)/P0.LT.EPS) THEN
    IF (P1.GE.0.) THEN
      IREP=0
      P=P1
      P0=P1
    ELSE
      IREP=-3
      P=0.
      P0=0.
      WRITE(*,FMT='(A32,G15.5)') &
        "SPLPR : SOLUTION NEGATIVE : P = ",P
    ENDIF
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ENDDO

IREP=-5
P=0.
P0=0.
WRITE(*,FMT='(A45,A23,G15.5)') &
  "SPLPR : NOMBRE MAXIMAL D ITERATIONS ATTEINT: ",&
  "VALEUR DE P ATTEINTE : ",P1
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPR',1,ZHOOK_HANDLE)

END SUBROUTINE SPLPR
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!PROCEDURES INTERMEDIAIRES POUR SPLPV
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!27.
SUBROUTINE SPLDV(B,W,N,P,RES)
!SPLPV
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES
!
REAL :: S1, S2, SB2, SB3, A1, A2, B2
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDV',0,ZHOOK_HANDLE)
S1=0.
S2=0.
SB2=0.
SB3=0.
DO I=1,SIZE(B)
  A1=1./(B(I)+N*P)
  A2=A1**2
  B2=A2*W(I)**2
  S1=S1+A1
  S2=S2+A2
  SB2=SB2+B2
  SB3=SB3+B2*A1
ENDDO
RES=S2*SB2-S1*SB3
RES=2.*N**2*RES/(S1**3)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLDV',1,ZHOOK_HANDLE)

END SUBROUTINE SPLDV
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!28.
SUBROUTINE SPLD2V(B,W,N,P,RES)
!SPLPV
IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
INTEGER, INTENT(IN) :: N
REAL, INTENT(IN) :: P
REAL, INTENT(OUT) :: RES

REAL :: S1, S2, S3, SB2, SB3, SB4
REAL :: A1, A2, B2
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLD2V',0,ZHOOK_HANDLE)
S1=0.
S2=0.
S3=0.
SB2=0.
SB3=0.
SB4=0.
DO I=1,SIZE(B)
  A1=1./(B(I)+N*P)
  A2=A1**2
  B2=A2*W(I)**2
  S1=S1+A1
  S2=S2+A2
  S3=S3+A2*A1
  SB2=SB2+B2
  SB3=SB3+B2*A1
  SB4=SB4+B2*A2
ENDDO
RES=3.*S2**2*SB2-4.*S2*S1*SB3+3.*S1**2*SB4-2.*S3*S1*SB2
RES=RES/(S1**4)
RES=RES*2.*N**3
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLD2V',1,ZHOOK_HANDLE)

END SUBROUTINE SPLD2V
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!29.
SUBROUTINE SPLPV(N,B,W,P0,P,IREP)
!SPLP
IMPLICIT NONE

INTEGER, INTENT(IN) :: N
REAL, DIMENSION(:), INTENT(IN):: B, W !N-M
REAL, INTENT(INOUT) :: P0
REAL, INTENT(OUT) :: P
INTEGER, INTENT(OUT) :: IREP

INTEGER, PARAMETER:: EPS=XSURF_TINY, EPSR=1.E-2
REAL :: DVM, D2VM, P1
INTEGER :: IFLAG, NITER
REAL(KIND=JPRB) :: ZHOOK_HANDLE


! Calcul de Vm'(0) . Si Vm'(0) > 0 alors popt = 0.
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',0,ZHOOK_HANDLE)
 CALL SPLDV(B,W,N,0.,DVM)

IF (DVM.GT.0.) THEN
  IREP=0.
  P=0.
  IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)
  RETURN
ENDIF

! Recherche du point de depart p0 de l'iteration de Newton
IFLAG=0

DO WHILE(IFLAG==0)

  IF (P0.EQ.0.) THEN

    IFLAG=1
    P0=B(1)/N

    !on augmente P0 et on calcul D2VM; quand D2VM est supérieur à 0, 
    !on passe à la suite
    DO NITER=1,NSDMAX
      CALL SPLD2V(B,W,N,P0,D2VM)
      IF (D2VM.GT.0.) EXIT
      P0=P0*10.
    ENDDO

    IF (D2VM.LE.0.) THEN
      IREP=-1
      P0=0.
      P=0.
      WRITE(*,FMT='(A41)') &
        "SPLPV : PAS DE MINIMUM: D2VM < 0 PARTOUT "
      IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)
      RETURN
    ENDIF

  ENDIF

  P1=P0

  DO NITER=1,NSDMAX

    P0=P1
    CALL SPLDV(B,W,N,P0,DVM)
    CALL SPLD2V(B,W,N,P0,D2VM)

    !Si D2Vm est très proche de 0, on sort et P=P0
    IF (ABS(D2VM).LT.EPS) THEN
      P0=0.
      IF (IFLAG.EQ.0) EXIT
      IREP=-2
      P=P0
      WRITE(*,FMT='(A28)') "SPLPV : PASSAGE PAR D2VM = 0"
      IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)
      RETURN
    ENDIF

    P1=P0-DVM/D2VM

     !si l'écart entre P0 et P1 est suffisamment petit
    IF (ABS(P1-P0)/P0.LT.EPSR) THEN
       !Si P1 est négatif, on repart dans la boucle
      IF (P1.LT.0.) THEN
        P0=0.
        IF (IFLAG.EQ.0) EXIT
        IREP=-3
        P=0.
        WRITE(*,FMT='(A33,G15.5)') &
          "SPLPV : SOLUTION NEGATIVE : P = ",P1
      ELSE
        !si P1 et positif et D2VM positif, on garde P1
        IF (D2VM.GE.0.) THEN
          IREP=0
          P=P1
          P0=P1
        ELSE
          !si P1 est négatif, on repart dans la boucle
          P0=0.
          IF (IFLAG.EQ.0) EXIT
          IREP=-4
          P=0.
          WRITE(*,FMT='(A28,G15.5)') &
            "SPLPV : MAXIMUM DE VM : P = ",P1
        ENDIF
      ENDIF
      IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)
      RETURN
    ENDIF
  ENDDO

  P0=0.
  IF (IFLAG.NE.0) THEN
    IREP=-5
    P=P1
    WRITE(*,FMT='(A46,A23,G15.5)') &
      "SPLPV : NOMBRE MAXIMAL D ITERATIONS ATTEINT : ",&
      "VALEUR DE P ATTEINTE : ",P1
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)
    RETURN
  ENDIF

ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLPV',1,ZHOOK_HANDLE)

END SUBROUTINE SPLPV
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!30.
SUBROUTINE SPLP(LISSAGE,N,B,W,P,S2,IREP)
!SP0NOP
IMPLICIT NONE

INTEGER, INTENT(IN) :: LISSAGE
INTEGER, INTENT(IN) :: N
REAL, DIMENSION(:), INTENT(IN) :: B
REAL, DIMENSION(:), INTENT(IN) :: W
REAL, INTENT(INOUT) :: P
REAL, INTENT(INOUT) :: S2
INTEGER, INTENT(OUT) :: IREP

REAL :: P0
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLP',0,ZHOOK_HANDLE)
IF (LISSAGE.EQ.1) THEN
!  Recherche de la valeur de P pour S2 connu, par la methode de Wahba
  P0=0.
  CALL SPLPS2V(B,W,N,P0,S2,P,IREP)
  IF (IREP.NE.0) THEN
    IREP=-2
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLP',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ELSEIF (LISSAGE.EQ.10) THEN
!  Recherche de la valeur de P pour S2 connu, par la methode de Reinsch
  P0=0.
  CALL SPLPR(N,W,B,P0,S2,P,IREP)
  IF (IREP.NE.0) THEN
    IREP=-2
    IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLP',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ELSEIF (LISSAGE.EQ.2) THEN
!  Recherche de la valeur de P par la methode de G.C.V
  P0=0.
  CALL SPLPV(N,B,W,P0,P,IREP)
  IF (IREP.NE.0) IREP=-3
!  Calcul de la variance d'erreur de mesure correspondante
  CALL SPLS2V(B,W,N,P,S2)
ELSEIF (LISSAGE.EQ.3) THEN
!  Calcul de l'ajustement correspondant a une valeur de P donnee
  CALL SPLRS(B,W,N,P,S2)
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLP',1,ZHOOK_HANDLE)

END SUBROUTINE SPLP
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!          GROUPE DES PROCEDURES T et R
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!31. TT procedure
SUBROUTINE SPLTT(X,DS,IW,TM,TT,IREP)
!SPLR
IMPLICIT NONE

REAL,DIMENSION(:,:),INTENT(IN) :: X !ND,N
REAL,DIMENSION(:),INTENT(IN) :: DS !N
INTEGER,DIMENSION(:,:),INTENT(IN) :: IW !ND,M
REAL,DIMENSION(:,:),INTENT(OUT) :: TM !N,M
REAL,DIMENSION(:,:),INTENT(OUT) :: TT !M,M
INTEGER, INTENT(OUT) :: IREP

REAL(KIND=JPRB) :: ZHOOK_HANDLE

! Calcul de la matrice T des monomes
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLTT',0,ZHOOK_HANDLE)
 CALL SPLT(X,IW,TM)
! Calcul de Tm=(Ds-1)*T
 CALL MXIDML(DS,TM,TM)
! Calcul de Tm'*Tm
 CALL MXMSPL(TRANSPOSE(TM),TM,TT)
! Calcul de (Tm'*Tm)-1
 CALL SMXINV(TT,IREP)
! Test du compte-rendu de smxinv
IF (IREP.NE.0) WRITE(*,FMT='(A27)') "SPLTT: MATRICE TT NON INVERSIBLE"
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLTT',1,ZHOOK_HANDLE)

END SUBROUTINE SPLTT
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!32. r procedure
SUBROUTINE SPLR_1(NORD,X,DS,IW,TTT,R,IREP)
!SP0NOP, SP0CVQ
IMPLICIT NONE

INTEGER,INTENT(IN) :: NORD
REAL,DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL,DIMENSION(:),INTENT(IN) :: DS !N
INTEGER,DIMENSION(:),INTENT(OUT) :: IW !ND
REAL,DIMENSION(:),INTENT(OUT) :: TTT !N
REAL,DIMENSION(:,:),INTENT(OUT) :: R !N,N
INTEGER, INTENT(OUT)::IREP

INTEGER :: N, I, J
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLR_1',0,ZHOOK_HANDLE)
N=SIZE(X,2)

! R = I
DO I=1,N
  DO J=1,N
    IF (I.EQ.J) THEN
      R(I,I)=1.
    ELSE
      R(I,J)=0.
    ENDIF
  ENDDO
ENDDO

IREP=0

!  Calcul de (Ds-1)*R
 CALL MXIDML(DS,R,R)

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLR_1',1,ZHOOK_HANDLE)

END SUBROUTINE SPLR_1
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!32.
SUBROUTINE SPLR_2(NORD,X,DS,IW,TTT,R,IREP)

IMPLICIT NONE

INTEGER,INTENT(IN) :: NORD
REAL,DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL,DIMENSION(:),INTENT(IN) :: DS !N
INTEGER,DIMENSION(:,:),INTENT(OUT) :: IW !ND,M
REAL,DIMENSION(:,:),INTENT(OUT) :: TTT !M,N
REAL,DIMENSION(:,:),INTENT(OUT) :: R !N,N-M
INTEGER, INTENT(OUT)::IREP

REAL,DIMENSION(SIZE(X,2),SIZE(X,2)) :: C !N,N
REAL,DIMENSION(SIZE(X,2),SIZE(IW,2))  :: TM !N,M
REAL,DIMENSION(SIZE(IW,2),SIZE(IW,2)) :: TT !M,M
REAL, DIMENSION(SIZE(X,2),SIZE(X,2)) :: R1
REAL,DIMENSION(SIZE(X,2)) :: VP !N
REAL,DIMENSION(SIZE(X,2)) :: WORK !N
INTEGER :: M, N, J
INTEGER, DIMENSION(1) :: IMIN
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLR_2',0,ZHOOK_HANDLE)
N=SIZE(X,2)
M=SIZE(IW,2)

!     Recherche des monomes de degre inferieur ou egal a nord-1
 CALL SPLIE(NORD-1,IW)
!----------------------------------------------------------
!     Calcul de la matrice (Tm'*Tm)-1
 CALL SPLTT(X,DS,IW,TM,TT,IREP)
!     Calcul de (T'm*Tm)-1 Tm'
 CALL MXMSPL(TT,TRANSPOSE(TM),TTT)
!R1(:,1:M)=TRANSPOSE(TTT)
!------------------------------------------------------------
!     Calcul de Tm ((Tm'*Tm)-1) Tm'
 CALL MTXAXM(TRANSPOSE(TM),TT,C)
DO J=1,N
  C(J,J)=C(J,J)-1.
ENDDO
C(:,:)=-C(:,:)
!-----------------------------------------------------------
!     Calcul des vecteurs propres de I - Tm c ((Tm'*Tm)-1) c Tm'
 CALL EISRS1(C,VP,R1,WORK,IREP)

!     Test du signe des valeurs propres de C
IMIN=MINLOC(VP(M+1:N))
IF(VP(M+IMIN(1)).LE.0) THEN
  IREP=-1
  WRITE(*,FMT='(A31)') "SPLR: VALEUR PROPRE < 0 DE PROJ"
  IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLR_2',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!     Compte-rendu OK
IREP=0
!  Calcul de (Ds-1)*R
R=R1(:,M+1:N)
 CALL MXIDML(DS,R,R)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLR_2',1,ZHOOK_HANDLE)

END SUBROUTINE SPLR_2
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!             GROUPE DES PROCEDURES U
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!33. U procedure
SUBROUTINE SPLU(NORD,X,G,R,RK,U,DB)
!SP0NOP, SP0CVQ
IMPLICIT NONE

INTEGER, INTENT(IN) :: NORD
REAL,DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL,DIMENSION(:,:), INTENT(IN) :: G ! ND,ND
REAL,DIMENSION(:,:), INTENT(IN) :: R !N,N-M
REAL,DIMENSION(:,:), INTENT(OUT) :: RK !N,N
REAL,DIMENSION(:,:), INTENT(OUT) :: U !N-M,N-M
REAL,DIMENSION(:), INTENT(OUT) :: DB !N-M

REAL,DIMENSION(SIZE(R,2),SIZE(R,2)) :: RKR !N-M,N-M
REAL,DIMENSION(SIZE(R,2)) :: WORK !N-M

INTEGER :: M, N, IREP
INTEGER, DIMENSION(1) :: IMIN
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLU',0,ZHOOK_HANDLE)
N=SIZE(X,2)
M=SIZE(X,2)-SIZE(R,2)

! Calcul de la matrice K des noyaux
 CALL SPLK(NORD,X,X,G,RK)
! Calcul de R'*K*R
 CALL MTXAXM(R,RK,RKR)
! Calcul de la matrice U des vecteurs propres de R'*K*R
 CALL EISRS1(RKR,DB,U,WORK,IREP)
! Test du signe des valeurs propres de R'KR
IMIN=MINLOC(DB)
IF (DB(IMIN(1)).LE.0)  &
   WRITE(*,FMT='(A31)') "SPLU : VALEUR PROPRE < 0 DE RKR"
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLU',1,ZHOOK_HANDLE)

END SUBROUTINE SPLU     
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!               GROUPE DES PROCEDURES W
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!34. w procedure
SUBROUTINE SPLW(Z,R,U,W,RZ)
!SP0NOP, SP0CVQ
IMPLICIT NONE

REAL,DIMENSION(:), INTENT(IN) :: Z !N
REAL, DIMENSION(:,:),INTENT(IN) :: R !N,N-M
REAL,DIMENSION(:,:),INTENT(IN) :: U !N-M,N-M
REAL, DIMENSION(:),INTENT(OUT) :: W !N-M
REAL,DIMENSION(:),INTENT(OUT) :: RZ !N-M

INTEGER :: N, M
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLW',0,ZHOOK_HANDLE)
N=SIZE(Z)
M=SIZE(Z)-SIZE(R,2)

! Calcul de R'*Z
 CALL MXMSPL(TRANSPOSE(R),Z,RZ)
! Calcul de U'R'Z
 CALL MXMSPL(TRANSPOSE(U),RZ,W)
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLW',1,ZHOOK_HANDLE)

END SUBROUTINE SPLW
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!              GROUPE DES PROCEDURES Q
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!35. splvpQ procedure
SUBROUTINE SPLVPQ(P,R,RK,U,DB,Q)
!SP0CVQ
IMPLICIT NONE

REAL, INTENT(IN) :: P
REAL, DIMENSION(:,:), INTENT(IN) :: R !N,N-M
REAL, DIMENSION(:,:), INTENT(IN) :: RK ! N,N
REAL, DIMENSION(:,:), INTENT(IN) :: U !N-M,N-M
REAL, DIMENSION(:), INTENT(IN) :: DB !N-M
REAL, DIMENSION(:,:), INTENT(OUT) :: Q ! N,N

REAL, DIMENSION(SIZE(R,1),SIZE(R,1)):: Q1 !N,N
REAL, DIMENSION(SIZE(U,1),SIZE(U,2))::QW1,QW2 !N-M,N-M
INTEGER :: N, J
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLVPQ',0,ZHOOK_HANDLE)
N=SIZE(R,1)

! Calcul de (DB+n*p*I)-1
QW1(:,:)=0.
DO J=1,SIZE(DB)
  QW1(J,J)=1./(DB(J)+N*P)
ENDDO

! Calcul de U((DB+n*p*I)-1)U'
 CALL MTXAXM(TRANSPOSE(U),QW1,QW2)
! Calcul de RU((DB+n*p*I)-1))UR'
 CALL MTXAXM(TRANSPOSE(R),QW2,Q)
! Calcul de QK
 CALL MXMSPL(Q,RK,Q1)

! Calcul de I - QK
Q1(:,:)=-Q1(:,:)
DO J=1,N
  Q1(J,J)=Q1(J,J)+1
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLVPQ',1,ZHOOK_HANDLE)

END SUBROUTINE SPLVPQ
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!              GROUPE DES PROCEDURES C
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!36. 
SUBROUTINE SPLC(Q,Z,RK,DS,TTT,RKC,C)
!SP0NOP, SP0CVQ
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: Q !Q(N,N)
REAL, DIMENSION(:), INTENT(IN) :: Z !Z(N)
REAL, DIMENSION(:,:), INTENT(IN) :: RK !RK(N,n)  
REAL, DIMENSION(:), INTENT(IN) :: DS !DS(N)
REAL, DIMENSION(:,:), INTENT(IN) :: TTT !TTT(M,N)
REAL, DIMENSION(:), INTENT(OUT)  :: RKC !RKC(N)
REAL, DIMENSION(:), INTENT(OUT) :: C !C(N+M)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

INTEGER :: N, M

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLC',0,ZHOOK_HANDLE)
N=SIZE(Z)
M=SIZE(TTT,1)

!     Calcul de c
!     -----------
!     Calcul de c = Qz = R((R'KR+n*p*I)-1)R'z
 CALL MXMSPL(Q,Z,C(1:N))

!     Calcul de d
!     -----------
IF (M.NE.0) THEN
! Calcul de K*c
  CALL MXMSPL(RK,C(1:N),RKC)
! Calcul de z - K*c
  RKC(:)=Z(:)-RKC(:)   
! Calcul de (Ds-1) * (z-K*c)
  RKC(:)=RKC(:)/DS(:)
! Calcul de d = (Tm'*Tm)-1 * Tm' * (Ds-1) * (z-K*c)
  CALL MXMSPL(TTT,RKC,C(N+1:N+M))
ENDIF

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLC',1,ZHOOK_HANDLE)

END SUBROUTINE SPLC
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                  PROCEDURES GLOBALES
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!37. Calcul de M en fonction de NORD et ND (qu'est-ce que c'est que ce ND?)
SUBROUTINE SPLM(ND,NORD,M)
!SP0NOP
IMPLICIT NONE

INTEGER, INTENT(IN) :: ND
INTEGER, INTENT(IN) :: NORD
INTEGER, INTENT(OUT):: M
REAL(KIND=JPRB) :: ZHOOK_HANDLE


IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLM',0,ZHOOK_HANDLE)
IF (ND==1) THEN
  M=NORD
ELSEIF (ND==2) THEN
  M=NORD*(NORD+1)/2.
ELSEIF (ND==3) THEN
  M=NORD*(NORD+1)*(NORD+2)/6.
ELSEIF (ND==4) THEN
  M=NORD*(NORD+1)*(NORD+2)*(NORD+3)/24.
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLM',1,ZHOOK_HANDLE)

END SUBROUTINE SPLM
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!38. 
SUBROUTINE SP0NOP(X,G,Z,DS,LISSAGE,LORDRE,IOPT,NORDOPT,M,S2,P,&
         IW,TTT,R,RK,U,DB,W,RZ,IREP) 
!SP0CVQ
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL, DIMENSION(:,:), INTENT(IN) :: G ! ND,ND
REAL, DIMENSION(:), INTENT(IN) :: Z ! N
REAL, DIMENSION(:), INTENT(IN) :: DS ! N
INTEGER, INTENT(IN) :: LISSAGE
INTEGER, INTENT(IN) :: LORDRE
INTEGER, INTENT(IN) :: IOPT
INTEGER, INTENT(INOUT) :: NORDOPT
INTEGER, INTENT(INOUT) :: M
REAL, INTENT(INOUT) :: S2
REAL, INTENT(INOUT) :: P
INTEGER, DIMENSION(:,:), INTENT(OUT):: IW !ND,M
REAL, DIMENSION(:,:), INTENT(OUT) :: TTT ! M,N
REAL, DIMENSION(:,:), INTENT(OUT) :: R ! N,N-M
REAL,DIMENSION(:,:), INTENT(OUT) :: RK !N,N
REAL,DIMENSION(:,:), INTENT(OUT) :: U !N-M,N-M
REAL,DIMENSION(:), INTENT(OUT) :: DB !N-M
REAL, DIMENSION(:),INTENT(OUT) :: W !N-M
REAL,DIMENSION(:),INTENT(OUT) :: RZ !N-M
INTEGER, INTENT (OUT) :: IREP


REAL, DIMENSION(NORDMAX) :: VM, S2SAVE, PSAVE, MSAVE
INTEGER :: N, ND
INTEGER :: NORDMI, NORDMA, NORDM, NORD
REAL :: VMMIN
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0NOP',0,ZHOOK_HANDLE)
N=SIZE(X,2)
ND=SIZE(X,1)

IF (LORDRE.EQ.0) THEN
!
!  Recherche de p seulement avec nord fixe
!  ---------------------------------------
  IF (IOPT.EQ.0) THEN
!   Calcul de IW, TTT, R
    CALL SPLR(NORDOPT,X,DS,IW,TTT,R,IREP)  
!   Calcul de RK, U, DB    
    CALL SPLU(NORDOPT,X,G,R,RK,U,DB)
  ENDIF
   !calcul de W, RZ
  CALL SPLW(Z,R,U,W,RZ)
  !calcul de P
  CALL SPLP(LISSAGE,N,DB,W,P,S2,IREP)

ELSE

!  Recherche de nordopt et popt
!  ----------------------------

  NORDMI=ND/2+1

!  Calcul de l'ordre maximal pour le nombre de points n
  CALL SPLM(ND,NORD,M)
  DO WHILE(N.GT.M) 
    NORD=NORD+1
    CALL SPLM(ND,NORD,M)
  ENDDO
  NORDMA=MIN(NORD,NORDMAX)
  NORDM=NORDMA

!  Calcul de Vm pour les differents ordre
  DO NORD=NORDMI,NORDMA
    !calcul de M
    CALL SPLM(ND,NORD,M)
    CALL SPLR(NORD,X,DS,IW,TTT,R,IREP)
    CALL SPLU(NORD,X,G,R,RK,U,DB)    
    CALL SPLW(Z,R,U,W,RZ)
    CALL SPLP(LISSAGE,N,DB,W,P,S2,IREP)
    MSAVE(NORD)=M
    S2SAVE(NORD)=S2
    PSAVE(NORD)=P
    IF (IREP.EQ.0) THEN
      !calcul de VM(NORD)
      CALL SPLV(DB,W,N,P,VM(NORD))
      WRITE(*,FMT='(A15,I5,A5,G15.5,A6,G15.5)')&
        "CNORD : NORD = ",NORD," P = ",P," VM = ",VM(NORD)
    ELSE
      IF (NORD.EQ.NORDMI) THEN
        NORDOPT=NORD
        IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0NOP',1,ZHOOK_HANDLE)
        RETURN
      ELSE
        NORDM=NORD-1
        IREP=0
        EXIT
      ENDIF
    ENDIF
  ENDDO

!  Recherche de l'ordre optimal
  VMMIN=XSURF_HUGE
  DO NORD=NORDMI,NORDM
    IF(VM(NORD).LT.VMMIN) THEN
      VMMIN=VM(NORD)
      NORDOPT=NORD
      M=MSAVE(NORD)
      S2=S2SAVE(NORD)
      P=PSAVE(NORD)
    ENDIF
  ENDDO
  WRITE(*,FMT='(A24,I5)') "CNORD : ORDRE OPTIMAL = ",NORDOPT
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0NOP',1,ZHOOK_HANDLE)

END SUBROUTINE SP0NOP
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!39. 
SUBROUTINE SP0CVQ(NORD,M,X,G,Z,DS,S2,P,LISSAGE,LORDRE,IOPT,C,IREP)
!SPLB2C
IMPLICIT NONE

INTEGER, INTENT(INOUT) :: NORD
INTEGER, INTENT(INOUT) :: M
REAL,DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL, DIMENSION(:,:), INTENT(IN) :: G !ND,ND
REAL,DIMENSION(:), INTENT(IN) :: Z ! N
REAL, DIMENSION(:), INTENT(IN) :: DS !N
REAL, INTENT(INOUT) :: S2
REAL, INTENT(INOUT) :: P
INTEGER, INTENT(IN) :: LISSAGE
INTEGER, INTENT(IN) :: LORDRE
INTEGER, INTENT(IN) :: IOPT
REAL, DIMENSION(:), INTENT(OUT) :: C !N+M
INTEGER, INTENT(OUT) :: IREP

INTEGER, DIMENSION(SIZE(X,1),SIZE(C)-SIZE(X,2)) :: IW
REAL, DIMENSION(SIZE(C)-SIZE(X,2),SIZE(X,2)) :: TTT ! M,N
REAL, DIMENSION(SIZE(X,2),2*SIZE(X,2)-SIZE(C)) :: R ! N,N-M
REAL,DIMENSION(SIZE(X,2),SIZE(X,2)) :: RK !N,N
REAL,DIMENSION(2*SIZE(X,2)-SIZE(C),2*SIZE(X,2)-SIZE(C)) :: U !N-M,N-M
REAL,DIMENSION(2*SIZE(X,2)-SIZE(C)):: DB !N-M
REAL, DIMENSION(2*SIZE(X,2)-SIZE(C)) :: W !N-M
REAL,DIMENSION(SIZE(R,2)) :: RZ !N-M
REAL, DIMENSION(SIZE(X,2),SIZE(X,2)) :: Q ! N,N
REAL :: RN
REAL, DIMENSION(SIZE(X,2)) :: RKC !RKC(N)


INTEGER :: N
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0CVQ',0,ZHOOK_HANDLE)
N=SIZE(X,2)

! Test des differents parametres
IF (NORD.LT.1) THEN
  WRITE(*,FMT='(A17)') "SP0CVQ : NORD < 1"
ELSEIF (M.LT.0) THEN
  WRITE(*,'(A14)') "SP0CVQ : M < 0"
ELSEIF (N.LT.M) THEN
  WRITE(*,'(A14)') "SP0CVQ : N < M"
ELSEIF (S2.LT.0.) THEN
  WRITE(*,'(A15)') "SP0CVQ : S2 < 0"
ELSEIF (P.LT.0.) THEN
  WRITE(*,'(A14)') "SP0CVQ : P < 0"
ELSEIF ((LISSAGE.LT.0.OR.LISSAGE.GT.3).AND.LISSAGE.NE.10) THEN
  WRITE(*,'(A27)') "SP0CVQ : LISSAGE < 0 OU > 3"
ELSEIF (LORDRE.LT.0.OR.LORDRE.GT.1) THEN
  WRITE(*,'(A26)') "SP0CVQ : LORDRE < 0 OU > 1"
ELSEIF (IOPT.LT.0.OR.IOPT.GT.1) THEN
  WRITE(*,'(A24)') "SP0CVQ : IOPT < 0 OU > 1"
ELSE

!     Determination de nord opt ou de p pot
!   -----------------------------------------
  IF (LORDRE.EQ.1 .OR. &
       LISSAGE.EQ.1 .OR. LISSAGE.EQ.2 .OR. LISSAGE.EQ.10)  THEN 
       !calcul de P en passant par SPLR, SPLU, SPLW, si LORDRE = 0
       !calcul de NORD, M, S2, P si LORDRE=1
    CALL SP0NOP(X,G,Z,DS,LISSAGE,LORDRE,IOPT,NORD,M,S2,P,&
         IW,TTT,R,RK,U,DB,W,RZ,IREP) 
  ELSEIF (LISSAGE.EQ.0) THEN
    P=0.
  ENDIF
!---------------------------------------------------------------------
!---------------------------------------------------------------------

  IF (LORDRE.EQ.1 .OR. LISSAGE.EQ.0 .OR. LISSAGE.EQ.3) THEN
    IF (IOPT.EQ.0) THEN
!     Calcul de IW, TTT, R
      CALL SPLR(NORD,X,DS,IW,TTT,R,IREP)
      IF (IREP.NE.0 .AND. LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0CVQ',1,ZHOOK_HANDLE)
      IF (IREP.NE.0) RETURN
!     Calcul de  RK, U, DB
      CALL SPLU(NORD,X,G,R,RK,U,DB)  
    ENDIF
!---------------------------------------------------------------------
!   Calcul de W,RZ
    CALL SPLW(Z,R,U,W,RZ)
!   Calcul de S2 pour lissage=3
    IF (LISSAGE.EQ.3) CALL SPLRS(DB,W,N,P,S2)
  ENDIF

!---------------------------------------------------------------------
!---------------------------------------------------------------------
! Calcul de Q
  CALL SPLVPQ(P,R,RK,U,DB,Q)
! Calcul de RN
  CALL SPLBVM(DB,W,N,P,RN)
! Calcul des coefficients C et RKC
  CALL SPLC(Q,Z,RK,DS,TTT,RKC,C)
  IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0CVQ',1,ZHOOK_HANDLE)
  RETURN

ENDIF

IREP=-1
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SP0CVQ',1,ZHOOK_HANDLE)

END SUBROUTINE SP0CVQ
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!40. AI(ND,ND,NSDMAX): calcul de AI en fonction de XD, NSD, INTER; 
!remarque: on dirait que la deuxième dimension de AI est plutôt 2 que ND
SUBROUTINE SPLBSD(NSD,INTER,XD,AI)
!SPLB2C
IMPLICIT NONE

INTEGER, DIMENSION(:), INTENT(IN) :: NSD
INTEGER, INTENT(IN) :: INTER
REAL, DIMENSION(:,:), INTENT(IN) :: XD
REAL, DIMENSION(:,:,:), INTENT(OUT) :: AI
REAL(KIND=JPRB) :: ZHOOK_HANDLE

REAL, DIMENSION(SIZE(NSD)) :: DXI, DXR
INTEGER :: J, I

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBSD',0,ZHOOK_HANDLE)
DO J=1,SIZE(NSD)
  DXI(J)=(XD(2,J)-XD(1,J))/NSD(J)
  DXR(J)=DXI(J)/(2*INTER-2)
ENDDO

DO J=1,SIZE(NSD)
  AI(J,1,1)=XD(1,J)
  DO I=2,NSD(J)
    AI(J,1,I)=XD(1,J)+(I-1)*DXI(J)-DXR(J)
  ENDDO
  AI(J,2,NSD(J))=XD(2,J)
  DO I=1,NSD(J)-1
    AI(J,2,I)=XD(2,J)-(NSD(J)-I)*DXI(J)+DXR(J)
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBSD',1,ZHOOK_HANDLE)

END SUBROUTINE SPLBSD
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!41. récupère les X et Z tels que les valeurs soient toutes comprises entre 
!A et B, dans XS et ZS. NS est la taille finale des tableaux. 
SUBROUTINE SPLBSEL(X,Z,AI,BI,NS,XS,ZS,IREP)
!SPLB2C
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL, DIMENSION(:), INTENT(IN) :: Z !N
REAL, DIMENSION(:), INTENT(IN) :: AI !ND
REAL, DIMENSION(:), INTENT(IN) :: BI !ND
INTEGER, INTENT(OUT) :: NS
REAL, DIMENSION(:,:), INTENT(OUT) :: XS !ND,NMAX
REAL, DIMENSION(:), INTENT(OUT) :: ZS !NMAX
INTEGER, INTENT(OUT) :: IREP

INTEGER :: I, J, I0
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBSEL',0,ZHOOK_HANDLE)
IREP=0
NS=0
DO J=1,SIZE(X,2)
  I0=0
  DO I=1,SIZE(X,1)
   !il faut que tous les X(I,J) soient compris entre AI et BI
    IF (X(I,J).LT.AI(I).OR.X(I,J).GT.BI(I)) THEN 
      I0=1
      EXIT
    ENDIF
  ENDDO
  IF (I0.NE.1) THEN
    NS=NS+1
    IF (NS.GT.SIZE(ZS)) THEN
      IREP=-1
      IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBSEL',1,ZHOOK_HANDLE)
      RETURN
    ENDIF
    DO I=1,SIZE(X,1)
      XS(I,NS)=X(I,J)
    ENDDO
    ZS(NS)=Z(J)
  ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLBSEL',1,ZHOOK_HANDLE)

END SUBROUTINE SPLBSEL
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!42. appelée par INTERPOL_SPLINES
SUBROUTINE SPLB2C(NORD,M,X,G,Z,S2,P,LISSAGE,IOPT,NSDI,NSDJ,&
                        INTER,XD,C,IREP)
!INTERPOL_SPLINES
IMPLICIT NONE

INTEGER, INTENT(INOUT) :: NORD 
INTEGER, INTENT(INOUT) :: M
REAL, DIMENSION(:,:), INTENT(IN) :: X !ND,N
REAL, DIMENSION(:,:), INTENT(IN) :: G ! ND,ND
REAL, DIMENSION(:), INTENT(IN) :: Z !N
REAL, INTENT(INOUT) :: S2
REAL, INTENT(INOUT) :: P
INTEGER, INTENT(IN) :: LISSAGE
INTEGER, INTENT(IN) :: IOPT
INTEGER, INTENT(IN) :: NSDI
INTEGER, INTENT(IN) :: NSDJ
INTEGER, INTENT(IN) :: INTER
REAL, DIMENSION(:,:), INTENT(IN) :: XD !2,ND
REAL, DIMENSION(:,:,:), INTENT(OUT) :: C !nmax+mmax,nsdi,nsdj
INTEGER, INTENT(OUT) :: IREP
!
INTEGER :: NN, ND
INTEGER :: LORDRE, I, J
INTEGER, DIMENSION(2) :: NSD
REAL, DIMENSION(NMAX) :: DS
REAL, DIMENSION(2,2) :: CI

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2C',0,ZHOOK_HANDLE)
ND=SIZE(X,1)
IF (.NOT.ALLOCATED(AI)) ALLOCATE(AI(ND,2,NSDMAX))
IF (.NOT.ALLOCATED(NS)) ALLOCATE(NS(NSDMAX,NSDMAX))
IF (.NOT.ALLOCATED(ZS)) ALLOCATE(ZS(NMAX,NSDMAX,NSDMAX))
IF (.NOT.ALLOCATED(XS)) ALLOCATE(XS(ND,NMAX,NSDMAX,NSDMAX))

! Initialisation de Ds
DS(:)=1.

! Generation des sous-domaines: matrice AI de MODD_SPLINES
NSD(1)=NSDI
NSD(2)=NSDJ
 CALL SPLBSD(NSD,INTER,XD,AI)

LORDRE=0

! Calcul des splines par sous-domaines
DO I=1,NSDI
  DO J=1,NSDJ
    CI(1,1)=AI(1,1,I)
    CI(1,2)=AI(1,2,I)
    CI(2,1)=AI(2,1,J)
    CI(2,2)=AI(2,2,J)
    !selection des points
    CALL SPLBSEL(X,Z,CI(:,1),CI(:,2),NS(I,J),XS(:,:,I,J),ZS(:,I,J),IREP)
    IF (IREP.NE.0) THEN
      WRITE(*,FMT='(A35)') "SPLB2C: NOMBRE DE POINTS TROP GRAND"
      IREP=-4
      CALL ABOR1_SFX('SPLINES: SPLB2C: NOMBRE DE POINTS TROP GRAND')
    ELSEIF (NS(I,J).LE.M) THEN
      WRITE(*,FMT='(A42)') &
        "SPB2C : NB DE PTS <= M : COEF NON CALCULES"
    ENDIF
    !calcul de la spline
    IF (NS(I,J).GT.M) THEN
       NN=NS(I,J) 
      !X(ND,NN) ZS(NN) C(NN+M) IW(ND,M) W(7*NN*NN)
      CALL SP0CVQ(NORD,M,XS(:,1:NN,I,J),G,ZS(1:NN,I,J),DS(1:NN),S2,P,&
        LISSAGE,LORDRE,IOPT,C(1:NN+M,I,J),IREP)   
    ENDIF 
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_SPLINES:SPLB2C',1,ZHOOK_HANDLE)

END SUBROUTINE SPLB2C


END MODULE MODE_SPLINES
