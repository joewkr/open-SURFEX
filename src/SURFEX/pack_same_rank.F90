!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      MODULE MODI_PACK_SAME_RANK
!     ##########################
INTERFACE PACK_SAME_RANK
      SUBROUTINE PACK_SAME_RANK_FROM1DI(KM,K1D_IN,K1D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:),   INTENT(IN) :: K1D_IN
INTEGER, DIMENSION(:),   INTENT(OUT):: K1D_OUT
END SUBROUTINE PACK_SAME_RANK_FROM1DI
!
      SUBROUTINE PACK_SAME_RANK_FROM2DI(KM,K1D_IN,K1D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:,:),   INTENT(IN) :: K1D_IN
INTEGER, DIMENSION(:,:),   INTENT(OUT):: K1D_OUT
END SUBROUTINE PACK_SAME_RANK_FROM2DI
!
      SUBROUTINE PACK_SAME_RANK_FROM3DI(KM,K3D_IN,K3D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:,:,:), INTENT(IN) :: K3D_IN
INTEGER, DIMENSION(:,:,:), INTENT(OUT):: K3D_OUT
!
END SUBROUTINE PACK_SAME_RANK_FROM3DI
!
      SUBROUTINE PACK_SAME_RANK_FROM1DL(KM,O1D_IN,O1D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
LOGICAL, DIMENSION(:),   INTENT(IN) :: O1D_IN
LOGICAL, DIMENSION(:),   INTENT(OUT):: O1D_OUT
END SUBROUTINE PACK_SAME_RANK_FROM1DL
!
      SUBROUTINE PACK_SAME_RANK_FROM1D(KM,P1D_IN,P1D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:),   INTENT(IN) :: P1D_IN
REAL, DIMENSION(:),   INTENT(OUT):: P1D_OUT
END SUBROUTINE PACK_SAME_RANK_FROM1D
!
      SUBROUTINE PACK_SAME_RANK_FROM2D(KM,P2D_IN,P2D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:), INTENT(IN) :: P2D_IN
REAL, DIMENSION(:,:), INTENT(OUT):: P2D_OUT
!
END SUBROUTINE PACK_SAME_RANK_FROM2D
!
      SUBROUTINE PACK_SAME_RANK_FROM3D(KM,P3D_IN,P3D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:), INTENT(IN) :: P3D_IN
REAL, DIMENSION(:,:,:), INTENT(OUT):: P3D_OUT
!
END SUBROUTINE PACK_SAME_RANK_FROM3D
!
      SUBROUTINE PACK_SAME_RANK_FROM4D(KM,P4D_IN,P4D_OUT)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: P4D_IN
REAL, DIMENSION(:,:,:,:),   INTENT(OUT):: P4D_OUT
!
END SUBROUTINE PACK_SAME_RANK_FROM4D
!
END INTERFACE PACK_SAME_RANK
!
END MODULE MODI_PACK_SAME_RANK
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM1D(KM,P1D_IN,P1D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:),   INTENT(IN) :: P1D_IN
REAL, DIMENSION(:),   INTENT(OUT):: P1D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1D',0,ZHOOK_HANDLE)
!cdir nodep
DO JI=1,SIZE(P1D_OUT,1)
  P1D_OUT(JI) = P1D_IN(KM(JI)) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM1D
!
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM1DI(KM,K1D_IN,K1D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:),   INTENT(IN) :: K1D_IN
INTEGER, DIMENSION(:),   INTENT(OUT):: K1D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1DI',0,ZHOOK_HANDLE)
!cdir nodep
DO JI=1,SIZE(K1D_OUT,1)
  K1D_OUT(JI) = K1D_IN(KM(JI)) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM1DI
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM2DI(KM,K2D_IN,K2D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 2D field into a 2D field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),  INTENT(IN) :: KM
INTEGER, DIMENSION(:,:),   INTENT(IN) :: K2D_IN
INTEGER, DIMENSION(:,:),   INTENT(OUT):: K2D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM2DI',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(K2D_OUT,2)
!cdir nodep
  DO JI=1,SIZE(K2D_OUT,1)
    K2D_OUT(JI,JJ) = K2D_IN(KM(JI),JJ)
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM2DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM2DI
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM3DI(KM,K3D_IN,K3D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 2D field into a 2D field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	F. Habets   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),  INTENT(IN) :: KM
INTEGER, DIMENSION(:,:,:),   INTENT(IN) :: K3D_IN
INTEGER, DIMENSION(:,:,:),   INTENT(OUT):: K3D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ, JK ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM3DI',0,ZHOOK_HANDLE)
DO JK=1,SIZE(K3D_OUT,3)
  DO JJ=1,SIZE(K3D_OUT,2)
    DO JI=1,SIZE(K3D_OUT,1)
      K3D_OUT(JI,JJ,JK) = K3D_IN(KM(JI),JJ,JK)
    ENDDO
  ENDDO 
ENDDO

IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM3DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM3DI
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM1DL(KM,O1D_IN,O1D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),   INTENT(IN) :: KM
LOGICAL, DIMENSION(:),   INTENT(IN) :: O1D_IN
LOGICAL, DIMENSION(:),   INTENT(OUT):: O1D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1DL',0,ZHOOK_HANDLE)
!cdir nodep
DO JI=1,SIZE(O1D_OUT,1)
  O1D_OUT(JI) = O1D_IN(KM(JI)) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM1DL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM1DL
!
!
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM2D(KM,P2D_IN,P2D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 2D field into a 2D field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),  INTENT(IN) :: KM
REAL, DIMENSION(:,:),   INTENT(IN) :: P2D_IN
REAL, DIMENSION(:,:),   INTENT(OUT):: P2D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM2D',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(P2D_OUT,2)
!cdir nodep
  DO JI=1,SIZE(P2D_OUT,1)
    P2D_OUT(JI,JJ) = P2D_IN(KM(JI),JJ)
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM2D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM3D(KM,P3D_IN,P3D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 3D field into a 3D field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:),  INTENT(IN) :: P3D_IN
REAL, DIMENSION(:,:,:),  INTENT(OUT):: P3D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ, JK ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM3D',0,ZHOOK_HANDLE)
DO JK=1,SIZE(P3D_OUT,3)
  DO JJ=1,SIZE(P3D_OUT,2)
!cdir nodep
    DO JI=1,SIZE(P3D_OUT,1)
      P3D_OUT(JI,JJ,JK) = P3D_IN(KM(JI),JJ,JK)
    ENDDO
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM3D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM3D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################
      SUBROUTINE PACK_SAME_RANK_FROM4D(KM,P4D_IN,P4D_OUT)
!     ##############################################
!
!!****  *PACK_SAME_RANK* - extract the defined data from a 4D field into a 4D field
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/03
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: P4D_IN
REAL, DIMENSION(:,:,:,:), INTENT(OUT):: P4D_OUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER :: JI, JJ, JK, JL ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM4D',0,ZHOOK_HANDLE)
DO JL=1,SIZE(P4D_OUT,4)
  DO JK=1,SIZE(P4D_OUT,3)
    DO JJ=1,SIZE(P4D_OUT,2)
!cdir nodep
      DO JI=1,SIZE(P4D_OUT,1)
        P4D_OUT(JI,JJ,JL,JK) = P4D_IN(KM(JI),JJ,JL,JK)
      ENDDO
    ENDDO
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_PACK_SAME_RANK:PACK_SAME_RANK_FROM4D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_SAME_RANK_FROM4D
