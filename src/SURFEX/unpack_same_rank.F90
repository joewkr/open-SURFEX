!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      MODULE MODI_UNPACK_SAME_RANK
!     ##########################
INTERFACE UNPACK_SAME_RANK
      SUBROUTINE UNPACK_SAME_RANK_FROM1DI(KM,K1D_IN,K1D_OUT,KMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:),   INTENT(IN) :: K1D_IN
INTEGER, DIMENSION(:),   INTENT(OUT):: K1D_OUT
INTEGER, OPTIONAL,       INTENT(IN) :: KMISS
END SUBROUTINE UNPACK_SAME_RANK_FROM1DI
!
      SUBROUTINE UNPACK_SAME_RANK_FROM2DI(KM,K1D_IN,K1D_OUT,KMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:,:), INTENT(IN) :: K1D_IN
INTEGER, DIMENSION(:,:), INTENT(OUT):: K1D_OUT
INTEGER, OPTIONAL,       INTENT(IN) :: KMISS
END SUBROUTINE UNPACK_SAME_RANK_FROM2DI
!
      SUBROUTINE UNPACK_SAME_RANK_FROM1DL(KM,O1D_IN,O1D_OUT,OMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
LOGICAL, DIMENSION(:),   INTENT(IN) :: O1D_IN
LOGICAL, DIMENSION(:),   INTENT(OUT):: O1D_OUT
LOGICAL, OPTIONAL,       INTENT(IN) :: OMISS
END SUBROUTINE UNPACK_SAME_RANK_FROM1DL
!
      SUBROUTINE UNPACK_SAME_RANK_FROM1D(KM,P1D_IN,P1D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:),   INTENT(IN) :: P1D_IN
REAL, DIMENSION(:),   INTENT(OUT):: P1D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
END SUBROUTINE UNPACK_SAME_RANK_FROM1D
!
      SUBROUTINE UNPACK_SAME_RANK_FROM2D(KM,P2D_IN,P2D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:), INTENT(IN) :: P2D_IN
REAL, DIMENSION(:,:), INTENT(OUT):: P2D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
!
END SUBROUTINE UNPACK_SAME_RANK_FROM2D
!
      SUBROUTINE UNPACK_SAME_RANK_FROM3D(KM,P3D_IN,P3D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:), INTENT(IN) :: P3D_IN
REAL, DIMENSION(:,:,:), INTENT(OUT):: P3D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
!
END SUBROUTINE UNPACK_SAME_RANK_FROM3D
!
      SUBROUTINE UNPACK_SAME_RANK_FROM3DI(KM,K3D_IN,K3D_OUT,KMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
INTEGER, DIMENSION(:,:,:), INTENT(IN) :: K3D_IN
INTEGER, DIMENSION(:,:,:), INTENT(OUT):: K3D_OUT
INTEGER, OPTIONAL,       INTENT(IN) :: KMISS
!
END SUBROUTINE UNPACK_SAME_RANK_FROM3DI
!
      SUBROUTINE UNPACK_SAME_RANK_FROM4D(KM,P4D_IN,P4D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: P4D_IN
REAL, DIMENSION(:,:,:,:),   INTENT(OUT):: P4D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
!
END SUBROUTINE UNPACK_SAME_RANK_FROM4D
!
END INTERFACE 
!
END MODULE MODI_UNPACK_SAME_RANK
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM1D(KM,P1D_IN,P1D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
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
!!      B. Decharme 2008  Allows to put other optional value for missval
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
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
REAL, OPTIONAL,       INTENT(IN) :: PMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1D',0,ZHOOK_HANDLE)
IF(PRESENT(PMISS))THEN
  P1D_OUT(:) = PMISS      
ELSE
  P1D_OUT(:) = XUNDEF
ENDIF
!
!cdir nodep
DO JI=1,SIZE(P1D_IN,1)
  P1D_OUT(KM(JI)) = P1D_IN(JI) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM1D
!
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM1DI(KM,K1D_IN,K1D_OUT,KMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
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
USE MODD_SURF_PAR,   ONLY : NUNDEF
!
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
INTEGER, OPTIONAL,       INTENT(IN) :: KMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1DI',0,ZHOOK_HANDLE)
IF(PRESENT(KMISS))THEN
  K1D_OUT(:) = KMISS      
ELSE
  K1D_OUT(:) = NUNDEF
ENDIF
!
!cdir nodep
DO JI=1,SIZE(K1D_IN,1)
  K1D_OUT(KM(JI)) = K1D_IN(JI) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM1DI
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM2DI(KM,K2D_IN,K2D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 2D field into a 2D field
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
USE MODD_SURF_PAR,   ONLY : NUNDEF
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
REAL, OPTIONAL,         INTENT(IN) :: PMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM2DI',0,ZHOOK_HANDLE)
!
IF(PRESENT(PMISS))THEN
  K2D_OUT(:,:) = PMISS      
ELSE
  K2D_OUT(:,:) = NUNDEF
ENDIF
!
DO JJ=1,SIZE(K2D_IN,2)
!cdir nodep
  DO JI=1,SIZE(K2D_IN,1)
    K2D_OUT(KM(JI),JJ) = K2D_IN(JI,JJ)
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM2DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM2DI
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM1DL(KM,O1D_IN,O1D_OUT,OMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 1D field into a 1D field of lower rank
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
!
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
LOGICAL, OPTIONAL,       INTENT(IN) :: OMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1DL',0,ZHOOK_HANDLE)
IF(PRESENT(OMISS))THEN
  O1D_OUT(:) = OMISS      
ELSE
  O1D_OUT(:) = .FALSE.
ENDIF
!
!cdir nodep
DO JI=1,SIZE(O1D_IN,1)
  O1D_OUT(KM(JI)) = O1D_IN(JI) 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM1DL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM1DL
!
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM2D(KM,P2D_IN,P2D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 2D field into a 2D field
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
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
REAL, OPTIONAL,         INTENT(IN) :: PMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM2D',0,ZHOOK_HANDLE)
IF(PRESENT(PMISS))THEN
  P2D_OUT(:,:) = PMISS      
ELSE
  P2D_OUT(:,:) = XUNDEF
ENDIF
!
DO JJ=1,SIZE(P2D_IN,2)
!cdir nodep
  DO JI=1,SIZE(P2D_IN,1)
    P2D_OUT(KM(JI),JJ) = P2D_IN(JI,JJ)
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM2D
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM3D(KM,P3D_IN,P3D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 3D field into a 3D field
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
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
REAL, OPTIONAL,          INTENT(IN) :: PMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ, JK ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM3D',0,ZHOOK_HANDLE)
IF(PRESENT(PMISS))THEN
  P3D_OUT(:,:,:) = PMISS      
ELSE
  P3D_OUT(:,:,:) = XUNDEF
ENDIF
!
DO JK=1,SIZE(P3D_IN,3)
  DO JJ=1,SIZE(P3D_IN,2)
!cdir nodep
    DO JI=1,SIZE(P3D_IN,1)
      P3D_OUT(KM(JI),JJ,JK) = P3D_IN(JI,JJ,JK)
    ENDDO
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM3D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM3D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM3DI(KM,K3D_IN,K3D_OUT,KMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 3D field into a 3D field
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
USE MODD_SURF_PAR,   ONLY : NUNDEF
!
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
REAL, DIMENSION(:,:,:),  INTENT(IN) :: K3D_IN
REAL, DIMENSION(:,:,:),  INTENT(OUT):: K3D_OUT
REAL, OPTIONAL,          INTENT(IN) :: KMISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JI, JJ, JK ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM3D',0,ZHOOK_HANDLE)
IF(PRESENT(KMISS))THEN
  K3D_OUT(:,:,:) = KMISS      
ELSE
  K3D_OUT(:,:,:) = NUNDEF
ENDIF
!
DO JK=1,SIZE(K3D_IN,3)
  DO JJ=1,SIZE(K3D_IN,2)
!cdir nodep
    DO JI=1,SIZE(K3D_IN,1)
      K3D_OUT(KM(JI),JJ,JK) = K3D_IN(JI,JJ,JK)
    ENDDO
  ENDDO 
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM3DI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM3DI
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK_FROM4D(KM,P4D_IN,P4D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK* - extract the defined data from a 4D field into a 4D field
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, DIMENSION(:),    INTENT(IN) :: KM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: P4D_IN
REAL, DIMENSION(:,:,:,:), INTENT(OUT):: P4D_OUT
REAL, OPTIONAL,           INTENT(IN) :: PMISS
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
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM4D',0,ZHOOK_HANDLE)
IF(PRESENT(PMISS))THEN
  P4D_OUT(:,:,:,:) = PMISS      
ELSE
  P4D_OUT(:,:,:,:) = XUNDEF
ENDIF
!
DO JL=1,SIZE(P4D_OUT,4)
  DO JK=1,SIZE(P4D_OUT,3)
    DO JJ=1,SIZE(P4D_OUT,2)
!cdir nodep
      DO JI=1,SIZE(P4D_OUT,1)
        P4D_OUT(KM(JI),JJ,JK,JL) = P4D_IN(JI,JJ,JK,JL)
      ENDDO
    ENDDO
  ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK:UNPACK_SAME_RANK_FROM4D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK_FROM4D
