!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      MODULE MODI_UNPACK_SAME_RANK2
!     ##########################
INTERFACE UNPACK_SAME_RANK2
!
      SUBROUTINE UNPACK_SAME_RANK2_FROM1D(KM,P1D_IN,P1D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:),   INTENT(IN) :: P1D_IN
REAL(KIND=4), DIMENSION(:),   INTENT(OUT):: P1D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
END SUBROUTINE UNPACK_SAME_RANK2_FROM1D
!
      SUBROUTINE UNPACK_SAME_RANK2_FROM2D(KM,P2D_IN,P2D_OUT,PMISS)

INTEGER, DIMENSION(:),   INTENT(IN) :: KM
REAL, DIMENSION(:,:), INTENT(IN) :: P2D_IN
REAL(KIND=4), DIMENSION(:,:), INTENT(OUT):: P2D_OUT
REAL, OPTIONAL,       INTENT(IN) :: PMISS
!
END SUBROUTINE UNPACK_SAME_RANK2_FROM2D
!
END INTERFACE 
!
END MODULE MODI_UNPACK_SAME_RANK2
!
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK2_FROM1D(KM,P1D_IN,P1D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK2* - extract the defined data from a 1D field into a 1D field of lower rank
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
REAL(KIND=4), DIMENSION(:),   INTENT(OUT):: P1D_OUT
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
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK2:UNPACK_SAME_RANK2_FROM1D',0,ZHOOK_HANDLE)
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
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK2:UNPACK_SAME_RANK2_FROM1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK2_FROM1D
!
!     ##############################################
      SUBROUTINE UNPACK_SAME_RANK2_FROM2D(KM,P2D_IN,P2D_OUT,PMISS)
!     ##############################################
!
!!****  *UNPACK_SAME_RANK2* - extract the defined data from a 2D field into a 2D field
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
REAL(KIND=4), DIMENSION(:,:),   INTENT(OUT):: P2D_OUT
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
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK2:UNPACK_SAME_RANK2_FROM2D',0,ZHOOK_HANDLE)
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
IF (LHOOK) CALL DR_HOOK('MODI_UNPACK_SAME_RANK2:UNPACK_SAME_RANK2_FROM2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_SAME_RANK2_FROM2D
!
