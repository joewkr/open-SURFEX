!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########################################
      SUBROUTINE GET_SERIES_n (F, &
                               HPROGRAM,KI,KS,PFIELD)
!     ########################################
!
!!****  *GET_SERIES_n* - routine to get some surface fields
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODI_GET_LUOUT
USE MODI_UNPACK_SAME_RANK
USE MODD_SURF_PAR,        ONLY   : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_1D_MASK
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
 CHARACTER(LEN=6),       INTENT(IN)     :: HPROGRAM
INTEGER,                INTENT(IN)     :: KI        ! Number of points
INTEGER,                INTENT(IN)     :: KS        ! Number of points
REAL, DIMENSION(KI,KS), INTENT(OUT)    :: PFIELD    ! output field
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
INTEGER :: IS
REAL, DIMENSION(SIZE(F%XTS),KS) :: ZINF
INTEGER, DIMENSION(KI)        :: IMASK
REAL, DIMENSION(KI)           :: ZOUT
REAL, DIMENSION(SIZE(F%XTS))    :: ZAUX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SERIES_N',0,ZHOOK_HANDLE)
!
ZINF(:,1)=F%XTS
ZINF(:,2)=F%XT_MNW
ZINF(:,3)=F%XT_BOT
ZINF(:,4)=F%XCT
ZINF(:,5)=F%XH_ML
IF (KS>5) ZINF(:,6:KS)=XUNDEF

DO IS=1,KS
   ZAUX(:)=ZINF(:,IS)
   CALL GET_1D_MASK(SIZE(ZAUX),KI,ZAUX,IMASK)
   CALL UNPACK_SAME_RANK(IMASK,ZAUX(:),ZOUT(:))
   PFIELD(KI,IS)=ZOUT(KI)
ENDDO
!
!           
IF (LHOOK) CALL DR_HOOK('GET_SERIES_N',1,ZHOOK_HANDLE)
!==============================================================================
!
END SUBROUTINE GET_SERIES_n
