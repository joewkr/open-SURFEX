!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!#############################################################
MODULE MODI_DIF_LAYER
CONTAINS
SUBROUTINE DIF_LAYER(KLU, IO, PK  )
!#############################################################
!
!!****  *DIF_LAYER_n* - routine to initialize dif numbers of layers
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
!!    S. Faroux
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2012!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SGH_PAR,        ONLY : XHORT_DEPTH
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLU
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KLU) :: ZWORK
INTEGER, DIMENSION(KLU) :: IWORK
INTEGER :: JL, JI, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('DIF_LAYER',0,ZHOOK_HANDLE)
!
DO JL = 1, IO%NGROUND_LAYER
  IF (ANY((PK%XROOTFRAC(:,JL)<0. .OR. PK%XROOTFRAC(:,JL)>1.) .AND. PK%XPATCH(:).NE.0.)) &
    CALL ABOR1_SFX('DIF_LAYER: WITH CISBA=DIF ROOTFRAC MUST BE DEFINED')
ENDDO
!
PK%XDZG     (:,:) = XUNDEF
PK%XDZDIF   (:,:) = XUNDEF
PK%XSOILWGHT(:,:) = 0.0
!
!*   soil layers thicknesses
PK%XDZG(:,1) = PK%XDG(:,1)
DO JL=2,IO%NGROUND_LAYER
  DO JI=1,KLU
    PK%XDZG(JI,JL) = PK%XDG(JI,JL) - PK%XDG(JI,JL-1)
  ENDDO
ENDDO
!
!*   distance between consecuative layer mid-points
DO JL=1,IO%NGROUND_LAYER
  DO JI=1,KLU
    IF(JL<IO%NGROUND_LAYER)THEN
      PK%XDZDIF(JI,JL)=0.5*(PK%XDZG(JI,JL)+PK%XDZG(JI,JL+1))
    ELSE
      PK%XDZDIF(JI,JL)=0.5*PK%XDZG(JI,JL)
    ENDIF
  ENDDO
ENDDO
!
!
! Horton runoff parameter
!
IWORK(:) = PK%NWG_LAYER(:)
!
DO JI=1,KLU
  IDEPTH = PK%NWG_LAYER(JI)
  IF (IDEPTH==NUNDEF) IDEPTH = IO%NGROUND_LAYER
  DO JL=1,IDEPTH-1
    IF(PK%XDG(JI,JL)<XHORT_DEPTH) IWORK(JI)=JL+1
  ENDDO
ENDDO
!
!
IF (SIZE(IWORK)>0.AND.MAXVAL(IWORK(:),IWORK(:)/=NUNDEF)>IO%NLAYER_HORT) THEN
  IO%NLAYER_HORT=MAXVAL(IWORK(:),IWORK(:)/=NUNDEF)
ENDIF
!
! Dunne runoff parameter
!
IWORK(:)=PK%NWG_LAYER(:)
!
!
DO JI=1,KLU
  IF(PK%XPATCH(JI)>0.0)THEN
    IDEPTH = PK%NWG_LAYER(JI)
    IF(PK%XDROOT(JI)>0.0.AND.PK%XDROOT(JI)/=XUNDEF)THEN
      PK%XRUNOFFD(JI) = PK%XDG(JI,1)
      DO JL=1,IDEPTH-1
        IF(PK%XROOTFRAC(JI,JL)<0.90)THEN
          PK%XRUNOFFD(JI) = PK%XDG(JI,JL+1)
        ENDIF
      ENDDO
    ELSE
      PK%XRUNOFFD(JI) = MIN(0.6,PK%XDG2(JI))
    ENDIF
  ENDIF
ENDDO
!
ZWORK(:) = 0.0
DO JL=1,IO%NGROUND_LAYER
  DO JI=1,KLU
    IF(PK%XPATCH(JI)>0.0)THEN
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZWORK    (JI      ) = ZWORK(JI) + PK%XDZG(JI,JL)
        PK%XSOILWGHT(JI,JL) = MIN(PK%XDZG(JI,JL), &
                                        MAX(0.0,PK%XRUNOFFD(JI)-ZWORK(JI)+PK%XDZG(JI,JL)))
      ENDIF
      IF(PK%XDG(JI,JL)<PK%XRUNOFFD(JI))THEN
        IWORK(JI)=JL+1
      ENDIF
    ENDIF
  ENDDO
ENDDO
!
!
IF (SIZE(IWORK)>0.AND.MAXVAL(IWORK(:),IWORK(:)/=NUNDEF)>IO%NLAYER_DUN) THEN
  IO%NLAYER_DUN=MAXVAL(IWORK(:),IWORK(:)/=NUNDEF)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIF_LAYER',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIF_LAYER
END MODULE MODI_DIF_LAYER
