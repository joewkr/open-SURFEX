!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_TEB_VEG (P, PEK, IO, PZS)
!     #################################################################################
!
!!****  *PREP_VER_TEB_GARDEN* - change in ISBA fields due to altitude change
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified by B. Decharme  (01/2009), Optional Arpege deep soil temperature initialization
!!------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_P_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP,           ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_CSTS,           ONLY : XTT, XDAY, XLMTT, XRHOLW
!
USE MODE_THERMOS
USE MODI_PREP_VER_SNOW
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
!*      0.2    declarations of local variables
!
!
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
REAL, DIMENSION(:), INTENT(IN) :: PZS
!
INTEGER                         :: JL        ! loop counter on layers
INTEGER                         :: IWORK     ! Work integer
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWGTOT    ! total water content
REAL, DIMENSION(:), ALLOCATABLE :: ZDW       ! variation of water in soil
REAL, DIMENSION(:), ALLOCATABLE :: ZZSFREEZE ! altitude where soil temperature equals XTT
INTEGER                         :: IDEEP_SOIL! layer corresponding to deep soil temperature
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWGI_CLIM_GRAD ! ice content vertical gradient
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTG_LS! temperature on initial orography
!
REAL                            :: ZGRADX = 5.E-4 ! slope of ice content gradient
REAL                            :: ZH0    = 5.E-1 ! constant used to define ice content gradient
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.0    Ice content climatologic gradient
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB_VEG',0,ZHOOK_HANDLE)
ALLOCATE(ZWGI_CLIM_GRAD (SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)))
!
ZWGI_CLIM_GRAD(:,:) = ZGRADX * EXP( - P%XDG(:,:) / ZH0 )
!-------------------------------------------------------------------------------------
!
!*      1.1    Temperature profile
!
ALLOCATE(ZTG_LS(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)))
ZTG_LS(:,:) = PEK%XTG(:,:)
!
DO JL=1,SIZE(PEK%XTG,2)
  WHERE(PEK%XTG(:,JL)/=XUNDEF) &
    PEK%XTG(:,JL) = PEK%XTG(:,JL) + XT_CLIM_GRAD  * (PZS - XZS_LS)  
END DO
!
!-------------------------------------------------------------------------------------
!
!*      1.2    Water and ice in the soil
!
ALLOCATE(ZZSFREEZE      (SIZE(PEK%XWG,1)))
ALLOCATE(ZWGTOT         (SIZE(PEK%XWG,1)))
ALLOCATE(ZDW            (SIZE(PEK%XWG,1)))
!
!* general case
!
IWORK=SIZE(PEK%XTG,2)
!
DO JL=1,IWORK
  !
  ZDW(:) = 0.
  ! altitude where deep soil freezes (diurnal surface response is not treated)
  ZZSFREEZE(:) = PZS + (XTT - PEK%XTG(:,JL)) / XT_CLIM_GRAD
  !
  WHERE(PEK%XTG(:,JL)/=XUNDEF) 
    !
    WHERE (ZTG_LS(:,JL) < XTT)
      !
      WHERE (PZS <= XZS_LS)
        !
        WHERE (PZS > ZZSFREEZE) 
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (PZS - XZS_LS)
        ELSEWHERE
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZSFREEZE - XZS_LS) + ZGRADX * (PZS - ZZSFREEZE)
        ENDWHERE
        !
      ELSEWHERE
        !
        ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (PZS - XZS_LS)
        !
      ENDWHERE
      !
    ELSEWHERE
      !
      WHERE (PZS <= XZS_LS)
        !
        ZDW(:) = ZGRADX * (PZS - XZS_LS)
        !
      ELSEWHERE
        !
        ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (PZS - ZZSFREEZE)
        !
      END WHERE
      !
    END WHERE
    !
    ZWGTOT(:) = XUNDEF
    !
    WHERE(PEK%XWG(:,JL)/=XUNDEF)         
      ZWGTOT(:) = PEK%XWG(:,JL) + PEK%XWGI(:,JL)
    ENDWHERE        
    !
    WHERE(PEK%XWG(:,JL)/=XUNDEF)      
      PEK%XWGI(:,JL) = PEK%XWGI(:,JL) + ZDW(:)
      PEK%XWG (:,JL) = PEK%XWG (:,JL) - ZDW(:)
    ENDWHERE
    !
    WHERE (PEK%XWGI(:,JL) < 0..AND.PEK%XWGI(:,JL)/=XUNDEF) 
      PEK%XWGI(:,JL) = 0.
      PEK%XWG (:,JL) = ZWGTOT(:)
    END WHERE
    !
    WHERE (PEK%XWG(:,JL) < XWGMIN.AND.PEK%XWG(:,JL)/=XUNDEF)
      PEK%XWG (:,JL) = XWGMIN
      PEK%XWGI(:,JL) = ZWGTOT(:) - XWGMIN
    END WHERE
    !
    WHERE(PEK%XWGI(:,JL) > 0..AND.PEK%XWGI(:,JL)/=XUNDEF)
      PEK%XTG(:,JL) = MIN(XTT,PEK%XTG(:,JL))
    ELSEWHERE
      PEK%XTG(:,JL) = MAX(XTT,PEK%XTG(:,JL))
    ENDWHERE
    !
  ENDWHERE
  !
END DO
!
!* limits in force-restore case
!
IF (IO%CISBA=='3-L') THEN 
  WHERE (PEK%XWGI(:,3) /= XUNDEF)
    PEK%XWG (:,3) = PEK%XWG(:,3)+PEK%XWGI(:,3)
    PEK%XWGI(:,3) = 0.
    PEK%XTG (:,3) = ZTG_LS(:,3)
  END WHERE
END IF
!
DEALLOCATE(ZZSFREEZE)
DEALLOCATE(ZWGI_CLIM_GRAD)
DEALLOCATE(ZWGTOT   )
DEALLOCATE(ZDW      )
!
!* masks where fields are not defined
WHERE (PEK%XTG(:,1:SIZE(PEK%XWG,2)) == XUNDEF)
  PEK%XWG (:,:) = XUNDEF
  PEK%XWGI(:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      1.4    Snow variables
!
!* vertical shift
IF (IO%CISBA=='DIF') THEN
  IDEEP_SOIL = IO%NGROUND_LAYER
ELSE
  IDEEP_SOIL = 2
END IF
 CALL PREP_VER_SNOW(PEK%TSNOW,XZS_LS,PZS,ZTG_LS,PEK%XTG,IDEEP_SOIL)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Deallocation of large-scale orography
!
DEALLOCATE(ZTG_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB_VEG',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_TEB_VEG
