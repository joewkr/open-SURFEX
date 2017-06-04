!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_ISBA (IO, NPE, PZS, NP)
!     #################################################################################
!
!!****  *PREP_VER_ISBA* - change in ISBA fields due to altitude change
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
!!      S. Riette   04/2010 Modification of XTG corrections after freezing
!!      Y. Seity    02/2016 Add limits in Force-Restore case (WG2 contains WG1)
!!------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP,           ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_PREP_ISBA,      ONLY : LSNOW_IDEAL
USE MODD_CSTS,           ONLY : XTT, XDAY, XLMTT, XRHOLW
!
USE MODE_THERMOS
USE MODI_PREP_VER_SNOW
USE MODI_PACK_SAME_RANK
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
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
REAL, DIMENSION(:), INTENT(IN) :: PZS
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
INTEGER                         :: JL        ! loop counter on layers
INTEGER                         :: JP        ! loop counter on patches
INTEGER                         :: IWORK     ! Work integer
!
REAL, DIMENSION(:), ALLOCATABLE :: ZZS, ZZS_LS
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
IF (LHOOK) CALL DR_HOOK('PREP_VER_ISBA',0,ZHOOK_HANDLE)
!
!
IF (IO%CISBA=='DIF') THEN
  IDEEP_SOIL = IO%NGROUND_LAYER
ELSE
  IDEEP_SOIL = 2
END IF
!
DO JP = 1,IO%NPATCH
  !
  PEK => NPE%AL(JP)
  PK => NP%AL(JP)
  !
  IF(IO%LTEMP_ARP)THEN
    IWORK=SIZE(PEK%XWG,2)
  ELSE
    IWORK=SIZE(PEK%XTG,2)
  ENDIF
  !
  ALLOCATE(ZZS(PK%NSIZE_P))
  CALL PACK_SAME_RANK(PK%NR_P,PZS,ZZS)
  ALLOCATE(ZZS_LS(PK%NSIZE_P))
  CALL PACK_SAME_RANK(PK%NR_P,XZS_LS,ZZS_LS)
  !
  ALLOCATE(ZWGI_CLIM_GRAD (SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)))
  !
  ZWGI_CLIM_GRAD(:,:) = ZGRADX * EXP( - PK%XDG(:,:) / ZH0 )
  !-------------------------------------------------------------------------------------
  !
  !*      1.1    Temperature profile
  !
  ALLOCATE(ZTG_LS(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)))
  ZTG_LS(:,:) = PEK%XTG(:,:)
  !
  DO JL=1,SIZE(PEK%XTG,2)
    WHERE(PEK%XTG(:,JL)/=XUNDEF) &
      PEK%XTG(:,JL) = PEK%XTG(:,JL) + XT_CLIM_GRAD  * (ZZS - ZZS_LS)  
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
  DO JL=1,IWORK
    !
    ZDW(:) = 0.
    ! altitude where deep soil freezes (diurnal surface response is not treated)
    ZZSFREEZE(:) = ZZS + (XTT - PEK%XTG(:,JL)) / XT_CLIM_GRAD
    !
    WHERE(PEK%XTG(:,JL)/=XUNDEF) 
      !
      WHERE (ZTG_LS(:,JL) < XTT)
        !
        WHERE (ZZS <= ZZS_LS)
          !
          WHERE (ZZS > ZZSFREEZE) 
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZS - ZZS_LS)
          ELSEWHERE
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZSFREEZE - ZZS_LS) + ZGRADX * (ZZS - ZZSFREEZE)
          ENDWHERE
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZS - ZZS_LS)
          !
        ENDWHERE
        !
      ELSEWHERE
        !
        WHERE (ZZS <= ZZS_LS)
          !
          ZDW(:) = ZGRADX * (ZZS - ZZS_LS)
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZS - ZZSFREEZE)
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
      WHERE (PEK%XWGI(:,JL)<0.0.AND.PEK%XWGI(:,JL)/=XUNDEF) 
        PEK%XWGI(:,JL) = 0.
        PEK%XWG (:,JL) = ZWGTOT(:)
      END WHERE
      !
      WHERE (PEK%XWG(:,JL)<XWGMIN.AND.PEK%XWG(:,JL)/=XUNDEF)
        PEK%XWG (:,JL) = XWGMIN
        PEK%XWGI(:,JL) = ZWGTOT(:) - XWGMIN
      END WHERE
      !
      WHERE(PEK%XWGI(:,JL)>0.0.AND.PEK%XWGI(:,JL)/=XUNDEF)
        PEK%XTG(:,JL) = MIN(XTT,PEK%XTG(:,JL))
      ELSEWHERE
        PEK%XTG(:,JL) = MAX(XTT,PEK%XTG(:,JL))
      ENDWHERE
      !
    END WHERE
    !
  END DO
  !
  !
  !* limits in force-restore case
  !
  IF (IO%CISBA=='2-L'.OR.IO%CISBA=='3-L') THEN
    PEK%XWG (:,2) = MAX(PEK%XWG (:,1)*PK%XDG(:,1),PEK%XWG (:,2)*PK%XDG(:,2))/PK%XDG(:,2)
    PEK%XWGI(:,2) = MAX(PEK%XWGI(:,1)*PK%XDG(:,1),PEK%XWGI(:,2)*PK%XDG(:,2))/PK%XDG(:,2)
  ENDIF
  !  
  IF (IO%CISBA=='3-L') THEN 
    !
    WHERE (PEK%XWGI(:,3) /= XUNDEF)
      PEK%XWG (:,3) = PEK%XWG(:,3)+PEK%XWGI(:,3)
      PEK%XWGI(:,3) = 0.
      PEK%XTG (:,3) = ZTG_LS(:,3) + XT_CLIM_GRAD  * (ZZS - ZZS_LS)       
    END WHERE
    IF(IO%LTEMP_ARP)THEN
       PEK%XTG (:,4:SIZE(PEK%XTG,2)) = ZTG_LS(:,4:SIZE(PEK%XTG,2))
    ENDIF
    !
  ELSEIF(IO%CISBA=='2-L'.AND.IO%LTEMP_ARP) THEN
    !
    PEK%XTG (:,3:SIZE(PEK%XTG,2)) = ZTG_LS(:,3:SIZE(PEK%XTG,2))
    !
  ENDIF
  !
  !* masks where fields are not defined
  WHERE (PEK%XTG(:,1:SIZE(PEK%XWG,2)) == XUNDEF)
    PEK%XWG (:,:) = XUNDEF
    PEK%XWGI(:,:) = XUNDEF
  END WHERE
  !
  IF (.NOT.LSNOW_IDEAL) THEN
    CALL PREP_VER_SNOW(PEK%TSNOW,ZZS_LS,ZZS,ZTG_LS,PEK%XTG,IDEEP_SOIL)
  ENDIF

  DEALLOCATE(ZZSFREEZE)
  DEALLOCATE(ZWGI_CLIM_GRAD)
  DEALLOCATE(ZWGTOT   )
  DEALLOCATE(ZDW      )
  DEALLOCATE(ZTG_LS, ZZS, ZZS_LS)
  !
END DO
!
!
!-------------------------------------------------------------------------------------
!
!*      2.     Deallocation of large-scale orography
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_ISBA
