!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_SNOW(TPSNOW,PZS_LS,PZS,PTG_LS,PTG,KDEEP_SOIL)
!          ###########################################
!
!
!!****  *PREP_VER_SNOW* - change in snow variables due to altitude change
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
!!       02/2014    E. Martin, vertical correction applied to snow cover and not by layers
!!       09/2014    B. Decharme, Bug : Coherence between snow layer and depth
!!------------------------------------------------------------------
!

USE MODD_TYPE_SNOW
USE MODD_CSTS,       ONLY : XTT
USE MODD_PREP,       ONLY : XT_CLIM_GRAD
USE MODD_PREP_SNOW,  ONLY : XWSNOW_CLIM_GRAD
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_MKFLAG_SNOW
!
USE MODE_SNOW3L
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW ! snow mantel characteristics
REAL, DIMENSION(:), INTENT(IN) :: PZS_LS ! initial orography
REAL, DIMENSION(:), INTENT(IN) :: PZS    ! final   orography
REAL, DIMENSION(:,:),INTENT(IN),OPTIONAL:: PTG_LS ! soil temperature on initial orography
REAL, DIMENSION(:,:),INTENT(IN),OPTIONAL:: PTG    ! soil temperature on final   orography
INTEGER,               INTENT(IN),OPTIONAL:: KDEEP_SOIL ! index of deep soil temperature
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWSNOW_LS ! snow reservoir   on initial orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTSNOW_LS ! snow temperature on initial orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWSNOW    ! snow content     on final   orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTSNOW    ! snow temperature on final   orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWSNOW2   ! snow content     on final   orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTSNOW2   ! snow temperature on final   orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWLIQ     ! snow liquid water content
REAL, DIMENSION(:),   ALLOCATABLE :: ZZSFREEZE ! altitude where deep soil freezes
REAL, DIMENSION(:),   ALLOCATABLE :: ZDTOT     ! snow depth
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDZSN     ! snow layer thickness
!
INTEGER                             :: JL    ! loop counter on snow layers
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_SNOW',0,ZHOOK_HANDLE)
!
!*       1.    Snow reservoir on initial orography
!              -----------------------------------
!
ALLOCATE(ZWSNOW_LS(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
ZWSNOW_LS(:,:) =  TPSNOW%WSNOW(:,:)
!
!-------------------------------------------------------------------------------------
!
!*       2.    temperature of snow on initial orography
!              ----------------------------------------
!
ALLOCATE(ZTSNOW_LS(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
SELECT CASE(TPSNOW%SCHEME)
  CASE ('D95','EBA')
    IF (PRESENT(PTG_LS)) THEN
      ZTSNOW_LS(:,1) =  MIN(PTG_LS(:,1),XTT)
    ELSE
      ZTSNOW_LS = XUNDEF
    END IF
  CASE ('1-L')
    ZTSNOW_LS(:,:) =  TPSNOW%T(:,:)
  CASE ('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT(:,:),TPSNOW%RHO(:,:),ZTSNOW_LS(:,:))
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*       3.    vertical shift of temperature
!              -----------------------------
!
ALLOCATE(ZTSNOW(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
DO JL=1,TPSNOW%NLAYER
  ZTSNOW(:,JL) = ZTSNOW_LS(:,JL) + XT_CLIM_GRAD  * (PZS(:) - PZS_LS(:))  
END DO
!
!-------------------------------------------------------------------------------------
!
!*       4.    vertical shift of snow content where snow already exists
!              ------------------------------
!
!* use of climatological snow content gradient
!
ALLOCATE(ZWSNOW(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
!
ZWSNOW(:,:) = ZWSNOW_LS(:,:) 
!
IF (PRESENT(PTG)) THEN
  DO JL=1,TPSNOW%NLAYER
    WHERE(ZWSNOW_LS(:,JL)>0..AND.((PTG(:,KDEEP_SOIL)-XTT >= 2.).OR.(PZS(:) > PZS_LS(:))))
      ZWSNOW(:,JL) = ZWSNOW_LS(:,JL) + ( XWSNOW_CLIM_GRAD  * (PZS(:) - PZS_LS(:))/TPSNOW%NLAYER)
      ZWSNOW(:,JL) = MAX(ZWSNOW(:,JL),0.)
    END WHERE
  END DO
ELSE
  DO JL=1,TPSNOW%NLAYER
    WHERE(ZWSNOW_LS(:,JL)>0.)
      ZWSNOW(:,JL) = ZWSNOW_LS(:,JL) + ( XWSNOW_CLIM_GRAD  * (PZS(:) - PZS_LS(:))/TPSNOW%NLAYER)
      ZWSNOW(:,JL) = MAX(ZWSNOW(:,JL),0.)
    END WHERE
  END DO
ENDIF
!
WHERE(TPSNOW%WSNOW(:,:)/=XUNDEF) TPSNOW%WSNOW = ZWSNOW
!
!-------------------------------------------------------------------------------------
!
!        5.    Where snow did not exist on initial orography
!              ---------------------------------------------
!
!* in this case, new snow can appear only if orography differences in larger
!  than 1000m, and starts at an altitude where the deep soil temperature becomes negative
!
!* the same climatological gradient is used, but the value zero for the snow
!  content is defined as the altitude where deep soil freezes.
!
!*       5.1   Altitude where deep soil freezes (only if soil temperatures are provided)
!              --------------------------------
!
IF (PRESENT(PTG)) THEN

  ALLOCATE(ZZSFREEZE(SIZE(TPSNOW%WSNOW,1)))
  ZZSFREEZE(:) = PZS + (XTT - PTG(:,KDEEP_SOIL)) / XT_CLIM_GRAD  
!
!*       5.2   Amount and Temperature of new snow (only if soil temperatures are provided)
!              ----------------------------------
!
!* Snow temperature is then defined as the deep soil temperature at the final
!  altitude.
!
  ALLOCATE(ZWSNOW2(SIZE(TPSNOW%WSNOW,1),TPSNOW%NLAYER))
  ALLOCATE(ZTSNOW2(SIZE(TPSNOW%WSNOW,1),TPSNOW%NLAYER))
  DO JL=1,TPSNOW%NLAYER
    ZWSNOW2(:,JL) =  XWSNOW_CLIM_GRAD * (PZS(:) - ZZSFREEZE(:))/TPSNOW%NLAYER
    ZWSNOW2(:,JL) = MAX(ZWSNOW2(:,JL),0.)
    ZTSNOW2(:,JL) = PTG(:,KDEEP_SOIL)
  END DO
!
!*       5.3   Apply maximum between this value and the shifted one
!              ----------------------------------------------------
!
  DO JL=1,TPSNOW%NLAYER
    WHERE(TPSNOW%WSNOW(:,JL)/=XUNDEF .AND. ZWSNOW_LS(:,JL)==0. .AND. (PZS(:)-PZS_LS(:))>1000. )  
      TPSNOW%WSNOW(:,JL) = ZWSNOW2(:,JL)
      ZTSNOW      (:,JL) = ZTSNOW2(:,JL)
    END WHERE
  END DO

  DEALLOCATE(ZZSFREEZE)
  DEALLOCATE(ZWSNOW2  )
  DEALLOCATE(ZTSNOW2  )
END IF
!
!-------------------------------------------------------------------------------------
!
!*       6.1   Coherence between temperature and snow content
!              ----------------------------------------------
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    !* snow temperature cannot be larger than 0 C
    TPSNOW%T (:,:) = MIN ( ZTSNOW(:,:), XTT )
  CASE('3-L','CRO')
    ALLOCATE(ZWLIQ(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW)
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW,ZWLIQ)
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW,ZWLIQ)
    DEALLOCATE(ZWLIQ)
END SELECT
!
!*       6.2   Coherence between snow layer and depth
!              --------------------------------------
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('3-L','CRO')
    ALLOCATE(ZDTOT(SIZE(TPSNOW%WSNOW,1)))
    ALLOCATE(ZDZSN(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2)))
    ZDTOT(:)=0.0
    DO JL=1,TPSNOW%NLAYER
       WHERE(TPSNOW%WSNOW(:,JL)/=XUNDEF.AND.TPSNOW%RHO(:,JL)/=XUNDEF)
         ZDTOT(:)=ZDTOT(:)+TPSNOW%WSNOW(:,JL)/TPSNOW%RHO(:,JL)
       ENDWHERE
    END DO
    CALL SNOW3LGRID(ZDZSN(:,:),ZDTOT(:))
    DO JL=1,TPSNOW%NLAYER
      WHERE(TPSNOW%RHO(:,JL)/=XUNDEF.AND.ZDTOT(:)>0.)
        TPSNOW%WSNOW(:,JL) = TPSNOW%RHO(:,JL) * ZDZSN(:,JL)
      ELSEWHERE(TPSNOW%RHO(:,JL)==XUNDEF.OR.ZDTOT(:)==0.0)
        TPSNOW%WSNOW(:,JL) = 0.0
      ELSEWHERE
        TPSNOW%WSNOW(:,JL) = XUNDEF
      END WHERE
    END DO   
    DEALLOCATE(ZDTOT)
    DEALLOCATE(ZDZSN)
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*       7.    Masking where there is no snow
!              ------------------------------
!
 CALL MKFLAG_SNOW(TPSNOW)
!
!-------------------------------------------------------------------------------------
DEALLOCATE(ZWSNOW_LS)
DEALLOCATE(ZTSNOW_LS)
DEALLOCATE(ZWSNOW   )
DEALLOCATE(ZTSNOW   )
IF (LHOOK) CALL DR_HOOK('PREP_VER_SNOW',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_SNOW
