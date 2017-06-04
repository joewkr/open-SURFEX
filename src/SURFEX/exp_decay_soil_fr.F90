!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########
      SUBROUTINE EXP_DECAY_SOIL_FR (HISBA, PF, PK, PC_DEPTH_RATIO)  
!     ##########################################################################
!
!!****  *EXP_DECAY_SOIL_FR*  
!!
!!    PURPOSE
!!    -------
!
!     We caculate the hydraulic coductivity decay factor for each FR-coefficients.
!     Also, we redefine the surface hydraulic coductivity at saturation for
!     convective precipitation parametrisation.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/11/03 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_P_t
!
USE MODD_SURF_PAR,ONLY : XUNDEF
USE MODD_SGH_PAR, ONLY : X2                                
USE MODD_CSTS,    ONLY : XDAY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*)                  :: HISBA   ! hydrology/soil:
!                                            ! '2-L'  = single column
!                                            ! '3-L'  = root zone/baseflow layer
!                                            ! 'DIF'  = N-layer diffusion: Richard's Eq.
!
REAL, DIMENSION(:), INTENT(IN)    :: PF
!                                    PF = exponential decay factor (1/m)
!
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
!
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PC_DEPTH_RATIO
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PF))         :: ZD_G_TOT, ZC_DEPTH, ZKSAT_NOEXP, ZC_DEPTH_RATIO
!                                    ZD_G_TOT = depth of the soil column (m)
!                                    ZC_DEPTH = assumed as the depth where the vertical 
!                                               satured hydraulic conductivities reach
!                                               the compacted value given in Clapp and
!                                               Hornberger. (m)
!                                               For ISBA-FR, we take the root depth.
!
INTEGER :: JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_FR',0,ZHOOK_HANDLE)
!
ZD_G_TOT(:) = PK%XDG(:,2)
IF(HISBA=='3-L')ZD_G_TOT(:) = PK%XDG(:,3)
!
ZKSAT_NOEXP(:) = PK%XCONDSAT(:,2)
!
ZC_DEPTH_RATIO(:) = 1.
!
IF (PRESENT(PC_DEPTH_RATIO)) ZC_DEPTH_RATIO(:) = PC_DEPTH_RATIO(:)
!
WHERE(ZD_G_TOT(:)/=XUNDEF)
  !
  !compacted depth
  !
  ZC_DEPTH(:) = PK%XDG(:,2)*ZC_DEPTH_RATIO(:)
  !ZC_DEPTH(:) = PK%XDG(:,2)
  !
  !surface hydraulic conductivity at saturation
  !
  PK%XCONDSAT(:,1) = PK%XCONDSAT(:,1)*EXP(PF(:)*ZC_DEPTH(:))
  !
  !mean hydraulic conductivity at saturation over the root zone
  !   
  PK%XCONDSAT(:,2) = ZKSAT_NOEXP(:)*( EXP(PF(:)*ZC_DEPTH)-EXP(PF(:)*(ZC_DEPTH(:)-PK%XDG(:,2))) )   &
                    /(PF(:)*PK%XDG(:,2))
  !   
  !mean hydraulic conductivity at saturation over the first soil centimeters
  !   
  PK%XKSAT_ICE(:) = ZKSAT_NOEXP(:)*( EXP(PF(:)*ZC_DEPTH)-EXP(PF(:)*(ZC_DEPTH(:)-PK%XD_ICE(:))) )   &
                   /(PF(:)*PK%XD_ICE(:))  
  !
  !decay factor for C1 coef
  !   
  PK%XC1SAT(:) = PK%XC1SAT(:)*SQRT( EXP(-PF(:)*ZC_DEPTH(:)) )
  !
  !decay factor for C2 coef 
  !
  PK%XC2REF(:)=PK%XC2REF(:)+( PK%XCONDSAT(:,2)-ZKSAT_NOEXP(:) ) * XDAY/PK%XDG(:,2) 
  !
  !C3 coef with exponential decay in root soil layer 
  !
  PK%XC3(:,1)=PK%XC3(:,1)*( EXP(PF(:)*ZC_DEPTH(:))-EXP(PF(:)*(ZC_DEPTH(:)-PK%XDG(:,2))) ) / &
          (PF(:)*PK%XDG(:,2))
  !
ENDWHERE
!
IF(HISBA=='3-L')THEN
  ! 
  WHERE(PK%XDG(:,2)< ZD_G_TOT(:).AND.PK%XDG(:,2)/=XUNDEF)
    !           
    !  C3 coef with exponential decay in deep soil layer 
    !
    PK%XC3(:,2)=PK%XC3(:,2)*( EXP(PF(:)*(ZC_DEPTH(:)-PK%XDG(:,2)))-EXP(PF(:)*(ZC_DEPTH(:)-ZD_G_TOT(:))) )      &
                     / (PF(:)*(ZD_G_TOT(:)-PK%XDG(:,2)))  
    ! 
    !  decay factor for C4 coef
    !      
    PK%XC4REF(:)=PK%XC4REF(:)*( EXP(PF(:)*(ZC_DEPTH(:)-PK%XDG(:,2)/X2))-EXP(PF(:)*(ZC_DEPTH(:)&
                         -((PK%XDG(:,2)+ZD_G_TOT(:))/2.))) ) * X2/(PF(:)*ZD_G_TOT(:))        
    !
  ENDWHERE
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_FR',1,ZHOOK_HANDLE)
!
END SUBROUTINE EXP_DECAY_SOIL_FR
