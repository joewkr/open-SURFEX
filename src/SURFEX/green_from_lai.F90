!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_GREEN_FROM_LAI
!#######################
!
INTERFACE GREEN_FROM_LAI
!
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_0D
!
!
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_1D
!
!
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_2D
!

    FUNCTION GREEN_FROM_LAI_VEGTYPE_1D(PLAI,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index for each vegtype
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PGREEN ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_VEGTYPE_1D
!
END INTERFACE
!
END MODULE MODI_GREEN_FROM_LAI
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!!      R. Alkama    05/2012  : Add 7 new vegtype (19 rather than 12)
!!      B. Decharme  05/2013  new param for equatorial forest
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG   
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL :: ZLAI, ZAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
ZLAI = PLAI
IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK)< 1.) THEN
  ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK))
END IF
!
ZAGRI=(1. - EXP( -0.6 * ZLAI ))
IF(OAGRI_TO_GRASS)ZAGRI=MIN(ZAGRI,0.95)
!
PGREEN= ZAGRI                     *(PVEGTYPE(NVT_C4  ) +     &! C4 crops
                                    PVEGTYPE(NVT_IRR ) +   &! irrigated crops
                                    PVEGTYPE(NVT_C3  )  )  &! C3 crops
       + MIN(1. - EXP( -0.5 * ZLAI ),0.95)                 &
                                  *(PVEGTYPE(NVT_TRBD) +   &! tropical broadleaf deciduous
                                    PVEGTYPE(NVT_TEBE) +   &! temperate broadleaf evergreen
                                    PVEGTYPE(NVT_TEBD) +   &! temperate broadleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_TENE) +   &! temperate needleleaf evergreen
                                    PVEGTYPE(NVT_BOBD) +   &! boreal broadleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_BONE) +   &! boreal needleleaf evergreen
                                    PVEGTYPE(NVT_BONE) +   &! boreal needleleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_SHRB) )   &! shrub
       + XEVERG_VEG               * PVEGTYPE(NVT_TRBE)     &! tropical broadleaf evergreen
       + MIN(1. - EXP( -0.6 * ZLAI ),0.95)                 &
                                  *(PVEGTYPE(NVT_GRAS) +   &! grassland
                                    PVEGTYPE(NVT_BOGR) +   &! Boreal grassland
                                    PVEGTYPE(NVT_TROG) +   &! tropical grassland
                                    PVEGTYPE(NVT_PARK)  )  &! irr. parks
       + 0.                       * PVEGTYPE(NVT_NO  )     &! no vegetation (smooth)
       + 0.                       * PVEGTYPE(NVT_SNOW)     &! no vegetation (snow)
       + 0.                       * PVEGTYPE(NVT_ROCK)      ! no vegetation (rocks)  
!
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_0D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      B. Decharme  05/2013  new param for equatorial forest
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI))      :: ZLAI, ZAGRI
REAL :: ZSUM1, ZSUM2, ZSUM3
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',0,ZHOOK_HANDLE)
!
ZLAI(:) = PLAI(:)
WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
  ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
END WHERE
!
ZAGRI(:)=(1. - EXP( -0.6 * ZLAI(:) ))
IF(OAGRI_TO_GRASS)ZAGRI(:)=MIN(ZAGRI(:),0.95)
!
DO JJ = 1,SIZE(PGREEN)
  !
  ZSUM1 = PVEGTYPE(JJ,NVT_C4)
  IF (NVT_C3/=0 .AND. NVT_IRR/=0) THEN
    ZSUM1 = ZSUM1 + PVEGTYPE(JJ,NVT_C3) + PVEGTYPE(JJ,NVT_IRR)
  ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
    ZSUM1 = ZSUM1 + PVEGTYPE(JJ,NVT_C3W) + PVEGTYPE(JJ,NVT_C3S)
  ENDIF
  !
  ZSUM2 = PVEGTYPE(JJ,NVT_TRBD) + PVEGTYPE(JJ,NVT_TEBE) + PVEGTYPE(JJ,NVT_TEBD) + &
          PVEGTYPE(JJ,NVT_TENE) + PVEGTYPE(JJ,NVT_BOBD) + PVEGTYPE(JJ,NVT_BONE) + &
          PVEGTYPE(JJ,NVT_BONE) + PVEGTYPE(JJ,NVT_SHRB) 
  IF (NVT_FLTR/=0) ZSUM2 = ZSUM2 + PVEGTYPE(JJ,NVT_FLTR)
  !
  ZSUM3 = PVEGTYPE(JJ,NVT_GRAS) + PVEGTYPE(JJ,NVT_BOGR) + PVEGTYPE(JJ,NVT_TROG)
  IF (NVT_PARK/=0) THEN
    ZSUM3 = ZSUM3 + PVEGTYPE(JJ,NVT_PARK)
  ELSEIF (NVT_FLGR/=0) THEN
    ZSUM3 = ZSUM3 + PVEGTYPE(JJ,NVT_FLGR)
  ENDIF
  !
  PGREEN(JJ)= ZAGRI(JJ) * ZSUM1  &
          + MIN(1. - EXP( -0.5 * ZLAI(JJ) ),0.95) * ZSUM2       &
          + XEVERG_VEG * PVEGTYPE(JJ,NVT_TRBE)                  &! tropical broadleaf evergreen
          + MIN(1. - EXP( -0.6 * ZLAI(JJ) ),0.95) * ZSUM3       &
          + 0.                      * PVEGTYPE(JJ,NVT_NO  )     &! no vegetation (smooth)
          + 0.                      * PVEGTYPE(JJ,NVT_SNOW)     &! no vegetation (snow)
          + 0.                      * PVEGTYPE(JJ,NVT_ROCK)      ! no vegetation (rocks)  
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_1D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: ZLAI, ZAGRI
REAL, DIMENSION(SIZE(PLAI,2)) :: ZSUM1, ZSUM2, ZSUM3
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
PGREEN(:,:) = XUNDEF
ZAGRI (:,:) = XUNDEF
!
WHERE (PLAI(:,:) /= XUNDEF)
      ZAGRI(:,:)=(1. - EXP( -0.6 * ZLAI(:,:) ))
ENDWHERE
IF(OAGRI_TO_GRASS)ZAGRI(:,:)=MIN(ZAGRI(:,:),0.95)
!
!
DO JJ = 1,SIZE(PGREEN)
  !
  ZSUM1(:) = PVEGTYPE(JJ,:,NVT_C4)
  IF (NVT_C3/=0 .AND. NVT_IRR/=0) THEN
    ZSUM1(:) = ZSUM1(:) + PVEGTYPE(JJ,:,NVT_C3) + PVEGTYPE(JJ,:,NVT_IRR)
  ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
    ZSUM1(:) = ZSUM1(:) + PVEGTYPE(JJ,:,NVT_C3W) + PVEGTYPE(JJ,:,NVT_C3S)
  ENDIF
  !
  ZSUM2(:) = PVEGTYPE(JJ,:,NVT_TRBD) + PVEGTYPE(JJ,:,NVT_TEBE) + PVEGTYPE(JJ,:,NVT_TEBD) + &
             PVEGTYPE(JJ,:,NVT_TENE) + PVEGTYPE(JJ,:,NVT_BOBD) + PVEGTYPE(JJ,:,NVT_BONE) + &
             PVEGTYPE(JJ,:,NVT_BONE) + PVEGTYPE(JJ,:,NVT_SHRB) 
  IF (NVT_FLTR/=0) ZSUM2(:) = ZSUM2(:) + PVEGTYPE(JJ,:,NVT_FLTR)
  !
  ZSUM3(:) = PVEGTYPE(JJ,:,NVT_GRAS) + PVEGTYPE(JJ,:,NVT_BOGR) + PVEGTYPE(JJ,:,NVT_TROG)
  IF (NVT_PARK/=0) THEN
    ZSUM3(:) = ZSUM3(:) + PVEGTYPE(JJ,:,NVT_PARK)
  ELSEIF (NVT_FLGR/=0) THEN
    ZSUM3(:) = ZSUM3(:) + PVEGTYPE(JJ,:,NVT_FLGR)
  ENDIF
  !
  WHERE (PLAI(JJ,:) /= XUNDEF)
    !
    PGREEN(JJ,:)= ZAGRI(JJ,:) * ZSUM1(:)  &
           + MIN((1. - EXP( -0.5 * ZLAI(JJ,:) )),0.95) * ZSUM2   &
           + XEVERG_VEG * PVEGTYPE(JJ,:,NVT_TRBE)                &! tropical broadleaf evergreen
           + MIN((1. - EXP( -0.6 * ZLAI(JJ,:) )),0.95) * ZSUM3   & 
           + 0.                      * PVEGTYPE(JJ,:,NVT_NO  )    &! no vegetation (smooth)
           + 0.                      * PVEGTYPE(JJ,:,NVT_SNOW)    &! no vegetation (snow)
           + 0.                      * PVEGTYPE(JJ,:,NVT_ROCK)      ! no vegetation (rocks)  
    !
  END WHERE
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_2D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_2D
!
!
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_VEGTYPE_1D(PLAI,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation for each vegetation patch
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!    F.Solmon/V.Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PGREEN ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_VEGTYPE_1D',0,ZHOOK_HANDLE)
PGREEN(:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  IF (PLAI(NVT_C4  )/=XUNDEF) PGREEN(NVT_C4  )=  MIN(1. - EXP( -0.6 * PLAI(NVT_C4  ) ),0.95)
  IF (NVT_IRR>0 .AND. NVT_C3>0) THEN
    IF (PLAI(NVT_IRR )/=XUNDEF) PGREEN(NVT_IRR )=  MIN(1. - EXP( -0.6 * PLAI(NVT_IRR ) ),0.95)
    IF (PLAI(NVT_C3  )/=XUNDEF) PGREEN(NVT_C3  )=  MIN(1. - EXP( -0.6 * PLAI(NVT_C3  ) ),0.95)
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (PLAI(NVT_C3W )/=XUNDEF) PGREEN(NVT_C3W )=  MIN(1. - EXP( -0.6 * PLAI(NVT_C3W ) ),0.95)
    IF (PLAI(NVT_C3S )/=XUNDEF) PGREEN(NVT_C3S )=  MIN(1. - EXP( -0.6 * PLAI(NVT_C3S ) ),0.95)
  ENDIF
ELSE
  IF (PLAI(NVT_C4  )/=XUNDEF) PGREEN(NVT_C4  )=  1. - EXP( -0.6 * PLAI(NVT_C4  ) )
  IF (NVT_IRR>0 .AND. NVT_C3>0) THEN
    IF (PLAI(NVT_IRR )/=XUNDEF) PGREEN(NVT_IRR )=  1. - EXP( -0.6 * PLAI(NVT_IRR ) )
    IF (PLAI(NVT_C3  )/=XUNDEF) PGREEN(NVT_C3  )=  1. - EXP( -0.6 * PLAI(NVT_C3  ) )
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (PLAI(NVT_C3W )/=XUNDEF) PGREEN(NVT_C3W )=  1. - EXP( -0.6 * PLAI(NVT_C3W ) )
    IF (PLAI(NVT_C3S )/=XUNDEF) PGREEN(NVT_C3S )=  1. - EXP( -0.6 * PLAI(NVT_C3S ) )
  ENDIF
ENDIF
!
IF (PLAI(NVT_TEBD)/=XUNDEF) PGREEN(NVT_TEBD)=  MIN(1. - EXP( -0.5 * PLAI(NVT_TEBD) ),0.95)
IF (PLAI(NVT_BONE)/=XUNDEF) PGREEN(NVT_BONE)=  MIN(1. - EXP( -0.5 * PLAI(NVT_BONE) ),0.95)
IF (PLAI(NVT_TRBD)/=XUNDEF) PGREEN(NVT_TRBD)=  MIN(1. - EXP( -0.5 * PLAI(NVT_TRBD) ),0.95)
IF (PLAI(NVT_TEBE)/=XUNDEF) PGREEN(NVT_TEBE)=  MIN(1. - EXP( -0.5 * PLAI(NVT_TEBE) ),0.95)
IF (PLAI(NVT_TENE)/=XUNDEF) PGREEN(NVT_TENE)=  MIN(1. - EXP( -0.5 * PLAI(NVT_TENE) ),0.95)
IF (PLAI(NVT_BOBD)/=XUNDEF) PGREEN(NVT_BOBD)=  MIN(1. - EXP( -0.5 * PLAI(NVT_BOBD) ),0.95)
IF (PLAI(NVT_BOND)/=XUNDEF) PGREEN(NVT_BOND)=  MIN(1. - EXP( -0.5 * PLAI(NVT_BOND) ),0.95)
IF (PLAI(NVT_SHRB)/=XUNDEF) PGREEN(NVT_SHRB)=  MIN(1. - EXP( -0.5 * PLAI(NVT_SHRB) ),0.95)
!
IF (NVT_FLTR>0) THEN
  IF (PLAI(NVT_FLTR)/=XUNDEF) PGREEN(NVT_FLTR)=  MIN(1. - EXP( -0.5 * PLAI(NVT_FLTR) ),0.95)
ENDIF
!
IF (PLAI(NVT_TRBE)/=XUNDEF) PGREEN(NVT_TRBE)=  XEVERG_VEG
!
IF (PLAI(NVT_GRAS)/=XUNDEF) PGREEN(NVT_GRAS)=  MIN(1. - EXP( -0.6 * PLAI(NVT_GRAS) ),0.95)
IF (PLAI(NVT_BOGR)/=XUNDEF) PGREEN(NVT_BOGR)=  MIN(1. - EXP( -0.6 * PLAI(NVT_BOGR) ),0.95)
IF (PLAI(NVT_TROG)/=XUNDEF) PGREEN(NVT_TROG)=  MIN(1. - EXP( -0.6 * PLAI(NVT_TROG) ),0.95)
IF (NVT_PARK>0) THEN
  IF (PLAI(NVT_PARK)/=XUNDEF) PGREEN(NVT_PARK)=  MIN(1. - EXP( -0.6 * PLAI(NVT_PARK) ),0.95)
ELSEIF (NVT_FLGR>0) THEN
  IF (PLAI(NVT_FLGR)/=XUNDEF) PGREEN(NVT_FLGR)=  MIN(1. - EXP( -0.6 * PLAI(NVT_FLGR) ),0.95)
ENDIF
!
PGREEN(NVT_NO  )= 0.
PGREEN(NVT_SNOW)= 0.
PGREEN(NVT_ROCK)= 0.  
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_VEGTYPE_1D',1,ZHOOK_HANDLE)

!
END FUNCTION GREEN_FROM_LAI_VEGTYPE_1D
!
!--------------------------------------------
!
