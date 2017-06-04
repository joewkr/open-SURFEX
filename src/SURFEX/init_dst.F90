!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE INIT_DST (DSTK, U, &
                     HPROGRAM,  &    ! Program calling unit
                  KSIZE_P, & ! Number of nature points in a patch
                  KR_P, &    ! Mask from patch --> nature vectors
                  PVEGTYPE_PATCH  ) ! fraction (in a nature point) of a vegtype for a patch

!
USE MODD_DST_n, ONLY : DST_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DST_SURF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK
!
USE MODI_GET_LUOUT
USE MODI_GET_VEGTYPE_2_PATCH_MASK
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!PASSED VARIABLES
!
TYPE(DST_t), INTENT(INOUT) :: DSTK
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN)      :: HPROGRAM              !Passing unit
!
INTEGER, INTENT(IN) :: KSIZE_P
INTEGER, DIMENSION(:), INTENT(IN) :: KR_P
REAL, DIMENSION(:,:), INTENT(IN) :: PVEGTYPE_PATCH
!
!LOCAL VARIABLES
 CHARACTER(LEN=4)    :: CRGUNIT               ! type of log-normal geometric mean radius
INTEGER             :: JVEG                  ! Counter for vegetation classes
INTEGER             :: JVEG_IN               ! Vegetation index
INTEGER             :: JPATCH                ! Counter for patches
INTEGER             :: JMODE                 ! Counter for dust modes
INTEGER             :: JMODE_IDX             ! Index for dust modes
INTEGER             :: ILUOUT
INTEGER             :: ISIZE_LARGEST_DST
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!get output listing unit
IF (LHOOK) CALL DR_HOOK('INIT_DST',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!Allocate memory for the real values which will be used by the model
ALLOCATE(DSTK%XEMISRADIUS_DST(NDSTMDE))
ALLOCATE(DSTK%XEMISSIG_DST   (NDSTMDE))
ALLOCATE(DSTK%XMSS_FRC_SRC   (NDSTMDE))
!
!Get initial size distributions. This is cut and pasted
!from dead routine dstpsd.F90
!Check for different source parameterizations
IF(CEMISPARAM_DST.EQ."She84")THEN
  CRGUNIT   = 'MASS'
  XEMISRADIUS_INI_DST(:) = 0.5d6 * (/ 0.0111e-6, 2.524e-6, 42.10e-6 /)  ! [um]  Mass median radius She84 p. 75 Table 1
  XEMISSIG_INI_DST   (:) = (/ 1.89 , 2.0  , 2.13 /)             ! [frc] Geometric standard deviation She84 p. 75 Table 1
  XMSS_FRC_SRC_INI   (:) = (/2.6e-6, 0.781, 0.219/)             ! [frc] Mass fraction She84 p. 75 Table 1
ELSEIF(CEMISPARAM_DST.EQ."PaG77")THEN
  CRGUNIT   = 'MASS'
  XEMISRADIUS_INI_DST(:) = 0.5d6 * (/0.27e-6  ,  5.6e-6  ,  57.6e-6 /) ! [um] Mass median radius PaG77 p. 2080 Table 1 
  XEMISSIG_INI_DST   (:) = (/ 1.88, 2.2  , 1.62 /)              ! [frc] Geometric standard deviation PaG77 p. 2080 Table 1
  XMSS_FRC_SRC_INI   (:) = (/0.036, 0.957, 0.007/)              ! [frc] Mass fraction BSM96 p. 73 Table 2 (ad hoc)  
ELSEIF(CEMISPARAM_DST.EQ."Dal87") THEN 
  ! D'Almeida, 1987 as default
  CRGUNIT   = 'MASS'
  XEMISRADIUS_INI_DST(:) = 0.5d6 * (/ 0.832e-6 ,  4.82e-6 , 19.38e-6 /) ! [um] Mass median radius BSM96 p. 73 Table 2
  XEMISSIG_INI_DST   (:) = (/ 2.10, 1.90 , 1.60 /)              ! [frc] Geometric standard deviation BSM96 p. 73 Table 2
  XMSS_FRC_SRC_INI   (:) = (/0.036, 0.957, 0.007/)              ! [frc] Mass fraction BSM96 p. 73 Table 2  
ELSEIF (CEMISPARAM_DST.EQ."alf98".OR.CEMISPARAM_DST.eq."EXPLI") THEN  !  Alfaro et al 1998 as default
  IF (CEMISPARAM_DST.EQ."alf98") XFLX_MSS_FDG_FCT = 6e-4
  IF (CEMISPARAM_DST.EQ."EXPLI") XFLX_MSS_FDG_FCT = 3.5e-4
  CRGUNIT   = 'MASS'
  XEMISRADIUS_INI_DST(:) = 0.5*(/ 1.5, 6.7, 14.2 /)      ! [um] Mass median radius BSM96 p. 73 Table 2
  XEMISSIG_INI_DST   (:) = (/1.70, 1.60, 1.50/)          ! [frc] Geometric standard deviation BSM96 p. 73 Table 2
  XMSS_FRC_SRC_INI   (:) = (/0.01, 0.19, 0.8 /)          ! [frc] Mass fraction BSM96 p. 73 Table 2  
ELSEIF (CEMISPARAM_DST.EQ."AMMA ") THEN ! Default: New distribution from AMMA
  XFLX_MSS_FDG_FCT = 105.e-4
  CRGUNIT   = 'NUMB' 
  XEMISRADIUS_INI_DST(:) = 0.5*(/ 0.078, 0.641, 5.00 /)  ! [um] Number median radius 
  XEMISSIG_INI_DST   (:) = (/ 1.75,  1.76, 1.70/)        ! [frc] Geometric standard deviation 
  XMSS_FRC_SRC_INI   (:) = (/0.008, 0.092, 0.99/)        ! [frc] Mass fraction  
ELSEIF (CEMISPARAM_DST.EQ."CRUM ") THEN ! Default: New distribution from AMMA
  XFLX_MSS_FDG_FCT = 10.e-4
  CRGUNIT   = 'NUMB' 
  XEMISRADIUS_INI_DST(:) = 0.5*(/ 0.078, 0.641, 5.00 /)  ! [um] Number median radius 
  XEMISSIG_INI_DST   (:) = (/  1.75,   1.76, 1.70   /)   ! [frc] Geometric standard deviation   
  XMSS_FRC_SRC_INI   (:) = (/0.0005, 0.0029, 0.9966/)    ! [frc] Mass fraction  
ELSE
  WRITE(ILUOUT,*) " FATAL ERROR "
  WRITE(ILUOUT,*) " YOU MUST DECIDE THE EMISSIUON PARAMETERIZATION, YOU USES "
  WRITE(ILUOUT,*) " CEMISPARAM_DST = ",CEMISPARAM_DST," AND IT IS NOT DEFINED "
  WRITE(ILUOUT,*) " see init_dstn.f90 to see what dust parameterization is available. "
  CALL ABOR1_SFX("INIT_DST: CEMISPARAM_DST not defined ")
ENDIF
!
DO JMODE=1,NDSTMDE
  JMODE_IDX=JORDER_DST(JMODE)
  !
  DSTK%XEMISSIG_DST   (JMODE) = XEMISSIG_INI_DST(JMODE_IDX)
  DSTK%XEMISRADIUS_DST(JMODE) = XEMISRADIUS_INI_DST(JMODE_IDX)
  DSTK%XMSS_FRC_SRC   (JMODE) = XMSS_FRC_SRC_INI(JMODE_IDX)
  !
  !Get emisradius, and at the same time convert to number median radius
  IF (CRGUNIT=='MASS') &
    DSTK%XEMISRADIUS_DST(JMODE) = DSTK%XEMISRADIUS_DST(JMODE) * EXP(-3.d0 * (LOG(DSTK%XEMISSIG_DST(JMODE)))**2)  
  !
ENDDO
!
!Normalize the sum of the emissions to 1 so that all dust is
!put in one mode or the other
IF(SUM(DSTK%XMSS_FRC_SRC(:)).LT.1.) DSTK%XMSS_FRC_SRC(:) = DSTK%XMSS_FRC_SRC(:) / SUM(DSTK%XMSS_FRC_SRC(:))
!
!Allocate memory
!ALLOCATE(NVEGNO_DST)
!Set the number of classes that can emit dust (fxm: set this elsewhere)
NVEGNO_DST = 2
!
!Allocate memory for the vegtype-translator
ALLOCATE(DSTK%NVT_DST(NVEGNO_DST))
!
!Set the dust/vegtype translator vector
DSTK%NVT_DST(1)  = NVT_NO
DSTK%NVT_DST(2)  = NVT_ROCK
!
!Allocate memory for roughness lengths of erodible surfaces
ALLOCATE(DSTK%Z0_EROD_DST(NVEGNO_DST))
!
!Set the roughness lengths corresponding to erodible surfaces
!Smooth roughness length is given to 1.d-5 (dstmbl.f90)
DSTK%Z0_EROD_DST(1) = 30.d-6    !m (30 um) 
DSTK%Z0_EROD_DST(2) = 200.d-6   !m (200 um) 
!
!Allocate memory for dust emitter surface vectors in patch vectors
IF (.NOT.ASSOCIATED(DSTK%NSIZE_PATCH_DST)) ALLOCATE(DSTK%NSIZE_PATCH_DST(NVEGNO_DST))
!
DO JVEG = 1,NVEGNO_DST
  !Count all the points in the patch where you have dust emitter vegetation
  DSTK%NSIZE_PATCH_DST(JVEG) = COUNT(PVEGTYPE_PATCH(:,DSTK%NVT_DST(JVEG)) > 0.) 
ENDDO
!
!Find the largest dust emitter vector in any patch
!ALLOCATE (NSIZE_LARGEST_DST)
ISIZE_LARGEST_DST = 0
DO JVEG = 1,NVEGNO_DST
  ISIZE_LARGEST_DST = max(ISIZE_LARGEST_DST,DSTK%NSIZE_PATCH_DST(JVEG))
ENDDO
!
!Allocate memory for KR_PATCH_DST mask translate from patch vector to dust vector
ALLOCATE(DSTK%NR_PATCH_DST(ISIZE_LARGEST_DST,NVEGNO_DST))
!
!Initialize the mask array
DSTK%NR_PATCH_DST(:,:)=0
!
!Get values from the dust emitter vegetation mask
DO JVEG=1,NVEGNO_DST
  JVEG_IN = DSTK%NVT_DST(JVEG)          ! Get the real vegtype index
#ifdef RJ_OFIX
  CALL GET_VEGTYPE_2_PATCH_MASK(ILUOUT,    &
           DSTK%NSIZE_PATCH_DST(JVEG),             &!I Size of dust emitter vector
           KSIZE_P,                   &!I Size of patch vector
!RJ: attempt to make this call generic
           KR_P,&!I Mask from patch to nature
           PVEGTYPE_PATCH,                           &!I Fraction of vegtype of nature point within jpatch 
           DSTK%NR_PATCH_DST(:DSTK%NSIZE_PATCH_DST(JVEG),JVEG),  &!O Part of mask array to fill with values
           JVEG_IN                                  &!I Index of vegtype in question
             )  
#else
  CALL GET_VEGTYPE_2_PATCH_MASK(ILUOUT,    &
           DSTK%NSIZE_PATCH_DST(JVEG),             &!I Size of dust emitter vector
           KSIZE_P,                   &!I Size of patch vector
           KR_P,                              &!I Mask from patch to nature
           PVEGTYPE_PATCH,                           &!I Fraction of vegtype of nature point within jpatch 
           DSTK%NR_PATCH_DST(:DSTK%NSIZE_PATCH_DST(JVEG),JVEG),  &!O Part of mask array to fill with values
           JVEG_IN                                  &!I Index of vegtype in question
             )  
#endif
ENDDO    !Loop on veg-types
!
IF (LHOOK) CALL DR_HOOK('INIT_DST',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_DST

