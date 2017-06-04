!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_ALBEDO_TEB(TOP, T, TPN, B,  PZENITH, PAZIM, &
                                     PALB_GARDEN, PALB_GREENROOF, PDIR_ALB_TOWN, PSCA_ALB_TOWN)
!     ###################################################
!
!!**** *AVERAGED_ALBEDO_TEB* computes averaged albedo for TEB scheme
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!     C. de Munck & A. Lemonsu   09/2011 Greenroofs
!!    G. Pigeon                  09/2012 B%XTRAN_WIN as arguments
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_TYPE_SNOW
!
USE MODI_URBAN_SOLAR_ABS
USE MODE_SURF_SNOW_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
!
REAL, DIMENSION(:), INTENT(IN) :: PZENITH       ! zenithal solar angle
REAL, DIMENSION(:), INTENT(IN) :: PAZIM         ! solar azimuthal angle
!                                               ! (radian from N, clockwise)
!
REAL, DIMENSION(:), INTENT(IN) :: PALB_GARDEN   ! green areas albedo
REAL, DIMENSION(:), INTENT(IN) :: PALB_GREENROOF! green roof albedo
REAL, DIMENSION(:), INTENT(OUT):: PDIR_ALB_TOWN ! direct albedo
REAL, DIMENSION(:), INTENT(OUT):: PSCA_ALB_TOWN ! diffuse albedo
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDN_ROOF       ! snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDN_ROAD       ! on the surface
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROOF       ! free-snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROAD       ! on the surface
LOGICAL, DIMENSION(SIZE(T%XBLD)) :: GMASK       ! .false. (= no snow precip.)
!
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDIR_SW        ! direct and diffuse shortwave radiation
REAL, DIMENSION(SIZE(T%XBLD)) :: ZSCA_SW        ! to mimic radiation behaviour of town
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_SW_PANEL      ! shortwave absorbed by solar panels
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_ROAD       ! shortwave received by roads
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_WALL_A     ! shortwave received by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_WALL_B     ! shortwave received by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_GARDEN     ! shortwave received by green areas
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_SNOW_ROAD  ! shortwave received by snow on roads
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_ROOF       ! shortwave received by roofs
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZSW_RAD_GARDEN ! total solar radiation reaching green areas
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_WIN       ! shortwave received by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREF_SW_GRND      !
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREF_SW_FAC       !
REAL, DIMENSION(SIZE(T%XBLD)) :: ZE_SHADING        !
LOGICAL, DIMENSION(SIZE(T%XBLD)) :: GSHAD_DAY
!
TYPE(DIAG_MISC_TEB_t) :: YDMT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* snow fractions
!  --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_TEB',0,ZHOOK_HANDLE)
GMASK(:) = .FALSE.
 CALL SNOW_FRAC_ROAD(T%TSNOW_ROAD%WSNOW(:,1),GMASK,ZDN_ROAD,ZDF_ROAD)
 CALL SNOW_FRAC_ROOF(T%TSNOW_ROOF%WSNOW(:,1),GMASK,ZDN_ROOF,ZDF_ROOF)
!
!
!* town  direct and diffuse albedo
!  -------------------------------
!
ZDIR_SW=1.
ZSCA_SW=1.
GSHAD_DAY=.FALSE.
IF (SIZE(B%LSHAD_DAY)>0) GSHAD_DAY=B%LSHAD_DAY
!
 CALL URBAN_SOLAR_ABS(TOP, T, B, YDMT, ZDIR_SW, ZSCA_SW, PZENITH, PAZIM,  &
                      TPN%XFRAC_PANEL, TPN%XALB_PANEL, PALB_GARDEN,        &
                      T%XSVF_GARDEN, PALB_GREENROOF, ZDN_ROOF, ZDF_ROOF,   &
                      ZDN_ROAD, ZDF_ROAD,  ZREC_SW_ROAD, ZREC_SW_SNOW_ROAD,&
                      ZREC_SW_WALL_A, ZREC_SW_WALL_B, ZREC_SW_GARDEN,      &
                      ZREC_SW_ROOF, PDIR_ALB_TOWN, PSCA_ALB_TOWN,          &
                      ZSW_RAD_GARDEN, ZREC_SW_WIN, ZREF_SW_GRND,           &
                      ZREF_SW_FAC, ZE_SHADING, GSHAD_DAY, GMASK,           &
                      OALB_ONLY=.TRUE. )
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_ALBEDO_TEB
