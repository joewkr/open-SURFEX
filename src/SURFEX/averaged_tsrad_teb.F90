!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_TSRAD_TEB(T, B, PEMIS_GARDEN, PTS_GARDEN, PEMIS_GREENROOF, &
                                    PTS_GREENROOF, PEMIS, PTSRAD     ) 
!     ###################################################
!
!!**** *AVERAGED_TSRAD_TEB* computes averaged emissivity and radiative surface
!!                          temperature for TEB scheme
!!
!!    PURPOSE
!!    -------
!!
!!    METHODi
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
!!    09/2012     C. de Munck, A. Lemonsu : add green roofs
!!
!!    Original    01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
!
USE MODD_TYPE_SNOW
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XSTEFAN
!
USE MODI_URBAN_LW_COEF
!
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
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
!
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GARDEN   ! green area emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GARDEN     ! green area surf. temp.
REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GREENROOF! green roof emissivity (snowfree)
REAL, DIMENSION(:), INTENT(IN) :: PTS_GREENROOF  ! green roof surf. temp.
REAL, DIMENSION(:), INTENT(OUT):: PEMIS          ! averaged emissivity (all tiles)
REAL, DIMENSION(:), INTENT(OUT):: PTSRAD         ! averaged radiaitve temp. (all tiles)
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(T%XEMIS_ROOF)) :: ZDN_ROOF       ! snow fraction 
REAL, DIMENSION(SIZE(T%XEMIS_ROOF)) :: ZDN_ROAD       ! on the surface
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROOF       ! free-snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROAD       ! on the surface
LOGICAL, DIMENSION(SIZE(T%XBLD)) :: GMASK       ! .false. (= no snow precip.)
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WA_TO_WB   ! longwave exchange coefficients
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WA_TO_R
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WB_TO_R
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WA_TO_NR
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WB_TO_NR
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WA_TO_G
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WB_TO_G
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WA_TO_WIN
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WB_TO_WIN
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_R_TO_WA
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_R_TO_WB
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_R_TO_WIN
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_G_TO_WA
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_G_TO_WB
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_G_TO_WIN
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_WA
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_WB
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_R
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_NR
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_G
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_S_TO_WIN
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WIN_TO_WA
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WIN_TO_WB
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WIN_TO_R
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WIN_TO_NR
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_WIN_TO_G
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_NR_TO_WA
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_NR_TO_WB
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_NR_TO_WIN
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_RAD          ! incoming LW to mimic
!                                               ! radiation behaviour of town
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_WALL     ! longwave absorbed by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_ROAD     ! longwave absorbed by roads
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_ROOF     ! longwave absorbed by roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_SNOW_ROAD! longwave absorbed by snow
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_SNOW_ROOF! on roads and roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_GARDEN   ! longwave absorbed by gardens
REAL, DIMENSION(SIZE(T%XBLD)) :: ZABS_LW_GREENROOF! longwave absorbed by green roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZLW_UP           ! outgoing longwave
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZT_SKY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* snow fractions
!  --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',0,ZHOOK_HANDLE)
GMASK(:) = .FALSE.
 CALL SNOW_FRAC_ROAD(T%TSNOW_ROAD%WSNOW(:,1),GMASK,ZDN_ROAD,ZDF_ROAD)
 CALL SNOW_FRAC_ROOF(T%TSNOW_ROOF%WSNOW(:,1),GMASK,ZDN_ROOF,ZDF_ROOF)
!
! fixed incoming LW (W/m2)
ZLW_RAD(:)= XSTEFAN * (T%XT_ROAD(:,1) ** 4)
!
! LW absorbed by roofs
ZABS_LW_ROOF(:) = T%XEMIS_ROOF(:) * (ZLW_RAD(:) - XSTEFAN * T%XT_ROOF(:,1)**4)
!
!* LW absorbed by snow on roof
ZABS_LW_SNOW_ROOF(:) = T%TSNOW_ROOF%EMIS(:) * (ZLW_RAD(:) - XSTEFAN * T%TSNOW_ROOF%TS(:)**4)
!
!* town averaged emissivity
PEMIS(:) = T%XBLD(:) * (1.-T%XGREENROOF(:)) * (ZDF_ROOF(:)*T%XEMIS_ROOF     (:)    &
                                       + ZDN_ROOF(:)*T%TSNOW_ROOF%EMIS(:)) &
         + T%XBLD(:) *     T%XGREENROOF(:)  *              PEMIS_GREENROOF(:)

!
!* long-wave trapping coefficients
!  -------------------------------
!
   ZT_SKY(:) = (ZLW_RAD(:)/XSTEFAN)**0.25
   !
   CALL URBAN_LW_COEF(B, T, ZLW_RAD, PEMIS_GARDEN,             &
                      T%XT_ROAD(:,1), PTS_GARDEN,              &  
                      ZLW_WA_TO_WB, ZLW_WA_TO_R, ZLW_WB_TO_R,  &
                      ZLW_WA_TO_NR,ZLW_WB_TO_NR,               &
                      ZLW_WA_TO_G, ZLW_WB_TO_G,                &
                      ZLW_WA_TO_WIN, ZLW_WB_TO_WIN,            &
                      ZLW_R_TO_WA, ZLW_R_TO_WB, ZLW_R_TO_WIN,  &
                      ZLW_G_TO_WA, ZLW_G_TO_WB, ZLW_G_TO_WIN,  &
                      ZLW_S_TO_WA, ZLW_S_TO_WB, ZLW_S_TO_R,    &
                      ZLW_S_TO_NR, ZLW_S_TO_G, ZLW_S_TO_WIN,   &
                      ZLW_WIN_TO_WA, ZLW_WIN_TO_WB, ZLW_WIN_TO_R, &
                      ZLW_WIN_TO_NR, ZLW_WIN_TO_G,                &
                      ZLW_NR_TO_WA, ZLW_NR_TO_WB, ZLW_NR_TO_WIN   )
   !
   !
   !* town averaged emissivity
   !  ------------------------
   !
   PEMIS(:) =  PEMIS(:)                                                      &
              + T%XROAD(:)*T%XSVF_ROAD(:)* (ZDF_ROAD(:)* T%XEMIS_ROAD(:)     &
                                      + ZDN_ROAD(:)* T%TSNOW_ROAD%EMIS(:)) &
              + T%XWALL_O_HOR(:)       * T%XSVF_WALL(:)   * T%XEMIS_WALL(:)  &
              + T%XGARDEN(:)           * T%XSVF_GARDEN(:) * PEMIS_GARDEN(:)
   
   !
   ! LW absorbed by roads
   ZABS_LW_ROAD(:) =  ZLW_S_TO_R  (:) * (ZT_SKY       (:) - T%XT_ROAD(:,1)) &
                    + ZLW_WA_TO_R (:) * (T%XT_WALL_A(:,1) - T%XT_ROAD(:,1)) &
                    + ZLW_WB_TO_R (:) * (T%XT_WALL_B(:,1) - T%XT_ROAD(:,1)) &
                    + ZLW_WIN_TO_R(:) * (B%XT_WIN1    (:) - T%XT_ROAD(:,1))
   
   !
   ! LW absorbed by walls
   ZABS_LW_WALL(:) =( ZLW_S_TO_WA(:)  * (ZT_SKY(:)       - T%XT_WALL_A(:,1)) &
                    + ZLW_R_TO_WA(:)  * (T%XT_ROAD(:,1)  - T%XT_WALL_A(:,1)) &
                    + ZLW_G_TO_WA(:)  * (PTS_GARDEN(:)   - T%XT_WALL_A(:,1)) &
                    + ZLW_WIN_TO_WA(:)* (B%XT_WIN1(:)    - T%XT_WALL_A(:,1)) &
                    + ZLW_S_TO_WB(:)  * (ZT_SKY(:)       - T%XT_WALL_B(:,1)) &
                    + ZLW_R_TO_WB(:)  * (T%XT_ROAD(:,1)  - T%XT_WALL_B(:,1)) &
                    + ZLW_G_TO_WB(:)  * (PTS_GARDEN(:)   - T%XT_WALL_B(:,1)) &
                    + ZLW_WIN_TO_WB(:)* (B%XT_WIN1(:)    - T%XT_WALL_B(:,1)))&
                   * 0.5
   
   !
   !* LW absorbed by snow on road
   ZABS_LW_SNOW_ROAD(:) =  ZLW_S_TO_R   (:) * (ZT_SKY(:)        - T%TSNOW_ROAD%TS(:)) &
                         + ZLW_WA_TO_NR (:) * (T%XT_WALL_A(:,1) - T%TSNOW_ROAD%TS(:)) &
                         + ZLW_WB_TO_NR (:) * (T%XT_WALL_B(:,1) - T%TSNOW_ROAD%TS(:)) &
                         + ZLW_WIN_TO_NR(:) * (B%XT_WIN1(:)     - T%TSNOW_ROAD%TS(:))
   !
   !* LW absorbed by gardens
   ZABS_LW_GARDEN(:) =  ZLW_S_TO_G  (:)*(ZT_SKY       (:)-PTS_GARDEN(:)) &
                      + ZLW_WA_TO_G (:)*(T%XT_WALL_A(:,1)-PTS_GARDEN(:)) &
                      + ZLW_WB_TO_G (:)*(T%XT_WALL_B(:,1)-PTS_GARDEN(:)) &
                      + ZLW_WIN_TO_G(:)*(B%XT_WIN1    (:)-PTS_GARDEN(:))
   !
   !* LW absorbed by green roofs
ZABS_LW_GREENROOF(:) = PEMIS_GREENROOF(:) * (ZLW_RAD(:) - XSTEFAN * PTS_GREENROOF(:)** 4)
   
!
!* outgoing longwave radiation
ZLW_UP(:) = ZLW_RAD(:)                                                     &
          - ( T%XBLD(:) *(1.-T%XGREENROOF(:))*ZDF_ROOF(:)*ZABS_LW_ROOF     (:)   &
             +T%XBLD(:) *(1.-T%XGREENROOF(:))*ZDN_ROOF(:)*ZABS_LW_SNOW_ROOF(:)   &
             +T%XBLD(:) *    T%XGREENROOF(:)             *ZABS_LW_GREENROOF(:)   &
             +T%XROAD(:)                 *ZDF_ROAD(:)*ZABS_LW_ROAD     (:)   &
             +T%XROAD(:)                 *ZDN_ROAD(:)*ZABS_LW_SNOW_ROAD(:)   &
             +T%XWALL_O_HOR(:)                       *ZABS_LW_WALL     (:)   &
             +T%XGARDEN(:)                           *ZABS_LW_GARDEN   (:))
!
!* town radiative surface temperature
PTSRAD(:)   = ((ZLW_UP(:) - ZLW_RAD(:)*(1.-PEMIS(:))) /PEMIS(:)/XSTEFAN)**0.25
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_TSRAD_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_TSRAD_TEB
