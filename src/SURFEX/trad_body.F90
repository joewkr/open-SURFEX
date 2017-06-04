!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_TRAD_BODY
INTERFACE
FUNCTION TRAD_BODY(PSCA_SW, PREF_SW_FAC, PREF_SW_GRND,   &
                   PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD, &
                   PBLD, PBLD_HEIGHT, PWALL_O_HOR,       &
                   PDIR_SW, PZENITH) RESULT(PTRAD_BODY)
REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW       ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_FAC   ! Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_GRND  ! Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_FAC  ! Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_GRND ! Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD       ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PBLD          ! plan area density of building (m2(bld)/m2(urban))
REAL, DIMENSION(:), INTENT(IN)  :: PBLD_HEIGHT   ! building height (m)
REAL, DIMENSION(:), INTENT(IN)  :: PWALL_O_HOR   ! ratio between facade and urban horizontal surface (m2(facade)/m2(urban))
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(SIZE(PSCA_SW)) :: PTRAD_BODY
END FUNCTION TRAD_BODY
END INTERFACE
END MODULE MODI_TRAD_BODY
!   ##########################################################################
FUNCTION TRAD_BODY(PSCA_SW, PREF_SW_FAC, PREF_SW_GRND, PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
                   PBLD, PBLD_HEIGHT, PWALL_O_HOR, PDIR_SW, PZENITH) RESULT(PTRAD_BODY)
!   ##########################################################################
!
!!****  *TRAD_BODY  
!!
!!    PURPOSE
!!    -------
!
!     Computes the radiant temperature equivalent to the total radiation
!     received by the human body
!     
!!**  METHOD
!     ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! a supplement
!!    MODD_CST
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  03/2011
!!      V.MASSON   08/2014 : bug in road view factor in computation of Universal Thermal Climate Index (diagnostic only)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XSTEFAN, XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW       ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_FAC   ! Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_GRND  ! Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_FAC  ! Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_GRND ! Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD       ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PBLD          ! plan area density of building (m2(bld)/m2(urban))
REAL, DIMENSION(:), INTENT(IN)  :: PBLD_HEIGHT   ! building height (m)
REAL, DIMENSION(:), INTENT(IN)  :: PWALL_O_HOR   ! ratio between facade and urban horizontal surface (m2(facade)/m2(urban))
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: PZENITH !solar zenithal angle (rad from vert.)
!REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW !Direct solar radiation (W/m2)
!REAL, DIMENSION(:), INTENT(IN) :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(SIZE(PSCA_SW))    :: PTRAD_BODY
!REAL, DIMENSION(:) :: PTRAD_BODY

!*      0.2    declarations of local variables
REAL :: ZHB = 1.7 !average height of human person (m)
REAL :: ZAB = 0.7 !absorption coef of solar radiation by human body
REAL :: ZEB = 0.97 !emissivity of human body
!REAL, DIMENSION(SIZE(PBLD)) :: TRAD_BODY
REAL, DIMENSION(SIZE(PBLD)) :: ZWROAD !width of the road (m)
REAL, DIMENSION(SIZE(PBLD)) :: ZL1, ZL2, ZL4 !lengths for view factor calculation
REAL, DIMENSION(SIZE(PBLD)) :: ZFFAC !facade view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZFGRND !ground view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZFSKY !sky view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZDIRSWBODY !solar radiation received by human body
REAL, DIMENSION(SIZE(PBLD)) :: ZELEV !solar elevation angle
REAL, DIMENSION(SIZE(PBLD)) :: ZRADBODY !total radiation received by human body
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY',0,ZHOOK_HANDLE)
!
DO JJ = 1, SIZE(PBLD_HEIGHT) 
  !
  !*  1 - calculation of view factors
  ZWROAD(JJ) = PBLD_HEIGHT(JJ) * 2. * (1. - PBLD(JJ)) / PWALL_O_HOR(JJ)
  !
  ZL1(JJ) = SQRT(ZHB**2                   + (ZWROAD(JJ)/2.)**2)
  ZL2(JJ) = SQRT( PBLD_HEIGHT(JJ)**2      + (ZWROAD(JJ)/2.)**2)
  ZL4(JJ) = SQRT((PBLD_HEIGHT(JJ)-ZHB)**2 + (ZWROAD(JJ)/2.)**2)
  !
  ZFFAC (JJ) = (ZL1(JJ) + ZL2(JJ) - ZWROAD(JJ)/2. - ZL4(JJ)) / (2. * ZHB)
  ZFGRND(JJ) = 0.5*ZWROAD(JJ)/ZHB
  ZFGRND(JJ) = 0.5 * (ZFGRND(JJ) + 1. - SQRT(ZFGRND(JJ)**2 + 1.))
  ZFSKY (JJ) = 1. - ZFFAC(JJ) - ZFGRND(JJ)
  !
  !*  2 - base calculation for both sun and shade
  ZRADBODY(JJ) = ZAB/ZEB * &
              ( PSCA_SW(JJ)*ZFSKY(JJ) + PREF_SW_FAC (JJ)*ZFFAC(JJ) + PREF_SW_GRND (JJ)*ZFGRND(JJ) ) &
              + PLW_RAD(JJ)*ZFSKY(JJ) + PEMIT_LW_FAC(JJ)*ZFFAC(JJ) + PEMIT_LW_GRND(JJ)*ZFGRND(JJ)
  !
ENDDO
!

!*  3 - add direct contribution in case of sunny conditions 
IF (PRESENT(PDIR_SW) .AND. PRESENT(PZENITH)) THEN
  DO JJ = 1, SIZE(PBLD_HEIGHT)
    ZELEV(JJ) = XPI/2. - PZENITH(JJ)
    IF (ZELEV(JJ) < 1E-6) ZELEV(JJ) = 0.
    ZDIRSWBODY(JJ) = PDIR_SW(JJ) * 0.308 * COS( ZELEV(JJ)*(1-(ZELEV(JJ)*180./XPI)**2)/48402. )
      ! the direct solar radiation is weighted by a projected area factor which can be expressed by this equation
      ! for a rotationally symmetric human being (Fanger, 1970)
    ZRADBODY  (JJ) = ZRADBODY(JJ) + ZAB/ZEB*ZDIRSWBODY(JJ)
  ENDDO
ENDIF

!PTRAD_BODY(:) = (ZRADBODY(:)/XSTEFAN)**0.25
DO JJ=1, SIZE(PBLD_HEIGHT)
   PTRAD_BODY(JJ) = (ZRADBODY(JJ)/XSTEFAN)**0.25
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY',1,ZHOOK_HANDLE)
!
END FUNCTION TRAD_BODY
