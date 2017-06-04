!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOIL( IO, KK, PK, PEK, DMI, PVEG, PCS, PFROZEN1, PFFG_NOSNOW, PFFV_NOSNOW               )  
!     ##########################################################################
!
!!****  *SOIL*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the coefficients related to the soil (i.e., CG, CT,
!     C1, C2, WGEQ) and to the snow canopy (i.e., Cs, ps, psng, psnv, and psnz0)
!         
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
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/03/95 
!!                  20/03/96      (Masson)  error in the threshold for PCG
!!                  04/09/98      (Masson)  error in C1 normalization
!!                  16/09/98      (Masson)  frozen water in the soil
!!                  07/10/98      (Masson)  new C1 formulation
!!                  26/11/98      (Boone)   C1 option (old vs new formulations)
!!                  15/03/99      (Boone)   Soil ice modifiactions: scale C1sat,
!!                                          use surface ice-weighted CG, GB method
!!                                          for dry conditions uses ZWWILT for MAX
!!                  25/03/99      (Boone)   Added Johansen (1975)/Peters-Lidard 
!!                                          option to explicitly compute CG
!!                  25/05/08     (Decharme) Added flood properties 
!!                  22/06/10     (Chauvin)  XWGMIN added as a limit value of ZWG2              
!!                                          Modification of the formula for DMI%XWGEQ                                      
!!                                          to solve numerical problem
!!                     10/10     (Decharme) The previous computation of WGEQ as ( 1.-ZX(JJ)**(IP%XPCOEF(JJ)*8.) )
!!                                          can introduced some model explosions for heavy clay soil
!!                     12/14     (LeMoigne) EBA scheme update
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,       ONLY : XPI, XCI, XRHOLI, XDAY, XCL, XRHOLW, XCONDI
USE MODD_ISBA_PAR,   ONLY : XCONDWTR, XWGMIN
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DEEPSOIL,   ONLY : LPHYSDOMC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMI
!
REAL, DIMENSION(:), INTENT(IN)    :: PVEG
!
REAL, DIMENSION(:), INTENT(OUT)   :: PCS, PFROZEN1
!                                      soil and snow coefficients
!                                      PCS = heat capacity of the snow
!                                      PFROZEN1   = fraction of ice in superficial
!                                               soil
!
REAL, DIMENSION(:), INTENT(IN)   :: PFFG_NOSNOW, PFFV_NOSNOW
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PVEG))   :: ZLAMS,                         &
!                                              conductivity of snow
!
                                  ZCW1MAX, ZX2, ZY1, ZY2,     &
                                  ZLYMY1, ZZA, ZZB, ZDELTA,   &
                                  ZA, ZB,                          &
!                                              temporary variables for the 
!                                              calculation of DMI%XC1 in the case
!                                              where PWG < PWWILT (i.e., dry soils)
!
                                  ZX,                              &
!                                              temporary variable for the 
!                                              calculation of DMI%XWGEQ
                                  ZWSAT,                           &
!                                              Wsat when ice is present in ground 
                                  ZWSAT1,                          &
!                                              Wsat when ice is present in surface
!                                              ground layer
                                  ZWWILT,                         &
!                                              Wwilt when ice is present in ground
                                  ZC1SAT  
!                                              C1sat scaled due to soil ice
!
!
!                               Thermal conductivity option:                   
!                               Johansen (1975) parameters (as presented by Peters-
!                               Lidard, 1998, JAS). Used to compute CG.
!
REAL, DIMENSION(SIZE(PVEG)) ::  ZFROZEN2, ZUNFROZEN2, ZCONDSAT, ZSATDEG, ZKERSTEN, &
                                  ZCOND, ZHCAP  
!                               ZFROZEN2   = fraction of total soil layer frozen
!                               ZUNFROZEN2 = unfrozen fraction available to liquid
!                               ZCONDSAT   = saturated conductivity (water)
!                               ZSATDEG    = degree of saturation
!                               ZKERSTEN   = Kersten number
!                               ZCOND      = soil thermal conductivity (explicitly
!                                            includes soil, water and ice)
!                               ZHCAP      = Soil heat capacity
!
REAL, DIMENSION(SIZE(PVEG)) :: ZWG2
!                              ZWG2 = adjusted root-zone soil water content
!
REAL, DIMENSION(SIZE(PVEG)) :: ZCF !heat capacity of the flood
REAL, DIMENSION(SIZE(PVEG)) :: ZFF !Fraction of floodplain at the surface without snow
!
INTEGER                 :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SOIL',0,ZHOOK_HANDLE)
ZWWILT (:)   = 0.
!
ZFROZEN2(:)  = 0.
ZUNFROZEN2(:)= 0.
ZCONDSAT(:)  = 0.
ZSATDEG(:)   = 0.
ZKERSTEN(:)  = 0.
ZCOND(:)     = 0.
ZHCAP(:)     = 0.
!
ZLAMS(:)     = 0.
ZX(:)        = 0.
ZCW1MAX(:)   = 0.
ZY1(:)       = 0.
ZX2(:)       = 0.
ZY2(:)       = 0.
ZLYMY1(:)    = 0.
ZZA   (:)    = 0.
ZZB   (:)    = 0.
ZDELTA(:)    = 0.
ZA    (:)    = 0.
ZB    (:)    = 0.
!
ZCF(:)       = XUNDEF
!
PCS(:)       = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       1.     FROZEN WATER FRACTION IN THE SOIL
!               ---------------------------------
!
PFROZEN1(:) = 0.
WHERE (PEK%XWGI(:,1) + PEK%XWG(:,1) .NE. 0.) 
  PFROZEN1(:) = PEK%XWGI(:,1) / (PEK%XWGI(:,1) + PEK%XWG(:,1))
END WHERE
!
DO JJ=1,SIZE(KK%XWSAT,1)
!
  ZWSAT(JJ)    = MAX(KK%XWSAT(JJ,1) - PEK%XWGI(JJ,2),XWGMIN)
!
  ZWSAT1(JJ)   = MAX(KK%XWSAT(JJ,1) - PEK%XWGI(JJ,1),XWGMIN)
!
  ZWWILT(JJ)   = KK%XWWILT(JJ,1) * (ZWSAT1(JJ) / KK%XWSAT(JJ,1))
!
ENDDO
!-------------------------------------------------------------------------------
!
!*       2.     THE HEAT CAPACITY OF BARE-GROUND
!               --------------------------------
!
IF(IO%CSCOND == 'NP89')THEN  
!
!                                      Actually, all the 'C' coefficients in
!                                      ISBA do not represent heat capacities,
!                                      but rather the inverse.  So in the
!                                      following formulation, CG is large
!                                      when W2 is small, thus leading to small
!                                      values for the heat capacity.  In other
!                                      words, a small amount of energy will
!                                      result in great temperature variations
!                                      (for dry soils).
!
!
! Now calculate the thermal inertia of the soil weighted
! by soil ice content (including the soil ice thermal inertia):
!
  DMI%XCG(:) = (1.-PEK%XWGI(:,2)) * KK%XCGSAT(:) * ( ZWSAT(:)/PEK%XWG(:,2) ) **( 0.5*KK%XBCOEF(:,1)/LOG(10.) )  &
              +  PEK%XWGI(:,2)  * 2. * SQRT(XPI/(XCONDI*XCI*XRHOLI*XDAY))  
!
!
ELSE
!
!                                      Method of Johansen (1975) as presented by 
!                                      Peters-Lidard (JAS: 1998) for thermal
!                                      Conductivity of soil. Explicit calculation for
!                                      now (as opposed to implicit method of
!                                      Noilhan and Planton 1989). NP89 uses the
!                                      method of McCumber and Pielke (1981)
!                                      with parameters of Clapp and Hornberger (1978).
!
  DO JJ=1,SIZE(PEK%XWG,1)
!
! Total fraction of soil frozen:
!
    ZFROZEN2(JJ)   = PEK%XWGI(JJ,2)/(PEK%XWGI(JJ,2) + PEK%XWG(JJ,2))
!
! Unfrozen fraction:
!
    ZUNFROZEN2(JJ) = (1.0-ZFROZEN2(JJ))*KK%XWSAT(JJ,1)
!
! Saturated thermal conductivity:
!
    ZCONDSAT(JJ)   = (KK%XCONDSLD(JJ,1)**(1.0-KK%XWSAT(JJ,1)))* (XCONDI**(KK%XWSAT(JJ,1)-ZUNFROZEN2(JJ)))*   &
                      (XCONDWTR**ZUNFROZEN2(JJ))  
!
! Degree of saturation of soil:
!
    ZSATDEG(JJ)   = MAX(0.1, (PEK%XWGI(JJ,2)+PEK%XWG(JJ,2))/KK%XWSAT(JJ,1))
!
! Kersten number:
!
    ZKERSTEN(JJ)  = LOG10(ZSATDEG(JJ)) + 1.0
!
! Put in a smooth transition from thawed to frozen soils:
! simply linearly weight Kersten number by frozen fraction 
! in soil:
!
    ZKERSTEN(JJ)  = (1.0-ZFROZEN2(JJ))*ZKERSTEN(JJ) +           &
                         ZFROZEN2(JJ) *ZSATDEG (JJ)  
!
! Thermal conductivity of soil:
!
    ZCOND(JJ)     = ZKERSTEN(JJ)*(ZCONDSAT(JJ)-KK%XCONDDRY(JJ,1)) + KK%XCONDDRY(JJ,1)
!
! Heat capacity of soil:
!
    ZHCAP(JJ)     = (1.0-KK%XWSAT(JJ,1)) * KK%XHCAPSOIL(JJ,1) +     &
                         PEK%XWG (JJ,2) * XCL * XRHOLW   +     &
                         PEK%XWGI(JJ,2) * XCI * XRHOLI       
!
! Explicit CG calculation:
!
    DMI%XCG(JJ)       = 2.*SQRT(XPI/ZCOND(JJ)/ZHCAP(JJ)/XDAY)
!
  ENDDO
!
ENDIF
!
!                                              Cg must be smaller than 2.E-5
!
DMI%XCG(:) = MIN( DMI%XCG(:), IO%XCGMAX )
!
!-------------------------------------------------------------------------------
!
!*       4.     THE HEAT CAPACITY OF THE SNOW AND FLOOD
!               ---------------------------------------
!
WHERE (KK%XFF(:) > 0.)                                                 
       ZCF(:) = 2.0 * SQRT( XPI/(XCONDWTR*XRHOLW*XCL*XDAY) )
END WHERE  
!
IF(PEK%TSNOW%SCHEME == 'D95' .OR. (PEK%TSNOW%SCHEME == 'EBA' .AND. IO%LGLACIER) )THEN
!
   WHERE (PEK%XPSN(:) > 0.)
      ZLAMS(:) = XCONDI * (PEK%TSNOW%RHO(:,1)/XRHOLW)**1.885              ! first calculate the
!                                                                   ! conductivity of snow
      PCS(:)   = 2.0 * SQRT( XPI/(ZLAMS(:)*PEK%TSNOW%RHO(:,1)*XCI*XDAY) )
   END WHERE
!
!-------------------------------------------------------------------------------
!
!*      5.      GRID-AVERAGED HEAT CAPACITY
!               ---------------------------
!
! With contribution from the ground, vegetation, flood and snow areas
! for composite (Force-Restore) snow scheme option:
!
   DMI%XCT(:) = 1. / ( (1.-PVEG(:))*(1.-PEK%XPSNG(:)-KK%XFFG(:)) / DMI%XCG(:)     &
                      +  PVEG(:)   *(1.-PEK%XPSNV(:)-KK%XFFV(:)) / PEK%XCV(:)     &
                      +                KK%XFF(:)                 / ZCF(:)     &
                      +                PEK%XPSN(:)               / PCS(:)     )  

!
ELSE
!
  DO JJ=1,SIZE(PVEG)
!
    ZFF  (JJ) = PVEG(JJ)*PFFV_NOSNOW(JJ) + (1.-PVEG(JJ))*PFFG_NOSNOW(JJ)
!
! With contribution from the ground and vegetation for explicit
! (ISBA-ES) snow scheme option:
!
     DMI%XCT(JJ) = 1. / ( (1.-PVEG(JJ))*(1.-PFFG_NOSNOW(JJ)) / DMI%XCG(JJ)     &
                      +  PVEG(JJ)      *(1.-PFFV_NOSNOW(JJ)) / PEK%XCV(JJ)     &
                      +  ZFF (JJ)                            / ZCF(JJ)     )  
  ENDDO
!
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!*      6.      COEFFICIENT C1
!               --------------
!                                      Scale the C1SAT coefficient as a function 
!                                      of the soil ice content
!
ZC1SAT(:) = PK%XC1SAT(:)*SQRT(ZWSAT1(:)/KK%XWSAT(:,1))
!
!
!                                      The coefficient C1 is calculated two
!                                      different ways depending on the humidity
!                                      of the soil
!
WHERE (PEK%XWG(:,1) > ZWWILT(:))
!                                    ! First situation:  humid soil
!                                      Then the calculation follows eq. (19)
!                                      of Noilhan and Planton(1989)
!
   DMI%XC1(:)    = ZC1SAT(:) * ( ZWSAT1(:)/PEK%XWG(:,1) )**( 0.5*KK%XBCOEF(:,1) + 1 )
!
END WHERE
!
!
!                                     Calculate C1 coefficient for dry soil.
!                                     The default option is the continuous
!                                     formulation of Giard and Bazile. The
!                                     alternate approach is a discontinuous
!                                     formulation by Giordani (1993) and
!                                     Braud et al. (1993). This method
!                                     is perhaps more accurate from a physical
!                                     standpoint, as it is an explicit function
!                                     of temperature, whereas the continuous method
!                                     assumes a constant temperature. 
!
IF(IO%CC1DRY=='GB93')THEN
!
  DO JJ=1,SIZE(PEK%XWG,1)
!  
    IF (PEK%XWG(JJ,1) <= ZWWILT(JJ)) THEN
!
!                                   ! Second situation: dry soil
!                                      We use the Gaussian formulation of
!                                      Giordanni (1993) and Braud et al. (1993)
!
!* maximum of C1 curve (computed with true Wwilt)
!
       ZCW1MAX(JJ)    = ( 1.19*ZWWILT(JJ)-5.09 )*PEK%XTG(JJ,1)  + (-146.4*ZWWILT(JJ)+1786.)
!
!* Giordanni (1993) and Braud et al. (1993)
!
       ZA(JJ)         =   (-1.815E-2*PEK%XTG(JJ,1)+6.41)*ZWWILT(JJ) + (6.5E-3*PEK%XTG(JJ,1)-1.4)  
       ZB(JJ)         = ZA(JJ)*ZWWILT(JJ)
       ZDELTA(JJ)     = ( ZB(JJ)*ZB(JJ) )  / ( 2.*LOG( ZCW1MAX(JJ) ) )  
!
       DMI%XC1(JJ) = ZCW1MAX(JJ)*(1. - 2.*PVEG(JJ)*( 1.-PVEG(JJ) ))             &
                  *EXP( -(PEK%XWG(JJ,1)-ZB(JJ))*(PEK%XWG(JJ,1)-ZB(JJ)) / (2.*ZDELTA(JJ)) )  
!
    ENDIF
!
  ENDDO
!
ELSE
!
  DO JJ=1,SIZE(PEK%XWG,1)
!
    IF (PEK%XWG(JJ,1) <= ZWWILT(JJ)) THEN
!
!* maximum of C1 curve (computed with true Wwilt)
!
       ZCW1MAX(JJ)    = ( 1.19*ZWWILT(JJ)-5.09 )*PEK%XTG(JJ,1)  + (-146.4*ZWWILT(JJ)+1786.)
!
!* C1 value at Wg = zero
!
       ZY1(JJ) = 10.
!
!* C1 value at Wg = wwilt
!
       ZX2(JJ) = ZWWILT(JJ)
       ZY2(JJ) = ZC1SAT(JJ)*(ZWSAT1(JJ)/ZWWILT(JJ))**( 0.5*KK%XBCOEF(JJ,1) + 1)
!
!* correction of maximum of C1 curve for frozen soils
!
       ZCW1MAX(JJ)    = MAX(MAX(ZCW1MAX(JJ),ZY2(JJ)),ZY1(JJ))
!
!* Giard-Bazile formulation (resolution of a second order equation)
!
       ZLYMY1(JJ) =   LOG( ZCW1MAX(JJ)/ZY1(JJ))
       ZZA   (JJ) = - LOG( ZY2    (JJ)/ZY1(JJ))
       ZZB   (JJ) = 2. * ZX2(JJ)    * ZLYMY1(JJ)
       ZDELTA(JJ) = 4. * (ZLYMY1(JJ)+ZZA(JJ)) * ZLYMY1(JJ) * ZX2(JJ)**2
!
       ZA    (JJ) = (-ZZB(JJ)+SQRT(ZDELTA(JJ))) / (2.*ZZA(JJ))
!
       ZB    (JJ) = ZA(JJ)**2 / ZLYMY1(JJ)
!
!
       DMI%XC1(JJ) = ZCW1MAX(JJ) * EXP( - (PEK%XWG(JJ,1)-ZA(JJ))**2 / ZB(JJ) )
!
    ENDIF
!
  ENDDO
!
ENDIF
!-------------------------------------------------------------------------------
!
!*      6.      COEFFICIENT C2
!               --------------
! Including vertical diffusion limiting factor for surface soil ice:
!
IF(IO%CKSAT=='SGH' .OR. IO%CKSAT=='EXP')THEN
!
! Adjusted root-zone soil water content
!
  DO JJ=1,SIZE(PEK%XWG,1)
     ZWG2(JJ)=PEK%XWG(JJ,2)*(PK%XCONDSAT(JJ,2)/PK%XCONDSAT(JJ,1))**(1./(2.*KK%XBCOEF(JJ,1)+3))
  ENDDO
  ZWG2(:)=MAX(ZWG2(:),XWGMIN)
!
ELSE
!
   ZWG2(:)=PEK%XWG(:,2)
!
ENDIF
!
DO JJ=1,SIZE(ZWSAT)
!
!Including vertical diffusion limiting factor for surface soil ice:
!
  DMI%XC2(JJ) = (PK%XC2REF(JJ)*ZWG2(JJ) / ( ZWSAT(JJ)-ZWG2(JJ) + 0.01 ))           &
              *(1.0-(PEK%XWGI(JJ,1)/(KK%XWSAT(JJ,1)-XWGMIN)))  
!
!-------------------------------------------------------------------------------
!
!*      7.      EQUILIBRIUM VOLUMETRIC WATER CONTENT WGEQ
!               -----------------------------------------
!
  ZX(JJ) = ZWG2(JJ)/ZWSAT(JJ)
!
  DMI%XWGEQ(JJ) = ZWG2(JJ) - ZWSAT(JJ)*KK%XACOEF(JJ) *  ZX(JJ)**KK%XPCOEF(JJ)           &
                               *( 1.-EXP(KK%XPCOEF(JJ)*8.*LOG(ZX(JJ))))  
!
ENDDO
!-------------------------------------------------------------------------------
!
!*      8.      SPECIAL CASE OF POLAR REGIONS
!               -----------------------------
!
IF (LPHYSDOMC) THEN
   DMI%XCT(:) = 9.427757E-6   ! corresponds to a density of 350kg/m3 for snow
ENDIF        
IF (LHOOK) CALL DR_HOOK('SOIL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOIL
