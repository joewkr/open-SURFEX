!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
 SUBROUTINE ALBEDO_RS14(PZENITH,PWIND,PDIR_ALB,PSCA_ALB)
!     ##################################################################
!
!!****  *ALBEDO_RS14*  
!!
!!    PURPOSE
!!    -------
!       computes the direct & diffuse albedo over open water
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!	R. Séférian           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2014
!                   05/2014 R. Séférian & B. Decharme :: Adaptation to spectral
!                   computation for diffuse and direct albedo
!                   09/2014 R. Séférian & B. Sunghye :: Adaptation to spectral
!                   bands compatible with 6-bands RRTM radiative code
!       
!-------------------------------------------------------------------------------
!
!*           DECLARATIONS
!            ------------
!
USE MODD_ALBEDO_RS14_PAR
USE MODD_CSTS, ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH                  ! zenithal angle (radian)
REAL, DIMENSION(:), INTENT(IN)  :: PWIND                      ! surface wind (m s-1)
REAL, DIMENSION(:), INTENT(OUT) :: PDIR_ALB             ! direct  ocean surface albedo
REAL, DIMENSION(:), INTENT(OUT) :: PSCA_ALB             ! diffuse ocean surface albedo
!
!*      0.2    declarations of local variables
!              -------------------------
!
REAL, DIMENSION(SIZE(PZENITH))               :: ZCHL             ! surface chlorophyll
REAL, DIMENSION(SIZE(PZENITH))               :: ZDIR_ALB    ! direct  ocean surface albedo (spectral)
REAL, DIMENSION(SIZE(PZENITH))               :: ZSCA_ALB    ! diffuse ocean surface albedo (spectral)
!
INTEGER                         :: JI, JWL                                                      ! indexes
REAL                            :: ZWL                                                               ! input parameter: wavelength and diffuse/direct fraction of light
REAL:: ZSIG, ZREFM, ZXX2, ZR00, ZRR0, ZRRR                          ! computation variables
REAL:: ZR22, ZUE, ZUE2, ZR11DF, ZALBT, ZFWC                     ! computation variables
REAL:: ZCHLABS, ZAW, ZBW, ZAP, ZYLMD, ZBP550                ! computation variables
REAL:: ZBBP, ZNU, ZHB                                                                     ! computation variables
REAL:: ZCOSZEN                                                                                  ! Cosine of the zenith solar angle
REAL:: ZR11, ZRW, ZRWDF, ZRDF                                                 ! 4 components of the OSA

! 
REAL            :: ZWORK                                                                        ! dummy variable
! 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_RS14',0,ZHOOK_HANDLE)
!
! Initiliazing :
!
PDIR_ALB(:) = 0. 
PSCA_ALB(:) = 0. 
!
ZDIR_ALB(:) = 0. 
ZSCA_ALB(:) = 0.
!
!
ZCHL(:) = 0.05 ! averaged global values for surface chlorophyll
               ! need to include bgc coupling in earth system model configuration
!
DO JWL=1,NNWL           ! loop over the wavelength
!
  DO JI=1,SIZE(PZENITH)   ! loop over the grid points
  
    !---------------------------------------------------------------------------------
    ! 0- Compute baseline values
    !---------------------------------------------------------------------------------
    ! Get refractive index for the correspoding wavelength
    ZWL=XAKWL(JWL)
    ZREFM= XAKREFM(JWL)
    !
    ! compute the cosine of the solar zenith angle
    ZCOSZEN = MAX(COS(PZENITH(JI)),0.)
    ! Compute sigma derived from wind speed (Cox & Munk reflectance model)
    ZSIG=SQRT(0.003+0.00512*PWIND(JI))
    !
    !---------------------------------------------------------------------------------
    ! 1- Compute direct surface albedo (ZR11)
    !---------------------------------------------------------------------------------
    ZXX2=SQRT(1.0-(1.0-ZCOSZEN**2)/ZREFM**2)
    ZRR0=0.50*(((ZXX2-ZREFM*ZCOSZEN)/(ZXX2+ZREFM*ZCOSZEN))**2 +((ZCOSZEN-ZREFM*ZXX2)/(ZCOSZEN+ZREFM*ZXX2))**2)
    ZRRR=0.50*(((ZXX2-1.34*ZCOSZEN)/(ZXX2+1.34*ZCOSZEN))**2 +((ZCOSZEN-1.34*ZXX2)/(ZCOSZEN+1.34*ZXX2))**2)
    ZR11=ZRR0-(0.0152-1.7873*ZCOSZEN+6.8972*ZCOSZEN**2-8.5778*ZCOSZEN**3+4.071*ZSIG-7.6446*ZCOSZEN*ZSIG) &
        & * EXP(0.1643-7.8409*ZCOSZEN-3.5639*ZCOSZEN**2-2.3588*ZSIG+10.0538*ZCOSZEN*ZSIG)*ZRR0/ZRRR
    ! 
    !---------------------------------------------------------------------------------
    ! 2- Compute surface diffuse albedo (ZRDF)
    !---------------------------------------------------------------------------------
    ! Diffuse albedo from Jin et al., 2006 + estimation from diffuse fraction of
    ! light (relying later on AOD)
    ! old: ZRDF=-0.1482-0.012*ZSIG+0.1609*ZREFM-0.0244*ZSIG*ZREFM ! surface diffuse (Eq 5a)
    ZRDF = -0.1479 + 0.1502*ZREFM - 0.0176*ZSIG*ZREFM      ! surface diffuse (Eq 5b)
    !
    !---------------------------------------------------------------------------------
    ! *- Determine absorption and backscattering
    ! coefficients to determine reflectance below the surface (Ro) once for all
    !
    ! *.1- Absorption by chlorophyll
    ZCHLABS= XAKACHL(JWL) 
    ! *.2- Absorption by seawater 
    ZAW= XAKAW3(JWL) 
    ! *.3- Backscattering by seawater
    ZBW= XAKBW(JWL) 
    ! *.4- Backscattering by chlorophyll
    ZYLMD = EXP(0.014*(440.0-ZWL))
    ZWORK= EXP(LOG(ZCHL(JI))*0.65)
    ZAP = 0.06*ZCHLABS*ZWORK +0.2*(XAW440+0.06*ZWORK)*ZYLMD
    ZBP550 = 0.416 * EXP(LOG(ZCHL(JI))*0.766)
   !
    IF ( ZCHL(JI) > 2. ) THEN
      ZNU=0.
    ELSE
      IF ( ZCHL(JI) > 0.02 ) THEN
        ZWORK=LOG10(MAX(MIN(ZCHL(JI),2.),0.02))
        ZNU=0.5*(ZWORK-0.3)
        ZBBP=(0.002+0.01*(0.5-0.25*ZWORK)*(ZWL/550.)**ZNU)*ZBP550
      ELSE
        ZBBP=0.019*(550./ZWL)*ZBP550       !ZBBPf=0.0113 at chl<=0.02
      ENDIF
    ENDIF
    !
    ! Morel-Gentili(1991), Eq (12)
    ! ZHB=h/(h+2*ZBBPf*(1.-h))        
    ZHB=0.5*ZBW/(0.5*ZBW+ZBBP)
    !
    !---------------------------------------------------------------------------------
    ! 3- Compute direct water-leaving albedo (ZRW)
    !---------------------------------------------------------------------------------
    ! Based on Morel & Gentilli 1991 parametrization
    ZR22=0.48168549-0.014894708*ZSIG-0.20703885*ZSIG**2
    ! Use Morel 91 formula to compute the direct reflectance
    ! below the surface
    ZR00=(0.5*ZBW+ZBBP)/(ZAW+ZAP) *(0.6279-0.2227*ZHB-0.0513*ZHB**2 + (-0.3119+0.2465*ZHB)*ZCOSZEN)
    ! ZRW=ZR00*(1.-ZR22)*(1.-ZR11)/(1.-ZR00*ZR22)
    ZRW=ZR00*(1.-ZR22)/(1.-ZR00*ZR22) ! accurate formulation
    !
    !---------------------------------------------------------------------------------
    ! 4- Compute diffuse water-leaving albedo (ZRWDF)
    !---------------------------------------------------------------------------------
    ! as previous water-leaving computation but assumes a uniform incidence of
    ! shortwave at surface (ue)
    ZUE=0.676               ! equivalent u_unif for diffuse incidence
    ZUE2=SQRT(1.0-(1.0-ZUE**2)/ZREFM**2)
    ZRR0=0.50*(((ZUE2-ZREFM*ZUE)/(ZUE2+ZREFM*ZUE))**2 +((ZUE-ZREFM*ZUE2)/(ZUE+ZREFM*ZUE2))**2)
    ZRRR=0.50*(((ZUE2-1.34*ZUE)/(ZUE2+1.34*ZUE))**2 +((ZUE-1.34*ZUE2)/(ZUE+1.34*ZUE2))**2)
    ZR11DF=ZRR0-(0.0152-1.7873*ZUE+6.8972*ZUE**2-8.5778*ZUE**3+4.071*ZSIG-7.6446*ZUE*ZSIG) &
          & * EXP(0.1643-7.8409*ZUE-3.5639*ZUE**2-2.3588*ZSIG+10.0538*ZUE*ZSIG)*ZRR0/ZRRR
    ! Use Morel 91 formula to compute the diffuse
    ! reflectance below the surface
    ZR00=(0.5*ZBW+ZBBP)/(ZAW+ZAP) *(0.6279-0.2227*ZHB-0.0513*ZHB**2 + (-0.3119+0.2465*ZHB)*ZUE)
    ZRWDF=ZR00*(1.-ZR22)*(1.-ZR11DF)/(1.-ZR00*ZR22)
    !
    ! original : correction for foam Monahanand and Muircheartaigh (1980) Eq 16-17
    ! new: Salisbury 2014 eq(2) at 37GHz, value in fraction
    ZFWC=3.97e-4*PWIND(JI)**(1.59) 
    ! has to be update once we have information from wave model (discussion with G. Madec)
    !
    ! --------------------------------------------------------------------
    !  *- OSA estimation
    ! --------------------------------------------------------------------
    ! partitionning direct and diffuse albedo
    !
    ZDIR_ALB(JI) = ZDIR_ALB(JI) + XFRWL(JWL) *((1.-ZFWC) * (ZR11+ZRW  ) + ZFWC*XRWC(JWL))
    ZSCA_ALB(JI) = ZSCA_ALB(JI) + XFRWL(JWL) *((1.-ZFWC) * (ZRDF+ZRWDF) + ZFWC*XRWC(JWL))
    !
    ENDDO ! end of the loop over grid points
   !
ENDDO ! ending loop over wavelengths
!
! --------------------------------------------------------------------
!  *- OSA estimation
! --------------------------------------------------------------------
!
PDIR_ALB(:)=ZDIR_ALB(:)
PSCA_ALB(:)=ZSCA_ALB(:)
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_RS14',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ALBEDO_RS14
