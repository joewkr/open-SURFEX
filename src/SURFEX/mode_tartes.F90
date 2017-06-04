!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_TARTES

!##########################
!
!! *MODE_TARTES*
!!
!! Radiative transfer in snowpack

!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!    M. Lafaysse       * Meteo France *
!!    translated from python codes of G. Picard, Q. Libois, LGGE.
!!
!! Main differences relatively to the python code :
!! ------------------------------------------------
!!
!! For optimization on large domains :
!!     * Loops are inside the subroutines 
!!     * All variables are multi-points (first dimension)
!!     * Loop on points is the last loop
!!     * Number of active or effective layers : argument of subroutines
!!
!! New routine to interpolate ice refractive index on the given wavelengths
!!
!! Wavelengths are assumed to be a parameter of the model and not an argument (if necessary, it will be in surfex namelists)
!!
!! Impurities : the python object is replaced by two variables (density and content). Last dimension represents impurity type.
!! For now : it is just implemented for soot (indice 1).
!!
!! To solve the linear system MX=Y :
!!     * Indices of upper diagonal (PDP) shifted one step right
!!     * Indices of lower diagonal (PDM) shifted one step left
!!
!! In routine INFINITE_MEDIUM_OPTICAL_PARAMETERS add a control IF ABS(PSNOWALB)<UEPSI
!!
!! Add a control of absorbed energy output to avoid occasional numerical problem in infra-red (not used)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    24/07/2013
!!      Matthieu Lafaysse interface with SURFEX 23/08/2013
!--------------------------------------------------------------------------------

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB


 CONTAINS

SUBROUTINE TARTES(PSNOWSSA,PSNOWRHO,PSNOWDZ,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,PSNOWIMP_DENSITY,&
                  PSNOWIMP_CONTENT,PALB,PSW_RAD_DIF,PSW_RAD_DIR,PCOSZEN,KNLVLS_USE,PSNOWALB, &
                  PSNOWENERGY,PSOILENERGY)
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS,XPWAVELENGTHS,XREFICE_R,XREFICE_I,XREFIMP_I,XP_MUDIFF
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWSSA !snow specific surface area (m^2/kg) (npoints,nlayer) 
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWRHO !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWDZ !snow layers thickness (m) (npoints,nlayer)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PALB ! soil/vegetation albedo (npoints,nbands)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PSW_RAD_DIF ! spectral diffuse incident light (W/m^2) (npoints,nbands)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSW_RAD_DIR ! spectral direct incident light (W/m^2) (npoints,nbands)
REAL, DIMENSION(:), INTENT(IN)     :: PCOSZEN ! cosine of zenithal solar angle (npoints)
!
INTEGER, DIMENSION(:), INTENT(IN)  :: KNLVLS_USE ! number of effective snow layers (npoints)
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSNOWALB !(npoints,nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWENERGY !(npoints,nlayer,nbands)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSOILENERGY !(npoints,nbands)
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2)*2,NPNBANDS) :: ZDM,ZD,ZDP !3 diagonals of the matrix
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2)*2,NPNBANDS) :: ZVECTOR_DIR,ZVECTOR_DIF
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZSNOWSSALB !total single scattering albedo (npoints,nlayer,nbands) 
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZSNOWG !asymmetry factor (npoints,nlayer,nbands)
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZSNOWALBEDO! Albedo (npoints,nlayer,nbands)
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands) 
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZG_STAR,ZSSALB_STAR,ZGAMMA1,ZGAMMA2
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZDTAUSTAR !Optical depth of each layer
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZTAUSTAR !Cumulated optical depth
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZGP_DIR,ZGM_DIR ! Gp Gm vectors for direct radiation
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZGP_DIF,ZGM_DIF ! Gp Gm vectors for diffuse radiation
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZXA_DIR,ZXA_DIF,ZXB_DIR,ZXB_DIF,ZXC_DIR,ZXC_DIF,ZXD_DIR,ZXD_DIF
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZEPROFILE_DIR,ZEPROFILE_DIF
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),NPNBANDS) :: ZSOILABS_DIR,ZSOILABS_DIF
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),NPNBANDS) :: ZALB ! same as PALB but possibly modified if snowpack is truncated
!
REAL, DIMENSION(SIZE(PSNOWSSA,1))::ZMUDIFF
!
INTEGER, DIMENSION(SIZE(PSNOWSSA,1),NPNBANDS) :: INLVLS_EFF ! number of effective snow layers
!
INTEGER, DIMENSION(NPNBANDS) :: IMAX_EFF ! maximum number of effective layers over each band
!
INTEGER :: JB,JI,JL !loop counters
INTEGER :: IMAX_USE ! maximum number of layers over the domain
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TARTES',0,ZHOOK_HANDLE)
!
! Initialization
PSNOWALB    = 0.
PSNOWENERGY = 0.
PSOILENERGY = 0.
ZALB        = PALB
!
ZMUDIFF = XP_MUDIFF !the diffuse incident flux is treated as direct flux at zenithal angle 53deg
!
IMAX_USE = MAXVAL(KNLVLS_USE)
!
!1 compute optical properties for each wavelength
 CALL SINGLE_SCATTERING_OPTICAL_PARAMETERS(PSNOWSSA,PSNOWRHO,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,     &
                                           PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,KNLVLS_USE,IMAX_USE, &
                                           ZSNOWSSALB,ZSNOWG)

 CALL INFINITE_MEDIUM_OPTICAL_PARAMETERS(ZSNOWSSALB,ZSNOWG,KNLVLS_USE,IMAX_USE,ZSNOWALBEDO,ZKESTAR, &
                                         ZG_STAR,ZSSALB_STAR,ZGAMMA1,ZGAMMA2)

! 2 computation on every wavelength and layer of the optical depth
 CALL TAUSTAR_VECTOR(PSNOWSSA,PSNOWRHO,PSNOWDZ,ZSNOWSSALB,ZSNOWG,ZKESTAR,KNLVLS_USE,IMAX_USE, &
                     ZDTAUSTAR,ZTAUSTAR)

! estimate the effective layers for absorption
 CALL ESTIMATE_EFFECTIVE_LAYER_NUMBER(ZKESTAR,ZDTAUSTAR,KNLVLS_USE,IMAX_USE,INLVLS_EFF,IMAX_EFF)

!3 solve the radiative transfer for each wavelength
!3.1 Compte G+ and G- vectors for direct and diffuse radiations
 CALL GP_GM_VECTORS(ZSNOWSSALB,ZKESTAR,ZG_STAR,ZSSALB_STAR,ZGAMMA1,ZGAMMA2,PCOSZEN,PSW_RAD_DIR, &
                    INLVLS_EFF,IMAX_EFF,ZGP_DIR,ZGM_DIR)
 CALL GP_GM_VECTORS(ZSNOWSSALB,ZKESTAR,ZG_STAR,ZSSALB_STAR,ZGAMMA1,ZGAMMA2,ZMUDIFF,PSW_RAD_DIF, &
                    INLVLS_EFF,IMAX_EFF,ZGP_DIF,ZGM_DIF)
!
!3.2 If the snowpack has been truncated, add a thick layer (optical) and force soil albedo to 1
DO JB = 1,NPNBANDS
  DO JI = 1,SIZE(KNLVLS_USE)
    IF ( INLVLS_EFF(JI,JB)<KNLVLS_USE(JI) ) THEN
      ZDTAUSTAR(JI,INLVLS_EFF(JI,JB)+1,JB) = 30. / ZKESTAR(JI,INLVLS_EFF(JI,JB)+1,JB)
      ZALB     (JI,JB) = 1.
    END IF
  END DO
END DO
!
!3.3 Compute the matrix and vectors
 CALL TWO_STREAM_MATRIX(ZSNOWALBEDO,ZALB,ZKESTAR,ZDTAUSTAR,INLVLS_EFF,IMAX_EFF,ZDM,ZD,ZDP)
 CALL TWO_STREAM_VECTOR(ZSNOWALBEDO,ZALB,ZDTAUSTAR,ZTAUSTAR,ZGM_DIR,ZGP_DIR,PCOSZEN,INLVLS_EFF,IMAX_EFF,ZVECTOR_DIR)
 CALL TWO_STREAM_VECTOR(ZSNOWALBEDO,ZALB,ZDTAUSTAR,ZTAUSTAR,ZGM_DIF,ZGP_DIF,ZMUDIFF,INLVLS_EFF,IMAX_EFF,ZVECTOR_DIF)
!
! DO JB=1,NPNBANDS,30
!     PRINT*,"band ",JB
!     PRINT*,ZDM(:,:,JB)
!     PRINT*,ZD(:,:,JB)
!     PRINT*,ZDP(:,:,JB)
!     PRINT*,ZVECTOR_DIR(:,:,JB)    
!     PRINT*,ZVECTOR_DIF(:,:,JB)    
! END DO
!
!3.4 solve the system
 CALL SOLVES_TWO_STREAM2(ZDM,ZD,ZDP,ZVECTOR_DIR,ZVECTOR_DIF,ZSNOWALBEDO,PSW_RAD_DIR,PSW_RAD_DIF,INLVLS_EFF, &
                         IMAX_EFF,ZXA_DIR,ZXA_DIF,ZXB_DIR,ZXB_DIF,ZXC_DIR,ZXC_DIF,ZXD_DIR,ZXD_DIF)
!
! DO JB=1,NPNBANDS,30
!     PRINT*,"solution band ",JB
!     PRINT*,ZXA_DIR(:,:,JB)
!     PRINT*,ZXA_DIF(:,:,JB)  
! END DO
!
!4 Diagnostics
!4.1 Albedo
 CALL SNOWPACK_ALBEDO(ZXC_DIR(:,1,:),ZXC_DIF(:,1,:),ZXD_DIR(:,1,:),ZXD_DIF(:,1,:),  &
                     ZGP_DIR(:,1,:),ZGP_DIF(:,1,:),PCOSZEN,ZMUDIFF,PSW_RAD_DIR,    &
                     PSW_RAD_DIF,PSNOWALB)
!
!4.2 Energy profile
 CALL ENERGY_PROFILE(ZXA_DIR,ZXB_DIR,ZXC_DIR,ZXD_DIR,ZKESTAR,ZDTAUSTAR,ZTAUSTAR,ZGM_DIR,ZGP_DIR,PCOSZEN, &
                     INLVLS_EFF,IMAX_EFF,ZEPROFILE_DIR)
 CALL ENERGY_PROFILE(ZXA_DIF,ZXB_DIF,ZXC_DIF,ZXD_DIF,ZKESTAR,ZDTAUSTAR,ZTAUSTAR,ZGM_DIF,ZGP_DIF,ZMUDIFF, &
                     INLVLS_EFF,IMAX_EFF,ZEPROFILE_DIF)
!
DO JB = 1,NPNBANDS
  DO JL = 1,SIZE(PSNOWSSA,2)
    WHERE ( JL<=INLVLS_EFF(:,JB) )
      PSNOWENERGY(:,JL,JB) = PSW_RAD_DIR(:,JB) * ZEPROFILE_DIR(:,JL,JB) + &
                             PSW_RAD_DIF(:,JB) * ZEPROFILE_DIF(:,JL,JB)
        ENDWHERE
    END DO
END DO
!
!4.3 Soil absorption
 CALL SOIL_ABSORPTION(ZXA_DIR,ZXB_DIR,ZKESTAR,ZDTAUSTAR,ZTAUSTAR,ZGM_DIR,PCOSZEN,PALB,INLVLS_EFF,ZSOILABS_DIR)
 CALL SOIL_ABSORPTION(ZXA_DIF,ZXB_DIF,ZKESTAR,ZDTAUSTAR,ZTAUSTAR,ZGM_DIF,ZMUDIFF,PALB,INLVLS_EFF,ZSOILABS_DIF)
!
PSOILENERGY = PSW_RAD_DIR * ZSOILABS_DIR + PSW_RAD_DIF * ZSOILABS_DIF
!
IF (LHOOK) CALL DR_HOOK('TARTES',1,ZHOOK_HANDLE)
!
END SUBROUTINE TARTES
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE INIT_TARTES()
!
!In surfex this routine has to been called only once (Crocus init).
!
IMPLICIT NONE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_TARTES',0,ZHOOK_HANDLE)
!
 CALL REFICE() ! Interpolate refractive index for pure ice on the prescribed wavelengths
 CALL REFSOOT_IMAG() ! Compute refractive index of soot according to wavelengths from Chang (1990)
!
IF (LHOOK) CALL DR_HOOK('INIT_TARTES',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TARTES
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE REFICE()
!
! Interpolate refractive index for pure ice on the prescribed wavelengths
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS,XPWAVELENGTHS,XPWAVELENGTHS_M,XREFICE_R,XREFICE_I, &
                             NPNBANDS_REF,XPWAVELENGTHS_REF,XPREFICE_R,XPREFICE_I,       & 
                             XREFICE_NORM,XGINF,XCONST_C
USE MODD_CSTS, ONLY: XPI, XRHOLI                           
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE                             
!
! Log of PPWAVELENGTHS PPWAVELENGTHS_REF PPREFICE_I for interpolation
REAL, DIMENSION(NPNBANDS)     :: ZLOG_WL
REAL, DIMENSION(NPNBANDS_REF) :: ZLOG_WL_REF
REAL, DIMENSION(NPNBANDS_REF) :: ZLOG_REFICE_I
!
INTEGER :: JB,JBREF !loop counters (wl band)
!
LOGICAL :: GINF !logical for interpolation
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('REFICE',0,ZHOOK_HANDLE)
!
ZLOG_WL       = LOG(XPWAVELENGTHS)
ZLOG_WL_REF   = LOG(XPWAVELENGTHS_REF)
ZLOG_REFICE_I = LOG(XPREFICE_I)

! Interpolate retractive index
DO JB = 1,NPNBANDS
  !
  GINF = .TRUE.
  !
  DO JBREF = 1,NPNBANDS_REF
    !
    IF ( XPWAVELENGTHS_REF(JBREF)>XPWAVELENGTHS(JB) ) THEN
      !
      IF ( JBREF<2 ) CALL ABOR1_SFX("FATAL ERROR INIT_TARTES (interpolation of refractive indexs)")
      !
      GINF = .FALSE.
      !
      XREFICE_R(JB) = ( (XPWAVELENGTHS    (JB)    - XPWAVELENGTHS_REF(JBREF-1)) * XPREFICE_R(JBREF)   +   &
                        (XPWAVELENGTHS_REF(JBREF) - XPWAVELENGTHS    (JB)     ) * XPREFICE_R(JBREF-1) ) / &
                       ( XPWAVELENGTHS_REF(JBREF) - XPWAVELENGTHS_REF(JBREF-1) )
      XREFICE_I(JB) = EXP( ( (ZLOG_WL    (JB)    - ZLOG_WL_REF(JBREF-1)) * ZLOG_REFICE_I(JBREF)   +   &
                             (ZLOG_WL_REF(JBREF) - ZLOG_WL    (JB)     ) * ZLOG_REFICE_I(JBREF-1) ) / &
                            ( ZLOG_WL_REF(JBREF) - ZLOG_WL_REF(JBREF-1) ) )
      !
      XREFICE_NORM(JB) = XREFICE_R(JB) - 1.3 !factor in eq 72-73-74
      XGINF       (JB) = 0.9751 - 0.105 * XREFICE_NORM(JB) !doc equation 72
      XCONST_C    (JB) = 24. * XPI * XREFICE_I(JB) / ( XRHOLI * XPWAVELENGTHS_M(JB) ) !constant c*SSA in equation 71
      !
      EXIT
      !
    END IF
  !
  END DO
  !
  IF ( GINF ) CALL ABOR1_SFX("FATAL ERROR INIT_TARTES (interpolation of refractive indexs)")
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('REFICE',1,ZHOOK_HANDLE)
!
END SUBROUTINE REFICE
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE REFSOOT_IMAG()

! Compute refractive index of soot according to wavelengths from Chang (1990)

USE MODD_CONST_TARTES, ONLY : NPNBANDS,XPWAVELENGTHS,XREFIMP_I
!
!PPWAVELENGTHS nanometers
!
IMPLICIT NONE  
!
REAL, DIMENSION    (NPNBANDS) :: ZWL_UM ! Wavelengths in micrometers (Chang, 1990 formulas)
REAL, DIMENSION    (NPNBANDS) :: ZINDEX_SOOT_REAL,ZINDEX_SOOT_IMAG ! real and imaginary components of refractive index
 COMPLEX, DIMENSION(NPNBANDS) :: ZINDEX_SOOT !complex refractive index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('REFSOOT_IMAG',0,ZHOOK_HANDLE)
!
ZWL_UM = XPWAVELENGTHS / 1000.
!
ZINDEX_SOOT_REAL = 1.811  + 0.1263*LOG(ZWL_UM) + 0.027 *LOG(ZWL_UM)**2 + 0.0417*LOG(ZWL_UM)**3
ZINDEX_SOOT_IMAG = 0.5821 + 0.1213*LOG(ZWL_UM) + 0.2309*LOG(ZWL_UM)**2 - 0.01  *LOG(ZWL_UM)**3
!
ZINDEX_SOOT = ZINDEX_SOOT_REAL - CMPLX(0,1) * ZINDEX_SOOT_IMAG
!
! absorption cross section of small particles (Bohren and Huffman, 1983)
XREFIMP_I(:,1) = AIMAG( (ZINDEX_SOOT**2-1.) / (ZINDEX_SOOT**2 + 2.) )
!
IF (LHOOK) CALL DR_HOOK('REFSOOT_IMAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE REFSOOT_IMAG
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SHAPE_PARAMETER_VARIATIONS(PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,PSNOWG00,PSNOWY,PSNOWW,PSNOWB)

!compute shape parameter variations as a function of the the refraction index with respect to the value in the visible range.
!These variation equations were obtained for sphere (Light Scattering Media Optics, Kokhanovsky, A., p.61) but should also apply to other shapes in a first approximation.
!see doc Section 2
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS,XREFICE_NORM !number of spectral bands
!
IMPLICIT NONE  
!
REAL, DIMENSION(:,:), INTENT(IN) :: PSNOWG0 !asymmetry parameter of snow grains at refractive index=1.3 and at non absorbing wavelengths (no unit)   (npoints*nlayers)
REAL, DIMENSION(:,:), INTENT(IN) :: PSNOWY0 !Value of y of snow grains at  refractive index=1.3 (no unit) (npoints*nlayers)
REAL, DIMENSION(:,:), INTENT(IN) :: PSNOWW0 !Value of W of snow grains at  refractive index=1.3 (no unit) (npoints*nlayers)
REAL, DIMENSION(:,:), INTENT(IN) :: PSNOWB0 !absorption enhancement parameter of snow grains at refractive index=1.3 and at non absorbing wavelengths (no unit) (npoints*nlayers)
!
! Spectral parameters necessary to compute the asymmetry parameter and single scattering albedo of snow. For now, those parameters do not evolve with time. They depend on shape only
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWG00 !(npoints*nlayers*nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWY !(npoints*nlayers*nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWW !(npoints*nlayers*nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWB !(npoints*nlayers*nbands)
!
INTEGER :: JB !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SHAPE_PARAMETER_VARIATIONS',0,ZHOOK_HANDLE)
!
DO JB = 1,NPNBANDS
  PSNOWG00(:,:,JB) = PSNOWG0(:,:) - 0.38  * XREFICE_NORM(JB) !doc equation 73
  PSNOWB  (:,:,JB) = PSNOWB0(:,:) + 0.4   * XREFICE_NORM(JB) !doc equation 77
  PSNOWW  (:,:,JB) = PSNOWW0(:,:) + 0.17  * XREFICE_NORM(JB) 
  PSNOWY  (:,:,JB) = PSNOWY0(:,:) + 0.752 * XREFICE_NORM(JB) !doc equation 74
END DO
!
IF (LHOOK) CALL DR_HOOK('SHAPE_PARAMETER_VARIATIONS',1,ZHOOK_HANDLE)
!
END SUBROUTINE SHAPE_PARAMETER_VARIATIONS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE IMPURITIES_CO_SINGLE_SCATTERING_ALBEDO(PSNOWSSA,PSNOWIMP_DENSITY,PSNOWIMP_CONTENT, &
                                                  KNLVLS_USE,KMAX_USE,PCOSSALB)
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS,NPNIMP,XPWAVELENGTHS_M,XREFIMP_I
USE MODD_CSTS, ONLY: XPI
!
IMPLICIT NONE
!
!return the spectral absorption of snow due to the impurities
!see doc Section 2.6
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWSSA !snow specific surface area (m^2/kg) (npoints,nlayer) 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE !number of active layers
INTEGER, INTENT(IN)                 :: KMAX_USE !maximum number of active layers over the domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PCOSSALB !co single scattering albedo of impurities
!
REAL,DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2)) :: ZABS_IMP
!
INTEGER :: JIMP !loop counter
INTEGER :: JB, JL,JJ !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('IMPURITIES_CO_SINGLE_SCATTERING_ALBEDO',0,ZHOOK_HANDLE)
!
PCOSSALB = 0.
!
DO JB = 1,NPNBANDS
  DO JIMP = 1,NPNIMP
    DO JL = 1,KMAX_USE
      DO JJ = 1,SIZE(KNLVLS_USE)
        !
        IF ( KNLVLS_USE(JJ)>=JL ) THEN
          !
          ZABS_IMP(JJ,JL)       = -XREFIMP_I(JB,JIMP)
          PCOSSALB(JJ,JL,JB) = PCOSSALB(JJ,JL,JB) + &
                                      12. * XPI / ( XPWAVELENGTHS_M(JB)*PSNOWSSA(JJ,JL) ) * &
                                      PSNOWIMP_CONTENT(JJ,JL,JIMP) / PSNOWIMP_DENSITY(JJ,JL,JIMP) * &
                                      ZABS_IMP(JJ,JL) !doc equation 79
          !
        ENDIF
        !
      ENDDO
    ENDDO
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('IMPURITIES_CO_SINGLE_SCATTERING_ALBEDO',1,ZHOOK_HANDLE)
!
END SUBROUTINE IMPURITIES_CO_SINGLE_SCATTERING_ALBEDO
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SINGLE_SCATTERING_OPTICAL_PARAMETERS(PSNOWSSA,PSNOWRHO,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,     &
                                                PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,KNLVLS_USE,KMAX_USE, &
                                                PSNOWSSALB,PSNOWG)
!
!see doc Section 2.3, 2.5, 2.6
USE MODD_CONST_TARTES, ONLY: NPNBANDS,XGINF,XCONST_C
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWSSA !snow specific surface area (m^2/kg) (npoints,nlayer) 
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE !number of active layers
INTEGER, INTENT(IN)                 :: KMAX_USE !maximum number of active layers over the domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWSSALB !total single scattering albedo (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWG !asymmetry factor (npoints,nlayer,nbands) 
!
!Local variables
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZSNOWG00,ZSNOWY,ZSNOWW,ZSNOWB
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZSNOWCOSSALB ! co- single scattering albedo of pure snow
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2),NPNBANDS) :: ZIMPCOSSALB ! co- single scattering albedo of impurities
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2)) :: ZC,ZPHI
!
INTEGER :: JB,JL,JJ !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SINGLE_SCATTERING_OPTICAL_PARAMETERS',0,ZHOOK_HANDLE)
!
 CALL SHAPE_PARAMETER_VARIATIONS(PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,ZSNOWG00,ZSNOWY,ZSNOWW,ZSNOWB)
!
DO JL = 1,KMAX_USE
  !
  DO JJ =1,SIZE(PSNOWSSA,1)
    !
    IF ( KNLVLS_USE(JJ)>=JL ) THEN
      !
      DO JB = 1,NPNBANDS
        !
        ! calculation of the spectral asymmetry parameter of snow
        ZC(JJ,JL) = XCONST_C(JB) / PSNOWSSA(JJ,JL)
        !    
        PSNOWG(JJ,JL,JB) = XGINF(JB) - ( XGINF(JB)-ZSNOWG00(JJ,JL,JB) ) * EXP( -ZSNOWY(JJ,JL,JB)*ZC(JJ,JL) )
       !
        ! co- single scattering albedo of pure snow
        ZPHI        (JJ,JL)       = 2./3. * ZSNOWB(JJ,JL,JB) / ( 1.-ZSNOWW(JJ,JL,JB) )
        ZSNOWCOSSALB(JJ,JL,JB) = 0.5 * ( 1.-ZSNOWW(JJ,JL,JB) ) * ( 1.-EXP( -ZPHI(JJ,JL)*ZC(JJ,JL) ) ) !doc equation 76
        !    
      ENDDO
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
!adding co- single scattering albedo for impureties
 CALL IMPURITIES_CO_SINGLE_SCATTERING_ALBEDO(PSNOWSSA,PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,&
                                            KNLVLS_USE,KMAX_USE,ZIMPCOSSALB)
!
ZSNOWCOSSALB = ZSNOWCOSSALB + ZIMPCOSSALB
!
!total single scattering albedo
PSNOWSSALB = 1.-ZSNOWCOSSALB
!
IF (LHOOK) CALL DR_HOOK('SINGLE_SCATTERING_OPTICAL_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE SINGLE_SCATTERING_OPTICAL_PARAMETERS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE INFINITE_MEDIUM_OPTICAL_PARAMETERS(PSNOWSSALB,PSNOWG,KNLVLS_USE,KMAX_USE,PSNOWALBEDO,PKESTAR,&
                                              PG_STAR,PSSALB_STAR,PGAMMA1,PGAMMA2)
!return albedo and kestar using Delta-Eddington Approximation (The Delta-Eddington Approximation of Radiative Flux Transfer, Jospeh et al (1976)).  
! Fluxes in the snowpack depend on these 2 quantities
! see doc section 1.4
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS
USE MODD_SNOW_METAMO, ONLY: XUEPSI
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWSSALB !total single scattering albedo (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWG !asymmetry factor (npoints,nlayer,nbands) 
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE !number of active layers
INTEGER, INTENT(IN)                 :: KMAX_USE !maximum number of active layers over the domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSNOWALBEDO ! Albedo (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PG_STAR,PSSALB_STAR,PGAMMA1,PGAMMA2 !(npoints,nlayer,nbands) 
!
INTEGER :: JB,JL,JJ !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INFINITE_MEDIUM_OPTICAL_PARAMETERS',0,ZHOOK_HANDLE)
!
DO JB = 1,NPNBANDS
  !
  DO JL = 1,KMAX_USE
    !
    DO JJ =1,SIZE(PSNOWG,1)
      !
      IF ( KNLVLS_USE(JJ)>=JL ) THEN
        !
        PG_STAR    (JJ,JL,JB) = PSNOWG    (JJ,JL,JB) / ( 1. + PSNOWG(JJ,JL,JB) ) !doc equation 12
        PSSALB_STAR(JJ,JL,JB) = PSNOWSSALB(JJ,JL,JB) * ( 1. - PSNOWG(JJ,JL,JB)**2 ) / &
                                      ( 1. - PSNOWG(JJ,JL,JB)**2 * PSNOWSSALB(JJ,JL,JB) ) !doc equation 16
        !
        ! Jimenez-Aquino, J. and Varela, J. R., (2005)
        PGAMMA1(JJ,JL,JB) =  0.25 * ( 7. - PSSALB_STAR(JJ,JL,JB)*(4.+3.*PG_STAR(JJ,JL,JB)) )      !doc equation 38
        PGAMMA2(JJ,JL,JB) = -0.25 * ( 1. - PSSALB_STAR(JJ,JL,JB)*(4.-3.*PG_STAR(JJ,JL,JB)) )      !doc equation 39
        !
        PKESTAR    (JJ,JL,JB) = SQRT( PGAMMA1(JJ,JL,JB)**2 - PGAMMA2(JJ,JL,JB)**2 )                     !doc equation 42
        PSNOWALBEDO(JJ,JL,JB) = ( PGAMMA1(JJ,JL,JB)-PKESTAR(JJ,JL,JB) ) / PGAMMA2(JJ,JL,JB)  !doc equation 43
        !
        ! Modif M Lafaysse to avoid division by 0
        !NB JJ note that this variable can be negative in infra-red wavelengths (it represents the albedo only in smallest wavelengths
        IF ( ABS(PSNOWALBEDO(JJ,JL,JB))<XUEPSI ) THEN
          PSNOWALBEDO(JJ,JL,JB) = SIGN( XUEPSI, PSNOWALBEDO(JJ,JL,JB) )
        ENDIF
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INFINITE_MEDIUM_OPTICAL_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE INFINITE_MEDIUM_OPTICAL_PARAMETERS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!
SUBROUTINE TAUSTAR_VECTOR(PSNOWSSA,PSNOWRHO,PSNOWDZ,PSNOWSSALB,PSNOWG,PKESTAR,KNLVLS_USE,KMAX_USE,&
                          PDTAUSTAR,PTAUSTAR)
!compute the taustar and dtaustar of the snowpack, the optical depth of each layer and cumulated optical depth
!see doc Section 1.2, 1.8, 2.4
!
USE MODD_CONST_TARTES, ONLY: NPNBANDS,XPMAX_OPTICALDEPTH
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWSSA !snow specific surface area (m^2/kg) (npoints,nlayer) 
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ !snow depth (m) (npoints,nlayer)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWSSALB  !total single scattering albedo (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWG ! asymmetry factor (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands) 
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE !number of active layers
INTEGER, INTENT(IN)                 :: KMAX_USE !maximum number of active layers over the domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDTAUSTAR !Optical depth of each layer
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PTAUSTAR !Cumulated optical depth
!
REAL, DIMENSION(SIZE(PSNOWSSA,1),SIZE(PSNOWSSA,2)) :: ZSIGEXT  ! Extinction coefficient (npoints,nlayer) 
!
INTEGER :: JB,JL !loop counters
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TAUSTAR_VECTOR',0,ZHOOK_HANDLE)
!
!Compute extinction coefficient
ZSIGEXT = PSNOWRHO*PSNOWSSA/2. !doc equation 75
!
DO JB=1,NPNBANDS
  !
  DO JL = 1,KMAX_USE
    !
    WHERE ( KNLVLS_USE>=JL )
      !Optical depth of each layer with delta-eddington variable change, doc equation 15
      ! + optical depth threshold (doc section 1.8)
      PDTAUSTAR(:,JL,JB) = MIN( ZSIGEXT(:,JL) * PSNOWDZ(:,JL) * ( 1.- PSNOWSSALB(:,JL,JB)*PSNOWG(:,JL,JB)**2 ), &
                                XPMAX_OPTICALDEPTH / PKESTAR(:,JL,JB) )
    ENDWHERE
    !
  END DO
  !  
  !Cumulated optical depth
  !First layer
  PTAUSTAR(:,1,JB) = PDTAUSTAR(:,1,JB)
  !Other layers
  DO JL = 2,KMAX_USE
    WHERE ( KNLVLS_USE>=JL )
      PTAUSTAR(:,JL,JB) = PTAUSTAR(:,JL-1,JB) + PDTAUSTAR(:,JL,JB)
    ENDWHERE
  END DO
  !  
END DO

IF (LHOOK) CALL DR_HOOK('TAUSTAR_VECTOR',1,ZHOOK_HANDLE)

END SUBROUTINE TAUSTAR_VECTOR
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE ESTIMATE_EFFECTIVE_LAYER_NUMBER(PKESTAR,PDTAUSTAR,KNLVLS_USE,KMAX_USE,KNLVLS_EFF,KMAX_EFF)
! estimate the number of layers to take into account at each wavelength
!doc section 1.8
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS,XPWAVELENGTHS,XPTAUMAX
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDTAUSTAR !Optical depth of each layer
INTEGER, DIMENSION(:), INTENT(IN)    :: KNLVLS_USE !number of active layers
INTEGER, INTENT(IN)                  :: KMAX_USE !maximum number of active layers over the domain
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(OUT)   :: KMAX_EFF !maximum number of effective layers over the domain (nbands)
!
REAL, DIMENSION(SIZE(PKESTAR,1),SIZE(PKESTAR,2),NPNBANDS) :: ZTAU
LOGICAL, DIMENSION(SIZE(PKESTAR,1)) :: GEFF
INTEGER :: JB,JL !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('ESTIMATE_EFFECTIVE_LAYER_NUMBER',0,ZHOOK_HANDLE)
!
DO JB = 1,NPNBANDS
  !
  KNLVLS_EFF(:,JB) = KNLVLS_USE
  ZTAU    (:,1,JB) = PKESTAR(:,1,JB) * PDTAUSTAR(:,1,JB)
  GEFF = .TRUE.
  !
  DO JL = 2,KMAX_USE
    !
    WHERE ( (KNLVLS_USE>=JL) .AND. GEFF )
      ZTAU(:,JL,JB) = ZTAU(:,JL-1,JB) + PKESTAR(:,JL,JB) * PDTAUSTAR(:,JL,JB)
    ELSEWHERE
      ZTAU(:,JL,JB) = 0.      
    ENDWHERE
    WHERE ( ZTAU(:,JL,JB)>XPTAUMAX )
      KNLVLS_EFF(:,JB) = MAX(1,JL-1)
      GEFF = .FALSE.
    ENDWHERE
    !
  END DO
  !  
  KMAX_EFF(JB) = MAXVAL(KNLVLS_EFF(:,JB))
  !  
END DO

IF (LHOOK) CALL DR_HOOK('ESTIMATE_EFFECTIVE_LAYER_NUMBER',1,ZHOOK_HANDLE)

END SUBROUTINE ESTIMATE_EFFECTIVE_LAYER_NUMBER
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!
SUBROUTINE GP_GM_VECTORS(PSNOWSSALB,PKESTAR,PG_STAR,PSSALB_STAR,PGAMMA1,PGAMMA2,PCOSZEN,PSW_RAD,KNLVLS_EFF,KMAX_EFF,PGP,PGM)
!
!return GP and GM vectors of equations 40/41 and 46/47
! (equations for the downward and upward fluxes in the snowpack for the 2 stream approximation)
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWSSALB !total single scattering albedo (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands) 
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PG_STAR ! asymmetry factor * (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSSALB_STAR
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGAMMA1,PGAMMA2
REAL, DIMENSION(:,:), INTENT(IN)    :: PSW_RAD ! incident radiation (direct or diffuse) (npoints,nbands)
REAL, DIMENSION(:), INTENT(IN)      :: PCOSZEN ! cosine of zenithal solar angle (npoints)
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(IN)   :: KMAX_EFF !maximum number of effective layers over the domain (nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PGP !GP vector (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PGM !GM vector (npoints,nlayer,nbands)
!
REAL :: ZGAMMA3,ZGAMMA4,ZG ! intermediate terms
!
INTEGER :: JB,JL,JJ !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GP_GM_VECTORS',0,ZHOOK_HANDLE)
!
!Init
PGP = 0.
PGM = 0.
!
DO JB = 1,NPNBANDS
  !
  DO JL = 1,KMAX_EFF(JB)
    !
    DO JJ =1,SIZE(PSW_RAD,1)
      !
      IF ( PSW_RAD(JJ,JB)>0. .AND. KNLVLS_EFF(JJ,JB)>=JL ) THEN
        !
        ZGAMMA3 = 0.25 * ( 2. - 3.*PG_STAR(JJ,JL,JB)*PCOSZEN(JJ) ) !doc equation 28
        ZGAMMA4 = 0.25 * ( 2. + 3.*PG_STAR(JJ,JL,JB)*PCOSZEN(JJ) ) !doc equation 27
        ZG = PCOSZEN(JJ)**2 * PSSALB_STAR(JJ,JL,JB) / ( (PKESTAR(JJ,JL,JB)*PCOSZEN(JJ))**2 - 1. ) !factor eq 44-45
        PGP(JJ,JL,JB) = ZG * ( (PGAMMA1(JJ,JL,JB)-1./PCOSZEN(JJ))*ZGAMMA3 + PGAMMA2(JJ,JL,JB)*ZGAMMA4 ) !doc equation 45
        PGM(JJ,JL,JB) = ZG * ( (PGAMMA1(JJ,JL,JB)+1./PCOSZEN(JJ))*ZGAMMA4 + PGAMMA2(JJ,JL,JB)*ZGAMMA3 ) !doc equation 44
        !
      ENDIF
      !
      ENDDO
    !
  END DO
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('GP_GM_VECTORS',1,ZHOOK_HANDLE)
!
END SUBROUTINE GP_GM_VECTORS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE TWO_STREAM_MATRIX(PSNOWALBEDO,PSOILALBEDO,PKESTAR,PDTAUSTAR,KNLVLS_EFF,KMAX_EFF,PDM,PD,PDP)
!compute the matrix in the system describing the continuity and boundary conditions at one point and one wavelength.
! see doc section 1.5
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWALBEDO ! snow albedo (npoints,nlayer,nbands)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILALBEDO ! soil albedo (npoints,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PKESTAR     !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDTAUSTAR   !Optical depth of each layer
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF  !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(IN)   :: KMAX_EFF    !maximum number of effective layers over the domain (nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDM,PD,PDP  ! three diagnonals of the matrix
!
REAL, DIMENSION(SIZE(PSNOWALBEDO,1)) :: ZFDIAG
!
REAL :: ZFDIAG2
!
INTEGER :: JB,JL,JI !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TWO_STREAM_MATRIX',0,ZHOOK_HANDLE)
!
!Initialization
PDM = 0.
PD  = 0.
PDP = 0.
!
DO JB = 1,NPNBANDS
  !
  DO JL = 1,KMAX_EFF(JB)-1
    !
!       IF(PSNOWALBEDO(1,JL,JB)<0.000001) THEN
!           PRINT*,"WARNING PSNOWALBEDO LAYER ",JL," BAND ",JB," : ",PSNOWALBEDO(1,JL,JB)
!       END IF


!       IF (EXP(-PKESTAR(1,JL,JB)*PDTAUSTAR(1,JL,JB))<0.000001) THEN
!           PRINT*,"WARNING ZFDIAG ",JL," BAND ",JB," : ",EXP(-PKESTAR(1,JL,JB)*PDTAUSTAR(1,JL,JB))     
!       END IF
!
    DO JI =1,SIZE(KNLVLS_EFF,1)
      !
      IF ( JL<=KNLVLS_EFF(JI,JB)-1 ) THEN
        !
        !See matrix documentation page 8 and formal expressions page 9
        ZFDIAG(JI) = EXP( -PKESTAR(JI,JL,JB)*PDTAUSTAR(JI,JL,JB) )
        !    
        !Décalage d'un indice vers la droite par rapport au code python
        PDM(JI,JL*2,JB)   = ( 1. - PSNOWALBEDO(JI,JL,JB)*PSNOWALBEDO(JI,JL+1,JB) ) * ZFDIAG(JI)
        PDM(JI,JL*2+1,JB) = ( 1./PSNOWALBEDO(JI,JL,JB) - PSNOWALBEDO(JI,JL,JB) )   * 1./ZFDIAG(JI)
        !  
        PD(JI,JL*2,JB)    = ( 1. - PSNOWALBEDO(JI,JL+1,JB)/PSNOWALBEDO(JI,JL,JB) ) * 1./ZFDIAG(JI)
        PD(JI,JL*2+1,JB)  = PSNOWALBEDO(JI,JL,JB) - PSNOWALBEDO(JI,JL+1,JB)
        !    
        !Décalage d'un indice vers la gauche par rapport au code python
        PDP(JI,JL*2,JB)   = PSNOWALBEDO(JI,JL+1,JB) * PSNOWALBEDO(JI,JL+1,JB) - 1.
        PDP(JI,JL*2+1,JB) = PSNOWALBEDO(JI,JL,JB) - 1./PSNOWALBEDO(JI,JL+1,JB)
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !  
  PDP(:,1,JB) = 1. !Décalage d'un indice vers la gauche par rapport au code python
  PD (:,1,JB) = 1.
  !  
  DO JI=1,SIZE(PSNOWALBEDO,1)
    !
    ZFDIAG2 = EXP( -PKESTAR(JI,KNLVLS_EFF(JI,JB),JB) * PDTAUSTAR(JI,KNLVLS_EFF(JI,JB),JB) )
    !      
    !Décalage d'un indice vers la droite par rapport au code python
    PDM(JI,2*KNLVLS_EFF(JI,JB),JB) = ZFDIAG2    * &
                                      ( PSNOWALBEDO(JI,KNLVLS_EFF(JI,JB),JB)    - PSOILALBEDO(JI,JB) )
    !
    PD (JI,2*KNLVLS_EFF(JI,JB),JB) = 1./ZFDIAG2 * &
                                      ( 1./PSNOWALBEDO(JI,KNLVLS_EFF(JI,JB),JB) - PSOILALBEDO(JI,JB) )
  END DO
  !  
END DO
!
IF (LHOOK) CALL DR_HOOK('TWO_STREAM_MATRIX',1,ZHOOK_HANDLE)
!
END SUBROUTINE TWO_STREAM_MATRIX
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE TWO_STREAM_VECTOR(PSNOWALBEDO,PSOILALBEDO,PDTAUSTAR,PTAUSTAR,PGM,PGP,PCOSZEN,KNLVLS_EFF,KMAX_EFF,PVECTOR)
!compute the V vector in the system describing the continuity and boundary conditions at one point and one wavelength.
! see doc section 1.5
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWALBEDO  ! snow albedo (npoints,nlayer,nbands)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILALBEDO  ! soil albedo (npoints,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDTAUSTAR    !Optical depth of each layer
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTAUSTAR     !Cumulated optical depth
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGP          !GP vector (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGM !GM vector (npoints,nlayer,nbands)
REAL, DIMENSION(:), INTENT(IN)      :: PCOSZEN ! cosine of zenithal solar angle (npoints)
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(IN)   :: KMAX_EFF !maximum number of effective layers over the domain (nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PVECTOR !output vector V
!
REAL :: ZDGP,ZDGM,ZEXP
!
INTEGER :: JB,JL,JI !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TWO_STREAM_VECTOR',0,ZHOOK_HANDLE)
!
PVECTOR(:,1,:) = -PGM(:,1,:)
!
DO JB = 1,NPNBANDS
  !
  DO JI = 1,SIZE(PSNOWALBEDO,1)
    !
    DO JL = 1,KMAX_EFF(JB)
      !
      IF ( JL<=KNLVLS_EFF(JI,JB)-1 ) THEN
        !
        ZDGP = PGP(JI,JL+1,JB) - PGP(JI,JL,JB) !doc equation 58
        ZDGM = PGM(JI,JL+1,JB) - PGM(JI,JL,JB) !doc equation 58
        !
        ZEXP = EXP( -PTAUSTAR(JI,JL,JB)/PCOSZEN(JI) )
        !see expression doc page 9
        PVECTOR(JI,2*JL,JB)   = ( ZDGM - PSNOWALBEDO(JI,JL+1,JB) * ZDGP ) * ZEXP 
        PVECTOR(JI,2*JL+1,JB) = ( ZDGP - PSNOWALBEDO(JI,JL,JB)   * ZDGM ) * ZEXP 
        !
      END IF
      !
    END DO
    !
    PVECTOR(JI,2*KNLVLS_EFF(JI,JB),JB) = ( PSOILALBEDO(JI,JB) * &
                                            ( PGM(JI,KNLVLS_EFF(JI,JB),JB) + PCOSZEN(JI) ) - &
                                           PGP(JI,KNLVLS_EFF(JI,JB),JB) ) * &
                                            EXP( -PTAUSTAR(JI,KNLVLS_EFF(JI,JB),JB) / PCOSZEN(JI) )
    !
  END DO
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('TWO_STREAM_VECTOR',1,ZHOOK_HANDLE)
!
END SUBROUTINE TWO_STREAM_VECTOR
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SOLVES_TWO_STREAM2(PDM,PD,PDP,PVECT_DIR,PVECT_DIF,PSNOWALBEDO,PSW_RAD_DIR,PSW_RAD_DIF,KNLVLS_EFF, &
                              KMAX_EFF,PXA_DIR,PXA_DIF,PXB_DIR,PXB_DIF,PXC_DIR,PXC_DIF,PXD_DIR,PXD_DIF)
!solve the two stream linear system for both direct and diffuse radiation
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
USE MODI_TRIDIAG_GROUND_SNOWCRO
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDM,PD,PDP ! three diagnonals of the matrix
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PVECT_DIR,PVECT_DIF !two-stream vector V (2 vectors are used when ther is diffuse AND direct incident light)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSNOWALBEDO! snow albedo (npoints,nlayer,nbands)
REAL, DIMENSION(:,:), INTENT(IN)    :: PSW_RAD_DIR,PSW_RAD_DIF !not used for now : useful if we want to pack the points excluding zero radiations
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(IN)   :: KMAX_EFF !maximum number of effective layers over the domain (nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PXA_DIR,PXA_DIF,PXB_DIR,PXB_DIF,PXC_DIR,PXC_DIF,PXD_DIR,PXD_DIF ! solutions (coeffs A and B in eq 46-47 or 48-49)
!
REAL, DIMENSION(SIZE(PDM,1),2*SIZE(PSNOWALBEDO,2),NPNBANDS) :: ZX0_DIR,ZX0_DIF
INTEGER :: JB,JI,JL !loop counters
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SOLVES_TWO_STREAM2',0,ZHOOK_HANDLE)
!
! for now we inverse the matrix twice :
! to be improved by adding a dimension in tridiag_ground_snowcro
 CALL TRIDIAG_GROUND_SNOWCRO(PDM(:,:,:),PD(:,:,:),PDP(:,:,:), &
                            PVECT_DIR(:,:,:),ZX0_DIR(:,:,:),  &
                            2*KNLVLS_EFF(:,:),0)
!
 CALL TRIDIAG_GROUND_SNOWCRO(PDM(:,:,:),PD(:,:,:),PDP(:,:,:), &
                            PVECT_DIF(:,:,:),ZX0_DIF(:,:,:),  &
                            2*KNLVLS_EFF(:,:),0)
!
!for now we always compute everything
DO JB = 1,NPNBANDS
  !  
  DO JL=1,KMAX_EFF(JB)
    !
    DO JI=1,SIZE(PDM,1)
      !
      IF ( JL<=KNLVLS_EFF(JI,JB) ) THEN
        PXA_DIR(JI,JL,JB) = ZX0_DIR(JI,JL*2-1,JB)
        PXA_DIF(JI,JL,JB) = ZX0_DIF(JI,JL*2-1,JB)
        PXB_DIR(JI,JL,JB) = ZX0_DIR(JI,JL*2,JB)
        PXB_DIF(JI,JL,JB) = ZX0_DIF(JI,JL*2,JB)
        !
        PXC_DIR(JI,JL,JB) = PXA_DIR(JI,JL,JB) * PSNOWALBEDO(JI,JL,JB)
        PXC_DIF(JI,JL,JB) = PXA_DIF(JI,JL,JB) * PSNOWALBEDO(JI,JL,JB)
        PXD_DIR(JI,JL,JB) = PXB_DIR(JI,JL,JB) / PSNOWALBEDO(JI,JL,JB)
        PXD_DIF(JI,JL,JB) = PXB_DIF(JI,JL,JB) / PSNOWALBEDO(JI,JL,JB)
      END IF
      !
    END DO
    !
  END DO
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('SOLVES_TWO_STREAM2',1,ZHOOK_HANDLE)
!
END SUBROUTINE SOLVES_TWO_STREAM2
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SNOWPACK_ALBEDO(PXC_DIR,PXC_DIF,PXD_DIR,PXD_DIF,PGP_DIR,PGP_DIF,&
                           PCOSZEN_DIR,PCOSZEN_DIF,PSW_RAD_DIR,PSW_RAD_DIF,PSNOWALB)
! compute the albedo of the snowpack at one wavelength
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PXC_DIR,PXC_DIF,PXD_DIR,PXD_DIF ! for first level (npoints*nbands)
REAL, DIMENSION(:,:), INTENT(IN)  :: PGP_DIR,PGP_DIF ! for first level (npoints*nbands)
REAL, DIMENSION(:), INTENT(IN)    :: PCOSZEN_DIR,PCOSZEN_DIF  ! cosine of zenithal solar angle (npoints)
REAL, DIMENSION(:,:), INTENT(IN)  :: PSW_RAD_DIR,PSW_RAD_DIF  ! incident radiation W/m^2 (npoints*nbands)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSNOWALB ! albedo at one wavelength
!
REAL,DIMENSION(SIZE(PSW_RAD_DIR,1)) :: ZREF_DIR,ZREF_DIF ! reflected direct and diffuse radiations W/m^2 
                                                         ! for one band
REAL,DIMENSION(SIZE(PSW_RAD_DIR,1)) :: ZINC ! incident radiation
!
INTEGER :: JB !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SNOWPACK_ALBEDO',0,ZHOOK_HANDLE)
!
DO JB = 1,NPNBANDS
  !
  ! Doc equation 66 (separated in direct and diffuse components)
  ZREF_DIR = ( PXC_DIR(:,JB)+PXD_DIR(:,JB)+PGP_DIR(:,JB) ) * PSW_RAD_DIR(:,JB)
  ZREF_DIF = ( PXC_DIF(:,JB)+PXD_DIF(:,JB)+PGP_DIF(:,JB) ) * PSW_RAD_DIF(:,JB) 
  ZINC = PSW_RAD_DIR(:,JB)*PCOSZEN_DIR + PSW_RAD_DIF(:,JB)*PCOSZEN_DIF
  WHERE ( ZINC>0. )
    PSNOWALB(:,JB) = (ZREF_DIR+ZREF_DIF) / ZINC
  ELSEWHERE
    PSNOWALB(:,JB) = 0.
  ENDWHERE
  !  
END DO

IF (LHOOK) CALL DR_HOOK('SNOWPACK_ALBEDO',1,ZHOOK_HANDLE)

END SUBROUTINE SNOWPACK_ALBEDO
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE ENERGY_PROFILE(PXA,PXB,PXC,PXD,PKESTAR,PDTAUSTAR,PTAUSTAR,PGM,PGP,PCOSZEN,KNLVLS_EFF,KMAX_EFF,PEPROFILE)

    !compute energy absorption for each layer and wavelength
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PXA,PXB,PXC,PXD ! solutions of linear system (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDTAUSTAR !Optical depth of each layer (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTAUSTAR !Cumulated optical depth (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGP !GP vector (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGM !GM vector (npoints,nlayer,nbands)
REAL, DIMENSION(:), INTENT(IN)      :: PCOSZEN! cosine of zenithal solar angle (npoints)
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
INTEGER, DIMENSION(:), INTENT(IN)   :: KMAX_EFF !maximum number of effective layers over the domain (nbands)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PEPROFILE ! energy absorbed by each layer (W/m^2) npoints,nlayer,nbands)
!
REAL :: ZINT1, ZINT2, ZINT3, ZINT4, ZINT5
REAL :: ZDEXP, ZFDU, ZFDD, ZSTAR
!
INTEGER::JB,JL,JJ !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('ENERGY_PROFILE',0,ZHOOK_HANDLE)

DO JB = 1,NPNBANDS
  !
  DO JJ =1,SIZE(PEPROFILE,1)
    !
    DO JL = 1,KMAX_EFF(JB)
      !
      IF (JL==1.OR.JL<=KNLVLS_EFF(JJ,JB)) THEN
        !
        ZSTAR = PKESTAR(JJ,JL,JB) * PDTAUSTAR(JJ,JL,JB)
        ZINT1 = EXP(-ZSTAR)
        ZINT2 = EXP( ZSTAR)
        !
        IF (JL==1) THEN
          !
          ZINT3 = EXP( -PDTAUSTAR(JJ,JL,JB)/PCOSZEN(JJ))
          ZINT4 = EXP( -PTAUSTAR (JJ,JL,JB)/PCOSZEN(JJ))
          !
          !surface layer doc equation 64
          PEPROFILE(JJ,1,JB) = ( PCOSZEN(JJ) - ( PXC(JJ,1,JB)+PXD(JJ,1,JB)+PGP(JJ,1,JB) ) ) + &
                               ( PXC(JJ,1,JB) * ZINT1 + PXD(JJ,1,JB) * ZINT2 + &
                                 PGP(JJ,1,JB) * ZINT3 ) - &
                               ( PXA(JJ,1,JB) * ZINT1 + PXB(JJ,1,JB) * ZINT2 + &
                                 PGM(JJ,1,JB) * ZINT3 + &
                                 PCOSZEN(JJ) * ZINT4 ) 
          !
        ELSE
          !
          ZINT5 = EXP( -PTAUSTAR(JJ,JL  ,JB)/PCOSZEN(JJ) )
          !last factor in equations 62 and 63
          ZDEXP = ZINT5 - ZINT4
          ZINT4 = ZINT5
          !
          !doc equation 62
          ZFDU = PXC(JJ,JL,JB) * ( ZINT1 -1. ) + &
                 PXD(JJ,JL,JB) * ( ZINT2 -1. ) + PGP(JJ,JL,JB) * ZDEXP
          !
          !doc equation 63
          ZFDD = PXA(JJ,JL,JB) * ( ZINT1 -1. ) + &
                 PXB(JJ,JL,JB) * ( ZINT2 -1. ) + ( PGM(JJ,JL,JB) + PCOSZEN(JJ) ) * ZDEXP
          !      
          PEPROFILE(JJ,JL,JB) = ZFDU - ZFDD !doc equation 61
          !
        ENDIF
        !
      ELSE
        !
        PEPROFILE(JJ,JL,JB) = 0.
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ENERGY_PROFILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE ENERGY_PROFILE
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SOIL_ABSORPTION(PXA,PXB,PKESTAR,PDTAUSTAR,PTAUSTAR,PGM,PCOSZEN,PALB,KNLVLS_EFF,PSOILENERGY)
!compute the energy absorbed by the soil at each wavelength
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PXA,PXB ! solutions of linear system (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PKESTAR !Asymptotic Flux Extinction Coefficent (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PDTAUSTAR !Optical depth of each layer (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTAUSTAR !Cumulated optical depth (npoints,nlayer,nbands)
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PGM !GM vector (npoints,nlayer,nbands)
REAL, DIMENSION(:), INTENT(IN)      :: PCOSZEN! cosine of zenithal solar angle (npoints)
REAL, DIMENSION(:,:), INTENT(IN)    :: PALB! soil albedo (npoints,nbands)
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNLVLS_EFF !number of effective layers (npoints,nbands)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSOILENERGY
INTEGER :: JB,JI !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SOIL_ABSORPTION',0,ZHOOK_HANDLE)
!
DO JB=1,NPNBANDS
  !
  DO JI=1,SIZE(PXA,1)
    !
    !doc equation 65
    PSOILENERGY(JI,JB) = ( 1.-PALB(JI,JB) ) * &
                           ( PXA(JI,KNLVLS_EFF(JI,JB),JB) * &
                               EXP( -PKESTAR(JI,KNLVLS_EFF(JI,JB),JB) * PDTAUSTAR(JI,KNLVLS_EFF(JI,JB),JB) ) + &
                             PXB(JI,KNLVLS_EFF(JI,JB),JB) * &
                               EXP(  PKESTAR(JI,KNLVLS_EFF(JI,JB),JB) * PDTAUSTAR(JI,KNLVLS_EFF(JI,JB),JB) ) + &
                           ( PGM(JI,KNLVLS_EFF(JI,JB),JB)+PCOSZEN(JI) ) * &
                               EXP( -PTAUSTAR(JI,KNLVLS_EFF(JI,JB),JB)/PCOSZEN(JI) ) )
    !
  END DO
  !
END DO

IF (LHOOK) CALL DR_HOOK('SOIL_ABSORPTION',1,ZHOOK_HANDLE)

END SUBROUTINE SOIL_ABSORPTION
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SPECTRAL_REPARTITION(PSW_RAD,PCOSZEN,PSW_RAD_DIF,PSW_RAD_DIR,PNIR_ABS)

USE MODD_CONST_TARTES, ONLY : NPNBANDS,XPRATIO_DIR,XPRATIO_DIF,XPCOEFNIR_DIR,XPCOEFNIR_DIF,XP_MUDIFF

IMPLICIT NONE

REAL, DIMENSION(:), INTENT(IN)    :: PSW_RAD ! broadband global incident light (W/m^2) (npoints)
REAL, DIMENSION(:), INTENT(IN)    :: PCOSZEN ! cosine of zenithal solar angle (npoints)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSW_RAD_DIF ! spectral diffuse incident light (W/m^2) (npoints,nbands)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSW_RAD_DIR ! spectral direct incident light (W/m^2) (npoints,nbands)
REAL, DIMENSION(:), INTENT(OUT)   :: PNIR_ABS ! Near infrared radiation (2500-4000 nm) absorbed by snowpack (W/m^2) (npoints)
!
REAL, DIMENSION(SIZE(PSW_RAD)) :: ZSW_RAD_BROADDIR,ZSW_RAD_BROADDIF ! direct and diffuse broadband incident light (W/m^2) (npoints)
INTEGER :: JB !Loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SPECTRAL_REPARTITION',0,ZHOOK_HANDLE)
!
! Separate broadband global radiation in direct and diffuse (parametrization Marie Dumont)
! NB : thresold 1. to the factor when zenithal angle close to pi/2
ZSW_RAD_BROADDIF = MIN( EXP( - 1.54991930344*PCOSZEN**3 + 3.73535795329*PCOSZEN**2 &
                             - 3.52421131883*PCOSZEN + 0.0299111951172 ), 1. ) * PSW_RAD
ZSW_RAD_BROADDIR = PSW_RAD - ZSW_RAD_BROADDIF
!
! Spectral decomposition
DO JB = 1,NPNBANDS
  PSW_RAD_DIF(:,JB) = XPRATIO_DIF(JB) * ZSW_RAD_BROADDIF / XP_MUDIFF
  PSW_RAD_DIR(:,JB) = XPRATIO_DIR(JB) * ZSW_RAD_BROADDIR / PCOSZEN(:)
END DO

PNIR_ABS = ZSW_RAD_BROADDIF*XPCOEFNIR_DIF + ZSW_RAD_BROADDIR*XPCOEFNIR_DIR

IF (LHOOK) CALL DR_HOOK('SPECTRAL_REPARTITION',1,ZHOOK_HANDLE)

END SUBROUTINE SPECTRAL_REPARTITION
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
SUBROUTINE SNOWCRO_TARTES(PSNOWGRAN1,PSNOWGRAN2,PSNOWRHO,PSNOWDZ,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0, &
                          PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,PALB,PSW_RAD,PZENITH,KNLVLS_USE,      &
                          PSNOWALB,PRADSINK,PRADXS,ODEBUG,HSNOWMETAMO)
!
! Interface between Tartes and Crocus
! M. Lafaysse 26/08/2013
!
USE MODD_CONST_TARTES, ONLY : NPNIMP
USE MODD_SNOW_METAMO,  ONLY : XUEPSI
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWGRAN1,PSNOWGRAN2  ! (npoints,nlayer) 
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWRHO !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWDZ !snow layers thickness (m) (npoints,nlayer)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
REAL, DIMENSION(:), INTENT(IN)     :: PALB ! soil/vegetation albedo (npoints)
!
REAL, DIMENSION(:), INTENT(IN)     :: PSW_RAD ! global broadband incident light (W/m^2) (npoints)
REAL, DIMENSION(:), INTENT(IN)     :: PZENITH ! zenithal solar angle (npoints)
!
INTEGER, DIMENSION(:), INTENT(IN)  :: KNLVLS_USE ! number of effective snow layers (npoints)
!
!Same outputs as SNOWCRORAD and SNOWCROALB
REAL, DIMENSION(:,:), INTENT(OUT) :: PRADSINK !(npoints,nlayers)
REAL, DIMENSION(:), INTENT(OUT)   :: PRADXS !(npoints,nlayers)
REAL, DIMENSION(:), INTENT(OUT)   :: PSNOWALB !(npoints,nlayers)
!
LOGICAL, INTENT(IN) :: ODEBUG ! Print for debugging
 CHARACTER(3), INTENT(IN)          :: HSNOWMETAMO ! metamorphism scheme
!
!packed variables
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),NPNIMP) :: ZSNOWIMP_DENSITY_P !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),NPNIMP) :: ZSNOWIMP_CONTENT_P !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN1_P,ZSNOWGRAN2_P  ! (npoints,nlayer) 
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO_P !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWG0_P ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWY0_P ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWW0_P ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWB0_P ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWDZ_P !snow layers thickness (m) (npoints,nlayer)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZRADSINK_P
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZALB_P ! soil/vegetation albedo (npoints)
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSW_RAD_P ! global broadband incident light (W/m^2) (npoints)
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZZENITH_P ! zenithal solar angle (npoints)
!
!Same outputs as SNOWCRORAD and SNOWCROALB
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZRADXS_P
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWALB_P
!
INTEGER, DIMENSION(SIZE(PSNOWRHO,1)) :: INLVLS_USE_P ! number of effective snow layers (npoints)
!
INTEGER, DIMENSION(SIZE(PSNOWRHO,1)) :: IDAYMASK ! mask for points where it's day
!
INTEGER :: IMAX_USE ! maximum number of layers over the domain
INTEGER :: JL,JIMP,JJ,JJ_P !Loop counter
INTEGER :: IPOINTDAY
INTEGER :: INPOINTS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_TARTES',0,ZHOOK_HANDLE)
!
!Default values (night)
PRADSINK = 0.
PRADXS   = 0.
PSNOWALB = 1.
!
INPOINTS = SIZE(PSNOWRHO,1)
!
!Mask
IPOINTDAY = 0
DO JJ = 1,INPOINTS
  IF ( COS(PZENITH(JJ))>XUEPSI .AND. PSW_RAD(JJ)>XUEPSI ) THEN
    !mask for day
    IPOINTDAY = IPOINTDAY + 1 
    IDAYMASK(IPOINTDAY) = JJ
  END IF
END DO
!
IF ( IPOINTDAY>=1 ) THEN
  !
  ! Pack 1D variables
  DO JJ_P = 1,IPOINTDAY
    !
    JJ = IDAYMASK(JJ_P)
    !
    ZALB_P      (JJ_P) = PALB      (JJ)
    ZSW_RAD_P   (JJ_P) = PSW_RAD   (JJ)
    ZZENITH_P   (JJ_P) = PZENITH   (JJ)
    INLVLS_USE_P(JJ_P) = KNLVLS_USE(JJ)
    !
  END DO
  !
  IMAX_USE = MAXVAL(KNLVLS_USE)
  !
  ! Pack 2D variables
  DO JL = 1,IMAX_USE
    !
    DO JJ_P = 1,IPOINTDAY
      !
      JJ = IDAYMASK(JJ_P)
      !
      ZSNOWGRAN1_P(JJ_P,JL) = PSNOWGRAN1(JJ,JL)
      ZSNOWGRAN2_P(JJ_P,JL) = PSNOWGRAN2(JJ,JL)
      ZSNOWRHO_P  (JJ_P,JL) = PSNOWRHO  (JJ,JL)
      ZSNOWG0_P   (JJ_P,JL) = PSNOWG0   (JJ,JL)
      ZSNOWY0_P   (JJ_P,JL) = PSNOWY0   (JJ,JL)
      ZSNOWW0_P   (JJ_P,JL) = PSNOWW0   (JJ,JL)
      ZSNOWB0_P   (JJ_P,JL) = PSNOWB0   (JJ,JL)      
      ZSNOWDZ_P   (JJ_P,JL) = PSNOWDZ   (JJ,JL)
      !
    END DO
    !
  END DO
  !
  ! Pack 3D variables
  DO JIMP = 1,NPNIMP
    !
    DO JL = 1,IMAX_USE
      !
      DO JJ_P = 1,IPOINTDAY
        !
        JJ = IDAYMASK(JJ_P)
        !
        ZSNOWIMP_DENSITY_P(JJ_P,JL,JIMP) = PSNOWIMP_DENSITY(JJ,JL,JIMP)
        ZSNOWIMP_CONTENT_P(JJ_P,JL,JIMP) = PSNOWIMP_CONTENT(JJ,JL,JIMP)      
        !
      END DO
      !
    END DO
    !
  END DO
  !
!RJ: fix fp-invalid(nan) trapping in ISBA_DIF8_SN3L_NIT_SNCRO8_C13_SNOWRAD_TAR, ISBA_DIF8_SN3L_NIT_SNCRO8_C13_SNOWRAD_TA2 tests
#ifdef RJ_OFIX
!RJ: temp fix to avoid accessing uninited values (NANS) in mode_tartes.F90 by explicit inited shape, no inpact for results, problem in array padding
  CALL SNOWCRO_CALL_TARTES(ZSNOWGRAN1_P(1:IPOINTDAY,1:IMAX_USE),ZSNOWGRAN2_P(1:IPOINTDAY,1:IMAX_USE), &
                           ZSNOWRHO_P(1:IPOINTDAY,1:IMAX_USE),ZSNOWDZ_P(1:IPOINTDAY,1:IMAX_USE),      &
                           ZSNOWG0_P(1:IPOINTDAY,1:IMAX_USE),ZSNOWY0_P(1:IPOINTDAY,1:IMAX_USE),       &
                           ZSNOWW0_P(1:IPOINTDAY,1:IMAX_USE),ZSNOWB0_P(1:IPOINTDAY,1:IMAX_USE),       &
                           ZSNOWIMP_DENSITY_P(1:IPOINTDAY,1:IMAX_USE,1:NPNIMP),                       &
                           ZSNOWIMP_CONTENT_P(1:IPOINTDAY,1:IMAX_USE,1:NPNIMP),                       &
                           ZALB_P(1:IPOINTDAY),ZSW_RAD_P(1:IPOINTDAY),                                &
                           ZZENITH_P(1:IPOINTDAY),INLVLS_USE_P(1:IPOINTDAY),ZSNOWALB_P(1:IPOINTDAY),  & 
                           ZRADSINK_P(1:IPOINTDAY,1:IMAX_USE),ZRADXS_P(1:IPOINTDAY),ODEBUG,HSNOWMETAMO)
#else
  CALL SNOWCRO_CALL_TARTES(ZSNOWGRAN1_P(1:IPOINTDAY,:),ZSNOWGRAN2_P(1:IPOINTDAY,:),ZSNOWRHO_P(1:IPOINTDAY,:),     &
                           ZSNOWDZ_P(1:IPOINTDAY,:),ZSNOWG0_P(1:IPOINTDAY,:),ZSNOWY0_P(1:IPOINTDAY,:),            &
                           ZSNOWW0_P(1:IPOINTDAY,:),ZSNOWB0_P(1:IPOINTDAY,:),ZSNOWIMP_DENSITY_P(1:IPOINTDAY,:,:), &
                           ZSNOWIMP_CONTENT_P(1:IPOINTDAY,:,:),ZALB_P(1:IPOINTDAY),ZSW_RAD_P(1:IPOINTDAY),        &
                           ZZENITH_P(1:IPOINTDAY),INLVLS_USE_P(1:IPOINTDAY),ZSNOWALB_P(1:IPOINTDAY),              &
                           ZRADSINK_P(1:IPOINTDAY,:),ZRADXS_P(1:IPOINTDAY),ODEBUG,HSNOWMETAMO)
#endif
  !
  !Unpack 1d output variables
  !
  DO JJ_P = 1,IPOINTDAY
    !
    JJ = IDAYMASK(JJ_P)
    !
    PRADXS  (JJ) = ZRADXS_P  (JJ_P)
    PSNOWALB(JJ) = ZSNOWALB_P(JJ_P)
    !
  END DO
  !
  !Unpack 2d output  variables
  DO JL = 1,IMAX_USE
    !
    DO JJ_P = 1,IPOINTDAY
      !
      JJ = IDAYMASK(JJ_P)
      !
      PRADSINK(JJ,JL) = ZRADSINK_P(JJ_P,JL)
      !
    END DO
    !
  END DO
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_TARTES',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCRO_TARTES

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

SUBROUTINE SNOWCRO_CALL_TARTES(PSNOWGRAN1,PSNOWGRAN2,PSNOWRHO,PSNOWDZ,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0, &
                               PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,PALB,PSW_RAD,PZENITH,KNLVLS_USE,      &
                               PSNOWALB,PRADSINK,PRADXS,ODEBUG,HSNOWMETAMO)
!
! Interface between Tartes and Crocus
! M. Lafaysse 26/08/2013
!
USE MODD_CONST_TARTES, ONLY : NPNBANDS,XPWAVELENGTHS,XP_MUDIFF
USE MODD_CSTS, ONLY : XRHOLI,XPI
!
USE MODE_SNOW3L, ONLY : GET_DIAM
!
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWGRAN1,PSNOWGRAN2  ! (npoints,nlayer) 
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWRHO !snow density (kg/m^3) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWDZ !snow layers thickness (m) (npoints,nlayer)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
REAL, DIMENSION(:), INTENT(IN)     :: PALB ! soil/vegetation albedo (npoints)
!
REAL, DIMENSION(:), INTENT(IN)     :: PSW_RAD ! global broadband incident light (W/m^2) (npoints)
REAL, DIMENSION(:), INTENT(IN)     :: PZENITH ! zenithal solar angle (npoints)
!
INTEGER, DIMENSION(:), INTENT(IN)  :: KNLVLS_USE ! number of effective snow layers (npoints)
!
!Same outputs as SNOWCRORAD and SNOWCROALB
REAL, DIMENSION(:,:), INTENT(OUT) :: PRADSINK !(npoints,nlayers)
REAL, DIMENSION(:), INTENT(OUT)   :: PRADXS !(npoints,nlayers)
REAL, DIMENSION(:), INTENT(OUT)   :: PSNOWALB !(npoints,nlayers)

LOGICAL,INTENT(IN) :: ODEBUG ! Print for debugging
 CHARACTER(3), INTENT(IN)          :: HSNOWMETAMO ! metamorphism scheme

!Local variables
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),NPNBANDS) :: ZSNOWENERGY !(npoints,nlayer,nbands)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWSSA !snow specific surface area (m^2/kg) (npoints,nlayer) 
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWENERGY_BB ! (W/m^2) (npoints,nlayers)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNBANDS) :: ZSW_RAD_DIF ! spectral diffuse incident light (W/m^2) (npoints,nbands)
REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNBANDS) :: ZSW_RAD_DIR ! spectral direct incident light (W/m^2) (npoints,nbands)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNBANDS) :: ZSNOWALB  !(npoints,nbands)
REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNBANDS) :: ZALB ! soil/vegetation albedo (npoints,nbands)
REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNBANDS) :: ZSOILENERGY !(npoints,nbands)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZNIR_ABS ! Near infrared radiation (2500-4000 nm) absorbed by snowpack (W/m^2) (npoints)
!
!broad band
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZTOTSNOWENERGY ! (W/m^2) (npoints)
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSOILENERGY_BB ! (W/m^2) (npoints)
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZREFLECTED_BB ! (W/m^2) (npoints)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWENERGY_CUM,ZSNOWENERGY_UPPER ! Cumulated absorbed energy for 1 wavelength W/m^2 (npoints)
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZMAX ! maximum available energy for 1 wavelength  W/m^2 (npoints,nbands)
!
REAL :: ZDIAM !optical diameter
!
INTEGER :: JB,JL,JJ !Loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_CALL_TARTES',0,ZHOOK_HANDLE)
!
! Compute SSA from SNOWGRAN1 and SNOWGRAN2
DO JL = 1,SIZE(PSNOWRHO,2)
  !
  DO JJ = 1,SIZE(PSNOWRHO,1)
    !
    IF ( JL<=KNLVLS_USE(JJ) ) THEN
      !
      CALL GET_DIAM(PSNOWGRAN1(JJ,JL),PSNOWGRAN2(JJ,JL),ZDIAM,HSNOWMETAMO)
      ZSNOWSSA(JJ,JL) = 6. / (XRHOLI*ZDIAM)
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
DO JB = 1,NPNBANDS
  ! Soil-vegetation albedo homogeneous in wavelength
  ZALB(:,JB) = PALB(:)
END DO
!
!Spectral repartition of radiation
 CALL SPECTRAL_REPARTITION(PSW_RAD,COS(PZENITH),ZSW_RAD_DIF,ZSW_RAD_DIR,ZNIR_ABS)
!
IF ( ODEBUG ) THEN
  WRITE(*,*) "ZSW_RAD_DIF=",ZSW_RAD_DIF
  WRITE(*,*) "ZSW_RAD_DIR=",ZSW_RAD_DIR
  WRITE(*,*) "PZENITH=",PZENITH
END IF
!
!Call tartes model
! For test and debugging this routine can be called independently by a python interface   
!
 CALL TARTES(ZSNOWSSA,PSNOWRHO,PSNOWDZ,PSNOWG0,PSNOWY0,PSNOWW0,PSNOWB0,PSNOWIMP_DENSITY,PSNOWIMP_CONTENT,ZALB,&
             ZSW_RAD_DIF,ZSW_RAD_DIR,COS(PZENITH),KNLVLS_USE,ZSNOWALB,ZSNOWENERGY,ZSOILENERGY)
! 
    ! Modif ML : in some cases, Tartes is unstable in infra-red wavelengths : control of energy values and apply threshold if necessary
    ! --------------------------------------------------------------------------------------------------------------------
    
    
    ! This does not seem to be necessary at CDP when the effective number of layers is properly limited.
    ! However, we let the comment code because it might happen again.
    
!     
!     DO JB=1,NPNBANDS
!       ! maximum available energy at this wavelength
!       ZMAX=ZSW_RAD_DIF(:,JB)*PP_MUDIFF+ZSW_RAD_DIR(:,JB)*COS(PZENITH)
!       ZSNOWENERGY_CUM(:)=0.
!       DO JL=1,SIZE(PSNOWRHO,2)
!         DO JJ=1,SIZE(PSNOWRHO,1)        
!           IF (JL<=KNLVLS_USE(JJ)) THEN
!             ZSNOWENERGY_UPPER(JJ)=ZSNOWENERGY_CUM(JJ) !0 for surface layer
!             ! absorbed energy cumulated from the surface
!             ZSNOWENERGY_CUM(JJ)=ZSNOWENERGY_CUM(JJ)+ZSNOWENERGY(JJ,JL,JB)
!             
!             ! Case when the energy is negative but close to 0. It can occurs in short wavelengths. Force to 0.
!             IF ((ZSNOWENERGY(JJ,JL,JB)<0).AND. (ZSNOWENERGY(JJ,JL,JB)>-0.1)) THEN
!               ZSNOWENERGY(JJ,JL,JB)=0.
!             END IF
!             
!             !if the cumulated absorbed energy excess the available energy or if severe negative energy, numerical problem in Tartes : total absorption
!             IF ((ZSNOWENERGY_CUM(JJ)>ZMAX(JJ)).OR.(ZSNOWENERGY(JJ,JL,JB)<=-0.1)) THEN
! !               IF (PPWAVELENGTHS(JB)<=1000) THEN             
!               IF ((PPWAVELENGTHS(JB)<=1000) .AND.(ABS(ZSNOWENERGY_CUM(JJ)-ZMAX(JJ))>0.01)) THEN 
!                 ! Tolerance 0.01 W/m2 of excess energy in the visible
!                 ! Above, the problem should never happen in visible
!                 
!                 PRINT*,"JB=",JB,"JL=",JL
!                 IF (ZSNOWENERGY_CUM(JJ)>ZMAX(JJ)) PRINT*,"excess energy"
!                 IF (ZSNOWENERGY(JJ,JL,JB)<0) PRINT*, "negative energy"
! 
!                 PRINT*,ZSW_RAD_DIF(JJ,JB),ZSW_RAD_DIR(JJ,JB),ZMAX(JJ),ZSNOWENERGY_CUM(JJ)
!                 
!                 PRINT*,"profile :",ZSNOWENERGY(JJ,:,JB)
!                 
!                 STOP "FATAL ERROR TARTES !!"
!               END IF
!               ! The layer absorbes all the remaining energy at this wavelength
!               ZSNOWENERGY(JJ,JL,JB)=ZMAX(JJ)-ZSNOWENERGY_UPPER(JJ)
!               ! update cumulated energy
!               ZSNOWENERGY_CUM(JJ)=ZMAX(JJ)
!             END IF
!           END IF
!         END DO
!       END DO
!       
!       ! Threshold on soil absorbed energy
!       DO JJ=1,SIZE(PSNOWRHO,1)
!         ZSNOWENERGY_UPPER(JJ)=ZSNOWENERGY_CUM(JJ)
!         ZSNOWENERGY_CUM(JJ)=ZSNOWENERGY_CUM(JJ)+ZSOILENERGY(JJ,JB)
!         IF (ZSNOWENERGY_CUM(JJ)>ZMAX(JJ)) THEN
!           IF (PPWAVELENGTHS(JB)<=1200) THEN
!                  STOP "FATAL ERROR TARTES (soil excess energy in visible)!!"           
!           END IF
!           ! The layer absorbes all the remaining energy at this wavelength
!           ZSOILENERGY(JJ,JB)=ZMAX(JJ)-ZSNOWENERGY_UPPER(JJ)
!         END IF
!       END DO
!       
!     END DO

    ! End modif ML
    ! --------------------------------------------------------------------------------------------------------------------
!    
! Broadband absorbed energy by snowpack and soil
ZSNOWENERGY_BB = 0.
ZSOILENERGY_BB = 0.
!
DO JB = 1,NPNBANDS
  ZSNOWENERGY_BB(:,:) = ZSNOWENERGY_BB(:,:) + ZSNOWENERGY(:,:,JB)
  ZSOILENERGY_BB(:)   = ZSOILENERGY_BB(:)   + ZSOILENERGY(:,JB)
END DO
!
!Add near infra-red wavelengths to first layer
ZSNOWENERGY_BB(:,1) = ZSNOWENERGY_BB(:,1) + ZNIR_ABS
!
! Total energy absorbed by snowpack
ZTOTSNOWENERGY(:)=0
DO JL = 1,SIZE(PSNOWRHO,2)
  DO JJ = 1,SIZE(PSNOWRHO,1)
    IF ( JL<=KNLVLS_USE(JJ) ) THEN
      ZTOTSNOWENERGY(JJ) = ZTOTSNOWENERGY(JJ) + ZSNOWENERGY_BB(JJ,JL)
    END IF
  END DO
END DO
!    
! Reflected energy
ZREFLECTED_BB = PSW_RAD - ZTOTSNOWENERGY - ZSOILENERGY_BB
! 
! Broad band Albedo
! PSW_RAD is never 0 because this routine is not called during the night
PSNOWALB = ZREFLECTED_BB / PSW_RAD
!   
! Source term
PRADSINK(:,1) = -PSW_RAD(:) + ZREFLECTED_BB + ZSNOWENERGY_BB(:,1)
!    
DO JL = 2,SIZE(PSNOWRHO,2)
  PRADSINK(:,JL) = PRADSINK(:,JL-1) + ZSNOWENERGY_BB(:,JL)
END DO
!
!Excess energy
PRADXS = PSW_RAD - ZTOTSNOWENERGY - ZREFLECTED_BB
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_CALL_TARTES',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCRO_CALL_TARTES       

END MODULE MODE_TARTES
