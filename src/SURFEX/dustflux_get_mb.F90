!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE DUSTFLUX_GET_MB(     &
        PUSTAR                  &  !I [m/s] Wind friction speed 
       ,PRHOA                   &  !I [kg/m3] air density at 2m height 
       ,PWG                     &  !I [m3/m3] volumetric water content 
       ,PZ0                     &  !I [m] roughness length of surface
       ,PWSAT                   &  !I [m3 m-3] saturation liquid water content
       ,PCLAY                   &  !I [frc] mass fraction clay
       ,PSAND                   &  !I [frc] mass fraction sand
       ,PDST_EROD               &  !I [frc] erodible surface
       ,PWIND10M                &  !I [m/s] wind at 10m altitude
       ,PSFDST                  &  !O [kg/m2/sec] Vertical dust flux
       ,KSIZE                   &  !I [nbr] number of points for calculation
       )
!
USE MODE_DSTMBLUTL                     !Dust mobilization subroutines
USE MODD_DST_SURF, ONLY :  CVERMOD
USE MODD_DSTMBL, ONLY : XFLX_MSS_FDG_FCTM, NTEX, NMODE, NDP, NBIN, XCST_SLT, &
                        XDMT_SLT_OPT
USE MODD_CSTS, only: XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!INPUT, set their dimensions to their passed lengths or to KSIZE ?
INTEGER, INTENT(IN)                  :: KSIZE    ![nbr] length of passed arrays
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PUSTAR   ![m/s] wind friction speed
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PRHOA    ![kg/m3] air density at 2m height
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PCLAY    ![frc] mass fraction clay
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PSAND    ![frc] mass fraction sand
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PDST_EROD![frc]
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWG      ![m3 m-3] volumetric water fraction
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWSAT    ![m3 m-3] saturation water content
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PZ0      ![m] surface roughness length
REAL, INTENT(IN), DIMENSION(KSIZE)   :: PWIND10M ![m/s] wind at 10m altitude
!OUTPUT the flux of dust
REAL, INTENT(OUT), DIMENSION(KSIZE)  :: PSFDST   ! [kg m-2 s-1] Output flux of atmospheric dust
!
!!!!!!!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&!!!!!!

!#ifdef AlG01
!    real,parameter::XFLX_MSS_FDG_FCT=28. ! [frc] Global mass flux tuning factor (a posteriori)
!#else
!    real,parameter::XFLX_MSS_FDG_FCT=7.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
!    real,parameter::XFLX_MSS_FDG_FCT=21.0e-4 ! [frc] Global mass flux tuning factor (a posteriori)
!    real,parameter::XFLX_MSS_FDG_FCT=12.0e-4 ! [frc] values used in Masdev47
!
!Define local variables:
LOGICAL, DIMENSION(KSIZE) :: GFLG_MBL                  ! [frc] Mobilization candidate flag
!REAL,    DIMENSION(KSIZE) :: ZMBL_BSN_FCT              ! [frc] enhancement factor for grid cells with higher erodibility
REAL,    DIMENSION(KSIZE) :: ZWND_RFR                  ! [m s-1] wind speed at reference level 
REAL,    DIMENSION(KSIZE) :: ZWND_FRC_THR_SLT          ! [m/s] Threshold wind friction speed when all effects taken into account
REAL,    DIMENSION(KSIZE) :: ZWND_RFR_THR_SLT          ! [m s-1] Threshold wind speed at reference level
REAL,    DIMENSION(KSIZE) :: ZGWC_SFC                  ! [kg/kg] Gravimetric water content
REAL,    DIMENSION(KSIZE) :: ZFRC_THR_NCR_WTR          ! [frc] Fraction by which soil wetness increases threshold wind
REAL,    DIMENSION(KSIZE) :: ZFRC_THR_NCR_DRG          ! [frc] fraction by which drag partitioning increases threshold wind
REAL,    DIMENSION(KSIZE) :: ZWND_FRC_SLT              ! [m/s] wind friction speed after modified for saltation feedbacks
REAL,    DIMENSION(KSIZE,NBIN) :: ZFLX_MSS_HRZ_SLT_TTL_WBN  ! [kg m-1 s-1] Vertically integrated horizontal saltation soil flux for a wind bin 
REAL,    DIMENSION(KSIZE,NBIN) :: ZFLX_MSS_VRT_DST_TTL_WBN  ! [kg m-2 s-1]
REAL,    DIMENSION(KSIZE) :: ZDST_SLT_FLX_RAT_TTL      ! [m-1] ratio of vertical to horizontal flux (alpha in several papers)
REAL,    DIMENSION(KSIZE) :: ZSILT                     ! [frc] dummy for fraction of silt 
REAL,    DIMENSION(KSIZE) :: ZWPRM                     ! threshold soil wetness
INTEGER, DIMENSION(KSIZE) :: ITEXT                     ! soil texture
REAL, DIMENSION(NTEX,NDP) :: ZDSRLV                    ! Surface relative des grains du sol
REAL, DIMENSION(KSIZE,NBIN) :: ZDSBIN                    !
REAL, DIMENSION(KSIZE,NBIN) :: ZGAMMA                    !
INTEGER, DIMENSION(NBIN+1)   :: ISEUIL
REAL, DIMENSION(NBIN,2)   :: ZPCEN
REAL, DIMENSION(NTEX)     :: ZZS0                      ! rugosité de la surface lisse
REAL, DIMENSION(NDP)      :: ZDP                       ! [µm] diamètre du particule
REAL :: ZDLNDP, ZRGH_Z0
INTEGER :: I                  !Counter for number of points (used in loops)
INTEGER :: IDP                !Counter for number of particle
INTEGER :: ITEX               !Counter for number of texture
INTEGER :: IS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DUSTFLUX_GET_MB',0,ZHOOK_HANDLE)
!
!Initialize mobilization candidate flag
GFLG_MBL(:) = .TRUE.
!fxm: Get erodibility limitation factor, use something connected to amount of sand
!Discuss with Valery Masson
!ZMBL_BSN_FCT(:) = PSAND(:)
! utilisé dans le calcul de l'effet Owen 
ZWND_RFR(:) = PWIND10M(:)
!
! Initialize vertical dust flux
ZFLX_MSS_VRT_DST_TTL_WBN(:,:) = 0.d0
ZFLX_MSS_HRZ_SLT_TTL_WBN(:,:) = 0.d0
!
PSFDST(:) = 0.0d0
!
! CLAY : CLAY >= 0.40 SILT < 0.40 SAND < 0.45
! SANDY CLAY : CLAY >= 0.36 SAND >= 0.45 
! SILTY CLAY : CLAY >= 0.40 SILT >= 0.40 
! SILT : SILT >= 0.8 CLAY < 0.12
! SAND : SAND >= 0.3*CLAY + 0.87
! SANDY CLAY LOAM : CLAY >= 0.28  CLAY < 0.36 SAND >= 0.45 | CLAY >= 0.20 CLAY < 0.28 SILT < 0.28
! SILTY CLAY LOAM : CLAY >= 0.28 CLAY < 0.40 SAND < 0.20
! CLAY LOAM : CLAY >= 0.28 CLAY < 0.40 SAND >= 0.20 SAND < 0.45
! SILT LOAM : SILT >= 0.8 CLAY >= 0.12 | SILT >= 0.5 SILT < 0.8 CLAY < 0.28
! LOAMY SAND : SAND >= CLAY + 0.7 SAND < 0.3*CLAY + 0.87
! SANDY LOAM : SAND >= 0.52  CLAY < 0.20 | SAND >= (0.5 - CLAY)  CLAY < 0.07
! LOAM : CLAY >= 0.20 CLAY < 0.28 SILT >= 0.28 SILT < 0.5 | SAND >= (0.5 - CLAY)  CLAY < 0.20
DO I = 1, KSIZE
  !
  ZSILT(I) = 1.0 - PCLAY(I) - PSAND(I)
  IF (ZSILT(I) <= 0.) ZSILT(I) = 0.0
  ZWPRM(I) = 3.0*PCLAY(I) * (0.17 + 0.0014 * PCLAY(I))
  ZWPRM(I) = MIN( 0.15, MAX(0.053, ZWPRM(I)) )
  ! 
  !tous les cas CLAY >= 0.28 sont traités
  IF ( PCLAY(I) >= 0.28 ) THEN
    IF ( PSAND(I) >= 0.45 ) THEN      
      IF (PCLAY(I) >= 0.36 ) THEN     ! Sandy Clay 
        ITEXT(I) = 9
        !ZWPRM(I) = 10.0E-2
      ELSE                            ! Sandy Clay Loam 
        ITEXT(I) = 6 
        !ZWPRM(I) = 6.0E-2
      ENDIF
    ELSEIF ( PCLAY(I) >= 0.40 ) THEN
      IF ( ZSILT(I) >= 0.40 ) THEN    ! Silty Clay 
        ITEXT(I) = 10
        !ZWPRM(I) = 10.5E-2
      ELSE                            ! Clay 
        ITEXT(I) = 11
        !ZWPRM(I) = 11.5E-2
      ENDIF
    ELSEIF (PSAND(I) >= 0.20 ) THEN  ! Clay Loam 
      ITEXT(I) = 8
      !ZWPRM(I) = 6.8E-2
    ELSE                             ! Silty Clay Loam 
      ITEXT(I) = 7
      !ZWPRM(I) = 6.8E-2
    ENDIF
  ENDIF
  ! les cas ZSILT >= 0.5 .AND. PCLAY < 0.28 sont traités
  ! les cas ZSILT < 0.5 .AND. PCLAY >= 0.20 sont traités 
  ! ne sont pas traités les cas PCLAY < 0.20 .AND. ZSILT < 0.5 => SAND >= 0.3
  IF ( ZSILT(I) >= 0.8 .AND. PCLAY(I) < 0.12 ) THEN ! Silt 
    ITEXT(I) = 12
    !ZWPRM(I) = 2.5E-2
  ELSEIF ( PCLAY(I) < 0.28 ) THEN    ! ( clay est forcément < 0.28 )
    IF ( ZSILT(I) >= 0.5 ) THEN      ! Silt Loam 
      ITEXT(I) = 4
      !ZWPRM(I) = 5.0E-2
    ELSEIF ( PCLAY(I) >= 0.20 ) THEN
      IF ( ZSILT(I) >= 0.28 ) THEN   ! Loam  
        ITEXT(I) = 5
        !ZWPRM(I) = 4.0E-2
      ELSE                           ! Sandy Clay Loam 
        ITEXT(I) = 6
        !ZWPRM(I) = 4.0E-2
      ENDIF
    ENDIF
  ENDIF
  ! les cas SAND >= 0.87 sont traités: silt < 0.13, clay < 0.1 => entrent dans les cas non encore traités
  ! les cas SAND >= 0.7 => CLAY < 0.15 , SILT < 0.3 sont traités ) => ""
  ! les cas SAND >=0.52 .AND. CLAY < 0.20 sont traités SILT < 0.48 => ""
  ! les cas restants considèrement pclay < 0.20, sand >= 0.5 - pclay => sand >=  0.3
  ! => clay + sand >= 0.5 => silt < 0.5
  IF ( PSAND(I) >= (0.3*PCLAY(I) + 0.87) ) THEN   ! Sand 
    ITEXT(I) = 1
    !ZWPRM(I) = 1.5E-2
  ELSEIF ( PSAND(I) >= (PCLAY(I) + 0.7) ) THEN    ! Loamy Sand
    ITEXT(I) = 2
    !ZWPRM(I) = 2.5E-2
  ELSEIF ( PSAND(I) >= 0.52 .AND. PCLAY(I) < 0.20 ) THEN ! Sandy Loam            
    ITEXT(I) = 3
    !ZWPRM(I) = 3.0E-2   
  ELSEIF ( PSAND(I) >= (0.5 - PCLAY(I)) ) THEN
    IF ( PCLAY(I) < 0.07 ) THEN                   ! Sandy Loam
      ITEXT(I) = 3
      !ZWPRM(I) = 3.0E-2
    ELSEIF ( PCLAY(I) < 0.20 ) THEN               ! Loam
      ITEXT(I) = 5
      !ZWPRM(I) = 4.0E-2
    ENDIF
  ENDIF
  !
ENDDO
!
ZDLNDP = 0.1d0        ! [µm]  Dln(DP)
 CALL DISTRIBUTION (NMODE, ZDLNDP, ITEXT, ZDP, ZDSRLV, ZZS0)
!
ISEUIL = (/0, 30, 40, 65, NDP/)
ZPCEN(:,1) = (/0.005, 0.006, 0.1, 0.10/)
ZPCEN(:,2) = (/0.005, 0.006, 1.0, 0.12/)
!
ZDSBIN(:,:) = 0.0
!
DO IS = 1, NBIN
  DO IDP = ISEUIL(IS)+1, ISEUIL(IS+1)
    DO I = 1, KSIZE
      ITEX = ITEXT(I)
      ZDSBIN(I,IS) = ZDSBIN(I,IS) + ZDSRLV(ITEX,IDP) / FLOAT(ISEUIL(IS+1)-ISEUIL(IS))
      IF (ITEX==4) THEN
        ZGAMMA(I,IS) = ZPCEN(IS,1)
      ELSE
        ZGAMMA(I,IS) = ZPCEN(IS,2)
      ENDIF
    ENDDO
  ENDDO
ENDDO
!
! Adjust threshold velocity for inhibition by roughness elements
DO I = 1, SIZE(ZFRC_THR_NCR_DRG)
  ITEX    = ITEXT(I)
  ZRGH_Z0 = PZ0(I)
  IF (ZRGH_Z0 <= ZZS0(ITEX)) ZRGH_Z0 = ZZS0(ITEX)
  ! Factor by which surface roughness increases threshold friction velocity 
  !++grini: fxm: USE WHOLE ARRAY OF Z0 INSTEAD OF ONLY RGH_MMN_MBL AS IN OLD CODE 
  CALL FRC_THR_NCR_DRG_GET(ZRGH_Z0, ZZS0(ITEX), ZFRC_THR_NCR_DRG(I))
ENDDO
!
! Convert volumetric water content to gravimetric water content
 CALL VWC2GWC(GFLG_MBL, PWSAT, PWG, ZGWC_SFC)
! Factor by which soil moisture increases threshold friction velocity 
 CALL FRC_THR_NCR_WTR_GET (GFLG_MBL, ZWPRM, ZGWC_SFC, ZFRC_THR_NCR_WTR)
ZFRC_THR_NCR_WTR(:) = MAX(ZFRC_THR_NCR_WTR(:), 1.0)
!
!
 CALL WND_FRC_THR_SLT_GET(PRHOA, XDMT_SLT_OPT, ZWND_FRC_THR_SLT)
!
DO I = 1,KSIZE
  IF (GFLG_MBL(I)) THEN
    ZWND_FRC_THR_SLT(I) = ZWND_FRC_THR_SLT(I) * ZFRC_THR_NCR_WTR(I) &  ! [frc] Adjustment for moisture
                                              * ZFRC_THR_NCR_DRG(I)    ! I [frc] Adjustment for roughness
    ZWND_RFR_THR_SLT(I) = ZWND_RFR(I) * ZWND_FRC_THR_SLT(I) / PUSTAR(I)
  ENDIF
ENDDO
!
! CHECK IF THIS CAN BE USED EASILY
! NEEDS 10M WIND SPEED WHICH IS MAYBE KNOWN MAYBE NOT !
! Saltation increases friction speed by roughening surface
 CALL WND_FRC_SLT_GET(GFLG_MBL, PUSTAR, ZWND_RFR, ZWND_RFR_THR_SLT, ZWND_FRC_SLT)
!
DO IS = 1,NBIN
  CALL FLX_MSS_HRZ_SLT_TTL_WHI79_GET(ZGAMMA(:,IS)*ZDSBIN(:,IS), GFLG_MBL, PRHOA, &
         ZWND_FRC_SLT, ZWND_FRC_THR_SLT, ZFLX_MSS_HRZ_SLT_TTL_WBN(:,IS))
ENDDO
!
! Vertical dust mass flux
DO IS = 1,NBIN
  CALL FLX_MSS_VRT_DST_AUST_GET(GFLG_MBL, PRHOA, ZWND_FRC_THR_SLT, ZFLX_MSS_HRZ_SLT_TTL_WBN(:,IS), & 
                      ZDST_SLT_FLX_RAT_TTL, ZFLX_MSS_VRT_DST_TTL_WBN(:,IS))
  !
  DO I = 1, KSIZE
    ! Vertically integrated streamwise mass flux in wind bin
    PSFDST(I) =  PSFDST(I) + PDST_EROD(I) * XFLX_MSS_FDG_FCTM * ZFLX_MSS_VRT_DST_TTL_WBN(I,IS)
  ENDDO
ENDDO
!
END SUBROUTINE DUSTFLUX_GET_MB 
