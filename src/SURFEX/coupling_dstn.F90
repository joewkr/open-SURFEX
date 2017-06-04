!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE COUPLING_DST_n (DSTK, KK, PK, PEK, DK, &
       HPROGRAM,                 &!I [char] Type of ISBA version
       KI,                       &!I [nbr] number of points in patch
       KDST,                     &!I Number of dust emission variables
       PPS,                      &!I [Pa] surface pressure
       PQA,                      &!I [kg/kg] atmospheric specific humidity
       PRHOA,                    &!I [kg/m3] atmospheric density
       PPA,                      &!I [K] Atmospheric pressure
       PTA,                      &!I [K] Atmospheric temperature
       PU,                       &!I [m/s] zonal wind at atmospheric height 
       PUREF,                    &!I [m] reference height of wind
       PV,                       &!I [m/s] meridional wind at atmospheric height
       PZREF,                    &!I [m] reference height of wind
       PSFDST                    &!O [kg/m2/sec] flux of dust
       )  
  
!PURPOSE
!-------
!Recieve a full vector for a given patch containing dust emitter surface points
!Pack it to vectors for which are purely dust emitter surfaces
!Send back the vector PSFDST (kg/m2/s) coming from this patch.
!In COUPLING_ISBA this will again be weighted by fraction_of_patch_in_nature_point

!AUTHOR
!-------
!ALF GRINI <alf.grini@cnrm.meteo.fr>  2005
! Modification P. Tulet introduce friction velocity for mode repartition upon 
! Alfaro et al, 1998 (Geo. Res. Lett.)
!
!!      Modified    09/2012  : J. Escobar , SIZE(PTA) not allowed without-interface , replace by KI
!
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE MODD_DST_n, ONLY : DST_t
!
USE MODD_CSTS, ONLY : XRD                      ! [J/K/kg] The universal gas constant  
       

USE MODD_DST_SURF

USE MODI_SURFACE_CD
USE MODI_CLS_WIND
USE MODI_CLS_T

USE MODI_DUSTFLUX_GET     ! Dust mobilization routines
USE MODI_DUSTFLUX_GET_MB  ! Dust mobilization routines (M. Mokhtari)  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!INPUT
!
TYPE(DST_t), INTENT(INOUT) :: DSTK
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(DIAG_t), INTENT(INOUT) :: DK
!
 CHARACTER(LEN=*), INTENT(IN)       :: HPROGRAM       !I Name of program
INTEGER, INTENT(IN)                :: KI             !I Number of points in patch
INTEGER, INTENT(IN)                :: KDST           !I Number of dust emission variables
REAL, DIMENSION(KI), INTENT(IN)    :: PPS            !I [Pa] surface pressure
REAL, DIMENSION(KI), INTENT(IN)    :: PQA            !I [kg/kg] atmospheric specific humidity
REAL, DIMENSION(KI), INTENT(IN)    :: PRHOA          !I [kg/m3] atmospheric density
REAL, DIMENSION(KI), INTENT(IN)    :: PPA            !I [K] Atmospheric pressure
REAL, DIMENSION(KI), INTENT(IN)    :: PTA            !I [K] Atmospheric temperature
REAL, DIMENSION(KI), INTENT(IN)    :: PU             !I [m/s] zonal wind at atmospheric height 
REAL, DIMENSION(KI), INTENT(IN)    :: PUREF          !I [m] reference height of wind
REAL, DIMENSION(KI), INTENT(IN)    :: PV             !I [m/s] meridional wind at atmospheric height
REAL, DIMENSION(KI), INTENT(IN)    :: PZREF          !I [m] reference height of wind
REAL, DIMENSION(KI,KDST), INTENT(OUT) :: PSFDST      !O [kg/m2/sec] flux of dust for a patch
  
!LOCAL VARIABLES
REAL, DIMENSION(KI,NVEGNO_DST,NDSTMDE) :: ZSFDST_TILE  ![kg/m2] flux of dust for each vegetation types and each mode
INTEGER                            :: JVEG           ![idx] counter for vegetation types
REAL(KIND=JPRB) :: ZHOOK_HANDLE


!Initialize output which is total flux of dust (kg/m2/sec) from this patch
!This number will get weighted by fraction of patch later on (in coupling_isban)
IF (LHOOK) CALL DR_HOOK('COUPLING_DST_N',0,ZHOOK_HANDLE)
PSFDST(:,:)=0.d0

!Initiate dust emissions for all points in patch
ZSFDST_TILE(:,:,:)=0.d0

DO JVEG=1,NVEGNO_DST

  !Jump out of loop if no dust emitter points
  IF (DSTK%NSIZE_PATCH_DST(JVEG)==0) CYCLE

   CALL TREAT_SURF(  &
      DSTK%NSIZE_PATCH_DST(JVEG),  &!I[idx] number of dust emitter points in patch
      DSTK%NR_PATCH_DST (:,JVEG)   &!I[idx] index translator from patch to dustemitter
            )  
   
ENDDO  !Loop on dust emitter vegetation
  
!Average dust flux from the two vegetation "tiles"
!Make sure you do this correctly so that area is OK.
!Need to have this as kg/m2/sec from PATCH because
!afterwards this is weighted by fraction of patch
 CALL AVG_FLUX_DST(NVEGNO_DST)
  
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
IF (LHOOK) CALL DR_HOOK('COUPLING_DST_N',1,ZHOOK_HANDLE)
!
CONTAINS
    
SUBROUTINE TREAT_SURF(KSIZE,KMASK)
!
USE MODD_DIAG_n, ONLY : DIAG_t
!
IMPLICIT NONE

INTEGER, INTENT(IN)    :: KSIZE  !Size of dust emitter vector
INTEGER, DIMENSION(:), INTENT(IN)    :: KMASK !mask from patch vector to dust emitter vector

!ALLOCATABLE VARIABLES FOR ALL VEGETATION TYPES
REAL, DIMENSION(KSIZE) :: ZP_CLAY       ![frc] fraction of clay
REAL, DIMENSION(KSIZE) :: ZP_SAND       ![frc] fraction of sand
!
REAL, DIMENSION(KSIZE) :: ZP_PA         ![K] Atmospheric pressure
REAL, DIMENSION(KSIZE) :: ZP_TA         ![K] Atmospheric temperature
REAL, DIMENSION(KSIZE) :: ZP_QA         ![kg_{H2O}/kg_{air}] specific humidity
REAL, DIMENSION(KSIZE) :: ZP_RHOA_2M    ![kg/m3] atmospheric density close to surface
REAL, DIMENSION(KSIZE) :: ZP_SFMER      ![m/s] wind friction in meridional direction
REAL, DIMENSION(KSIZE) :: ZP_SFZON      ![m/s] wind friction in zonal direction
REAL, DIMENSION(KSIZE) :: ZP_U          ![m/s] zonal wind at atmospheric height
REAL, DIMENSION(KSIZE) :: ZP_V          ![m/s] meridional wind at atmospheric height
REAL, DIMENSION(KSIZE) :: ZP_VMOD       ![m/s] Wind at atmospheric height
!
REAL, DIMENSION(KSIZE) :: ZP_SFDST      ![kg/m2/s] dust flux (kg/m2/s)
REAL, DIMENSION(KSIZE,NDSTMDE) :: ZP_SFDST_MDE ![kg/m2/s] dust flux from modes
!
REAL, DIMENSION(KSIZE) :: ZP_TG         ![K] ground temperature
REAL, DIMENSION(KSIZE) :: ZP_WG         ![m3/m3] ground volumetric water content
REAL, DIMENSION(KSIZE) :: ZP_WSAT       ![m3/m3] saturation volumetric water content
REAL, DIMENSION(KSIZE) :: ZP_USTAR      ![m/s] wind friction speed
!
REAL, DIMENSION(KSIZE) :: ZP_Z0_EROD    ![m] roughness length for erodible surface
REAL, DIMENSION(KSIZE) :: ZP_DST_EROD      !Erodable Surface (COVER004=1 and COVER005=0.01)
REAL, DIMENSION(KSIZE,3) :: ZP_MSS_FRC_SRC ![%] dust mode fraction of emitted mass 
REAL, DIMENSION(KSIZE) :: ZP_INTER ![%] dust mode fraction of emitted mass 
REAL, DIMENSION(KSIZE) :: ZP_CD_DST     ! drag coefficient uses by DEAD
REAL, DIMENSION(KSIZE) :: ZH               ! 10m (wind altitude)
!
REAL, DIMENSION(KSIZE) :: ZP_MER10M     ![m/s] meridional wind at 10m
REAL, DIMENSION(KSIZE) :: ZP_WIND10M    ![m/s] wind at 10m
REAL, DIMENSION(KSIZE) :: ZP_ZON10M     ![m/s] zonal wind at 10m
REAL, DIMENSION(KSIZE) :: ZP_PS         ![Pa] surface pressure
REAL, DIMENSION(KSIZE) :: ZP_TS         ![K] surface temperature
REAL, DIMENSION(KSIZE) :: ZP_Q2M        ![kg_{H2O}/kg_{air}] speficic humidity
REAL, DIMENSION(KSIZE) :: ZP_HU2M       ![-] relative humidity
REAL, DIMENSION(KSIZE) :: ZP_RHOA       ![kg/m3] atmospheric density
REAL, DIMENSION(KSIZE) :: ZP_T2M        ![K] 2M temperature
REAL, DIMENSION(KSIZE) :: ZP_UREF       ![m] reference height for wind
REAL, DIMENSION(KSIZE) :: ZP_ZREF       ![m] reference height for wind
REAL, DIMENSION(KSIZE) :: ZP_CD         !I [] drag coefficient for momentum from isba
REAL, DIMENSION(KSIZE) :: ZP_CH         !I [] drag coefficient for heat
REAL, DIMENSION(KSIZE) :: ZP_CDN        ![-] drag coefficient neutral atm
REAL, DIMENSION(KSIZE) :: ZP_RI         !I [-] Richardson number
REAL, DIMENSION(KSIZE) :: ZP_Z0H        ![frc] Z0 heat with snow 
!
REAL, DIMENSION(5)   :: ZSEUIL
REAL, DIMENSION(6,2) :: ZPCEN
INTEGER                :: JJ, JS            !Counter for vector points
INTEGER                :: JMODE         
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('COUPLING_DST_n:TREAT_SURF',0,ZHOOK_HANDLE)

!Pack patch-vectors to dust emitter vectors
!i.e. allocate memory for the packed (dust emitter) vectors
DO JJ=1,KSIZE
  ZP_CLAY(JJ) = KK%XCLAY(KMASK(JJ),1)  
  ZP_PS  (JJ) = PPS     (KMASK(JJ))
  ZP_TS  (JJ) = DK%XTS  (KMASK(JJ))
  ZP_QA  (JJ) = PQA     (KMASK(JJ))
  ZP_RHOA(JJ) = PRHOA   (KMASK(JJ))
  ZP_SAND(JJ) = KK%XSAND(KMASK(JJ),1)
  ZP_PA  (JJ) = PPA     (KMASK(JJ)) 
  ZP_TA  (JJ) = PTA     (KMASK(JJ)) 
  ZP_TG  (JJ) = PEK%XTG (KMASK(JJ),1)
  ZP_U   (JJ) = PU      (KMASK(JJ))
  ZP_UREF(JJ) = PUREF   (KMASK(JJ)) 
  ZP_V   (JJ) = PV      (KMASK(JJ))
  ZP_WG  (JJ) = PEK%XWG (KMASK(JJ),1)
  ZP_WSAT(JJ) = KK%XWSAT(KMASK(JJ),1)                      
  ZP_ZREF(JJ) = PZREF   (KMASK(JJ))    
  ZP_CD  (JJ) = DK%XCD  (KMASK(JJ))
  ZP_CDN (JJ) = DK%XCDN (KMASK(JJ))
  ZP_CH  (JJ) = DK%XCH  (KMASK(JJ))
  ZP_RI  (JJ) = DK%XRI  (KMASK(JJ))
  ZP_Z0H (JJ) = DK%XZ0H (KMASK(JJ))
ENDDO
!
!Manipulate some variables since we assume dust emission occurs over flat surface
ZP_Z0_EROD(:) = DSTK%Z0_EROD_DST(JVEG)   !Set z0 to roughness of erodible surface
!
IF (JVEG ==1) THEN
  ZP_DST_EROD(:) = 1.
ELSE
  ZP_DST_EROD(:) = 0.01
ENDIF
!
ZP_CD_DST(:) = ZP_CD(:)
!
IF (CVERMOD/='CMDVER') THEN
  !Re-calculate the drag over the dust emitter (erodable) surface. Since we
  !don't use the roughness length of the patch, but rather use specific roughness
  !lengths of dust emitter surfaces, the drag changes.  
  CALL SURFACE_CD(ZP_RI, ZP_ZREF, ZP_UREF, ZP_Z0_EROD, ZP_Z0H, ZP_CD_DST, ZP_CDN)
ENDIF
!
!Get total wind speed
ZP_VMOD(:) = SQRT(ZP_U(:)**2 + ZP_V(:)**2)  
!
ZP_USTAR(:) =  SQRT(ZP_CD_DST(:))*ZP_VMOD
!Get zonal friction speed (m/s)
ZP_SFZON(:) = - SQRT(ZP_CD_DST(:))*ZP_U(:)
!Get meridional surface stress
ZP_SFMER(:) = - SQRT(ZP_CD_DST(:))*ZP_V(:)
!
!Get the 10m wind speed (needed for one operation in dust model)
!And get the density at 2m to feed into erosion model
ZH(:)=10.
 CALL CLS_WIND(ZP_U, ZP_V, ZP_UREF, ZP_CD_DST, ZP_CDN, ZP_RI, ZH, &
                ZP_ZON10M, ZP_MER10M                 )
ZH(:)=2.
 CALL CLS_T(ZP_TA, ZP_QA, ZP_PA, ZP_PS, ZP_ZREF, ZP_CD_DST, ZP_CH, ZP_RI,   &
              ZP_TS, ZP_Z0H, ZH, ZP_T2M  )

!Get density at 2 meters rho=P/(RT)
ZP_RHOA_2M(:) = ZP_PS(:)/(ZP_T2M(:)*XRD)
!
!Get wind speed at 10m heigh
ZP_WIND10M(:) = SQRT(ZP_ZON10M(:)**2 + ZP_MER10M(:)**2)
!     
!the erodible surface
ZP_WIND10M(:) = MAX(ZP_WIND10M(:), 1E-2)
!
IF (CVERMOD=='CMDVER') THEN
  !
  CALL DUSTFLUX_GET_MB(               &
       ZP_USTAR,                      & !I [m/s] wind friction speed over erodible surface
       ZP_RHOA_2M,                    & !I [kg/m3] air density at surface
       ZP_WG,                         & !I [m3/m3] volumetric water content of ground
       ZP_Z0_EROD,                    & !I [m] roughness length of erodible surface
       ZP_WSAT,                       & !I [m3/m3] saturation water volumetric content
       ZP_CLAY,                       & !I [frc] mass fraction of clay
       ZP_SAND,                       & !I [frc] mass fraction of sand
       ZP_DST_EROD,                   & !I [frc] erodabilitC) de la surface (cover004 = 1 et cocer005=0.5)
       ZP_WIND10M,                    & !I [m/s] wind at 10m 
       ZP_SFDST,                      & !O [kg/m2/sec] flux of dust 
       KSIZE                          & !I [nbr] number of points for which we do calculation
       )
  !
ELSE
  !
  CALL DUSTFLUX_GET(                        &
           ZP_USTAR,                        &!I [m/s] wind friction speed over erodible surface
           ZP_RHOA_2M,                      &!I [kg/m3] air density at surface
           ZP_WG,                           &!I [m3/m3] volumetric water content of ground
           ZP_Z0_EROD,                      &!I [m] roughness length of erodible surface
           ZP_WSAT,                         &!I [m3/m3] saturation water volumetric content
           ZP_CLAY,                         &!I [frc] mass fraction of clay
           ZP_SAND,                         &!I [frc] mass fraction of sand
           ZP_WIND10M,                      &!I [m/s] wind at 10m 
           ZP_SFDST,                        &!O [kg/m2/sec] flux of dust 
           KSIZE                            &!I [nbr] number of points for which we do calculation
           )
ENDIF  
    
ZP_MSS_FRC_SRC(:,:) = 0.

IF (CEMISPARAM_DST == "EXPLI" .OR. CEMISPARAM_DST == "AMMA ") THEN

  ! For the repartition of mass fraction we uses the real USTAR
  ZP_USTAR(:) =  sqrt(ZP_CD_DST(:))*ZP_VMOD(:)

  ZSEUIL      = (/0., 0.35, 0.42, 0.50 ,0.66/)

  IF (CEMISPARAM_DST == "EXPLI") THEN
    ! Compute modes mass fraction of emitted dust upon Alfaro et al. 1998
    ZPCEN(:,1)  = (/0., 0., 0.01, 0.08, 0.15, 0.15/)
    ZPCEN(:,2)  = (/0., 0., 0.36, 0.43, 0.76, 0.76/)
    ! Case of u* < 35E-2 m/s
    ! only coarse mode is activated 
    ! mode 1 = 0
    ! mode 2 = 0
    ! mode 3 = 100      
    ! Case of  35E-2 m/s < u* < 42E-2 m/s
    !  0 % < mode 1 < 1 %
    !  0 % < mode 2 < 36 %
    !  63 % < mode 3 < 100 %
    ! Case of  42E-2 m/s < u* < 50E-2 m/s
    !  1 % < mode 1 < 8 %
    !  36 % < mode 2 < 43 %
    !  49 % < mode 3 < 63 %
    ! Case of  50E-2 m/s < u* < 66E-2 m/s
    !  8 % < mode 1 < 15 %
    !  43 % < mode 2 < 76 %
    !  9  % < mode 3 < 49 %
    ! Case of  u* > 66E-2 m/s 
    !   mode 1 = 15 %
    !   mode 2 = 76 %
    !   mode 3 = 9 %
  ELSEIF (CEMISPARAM_DST == "AMMA ") THEN
    ! For the repartition of mass fraction we uses the real USTAR upon
    ! Alfaro/Gomes 2001, jgr, table 5, soil type Salty sand.
    ! Percentage of mass considered between the fin and accumulation modes
    ZPCEN(:,1) = (/0., 0., 0.0023, 0.0185, 0.0345, 0.0345/)
    ZPCEN(:,2) = (/0., 0., 0.0077, 0.0615, 0.1155, 0.1155/)
    ! Case of u* < 35E-2 m/s
    ! only coarse mode is activated 
    ! mode 1 = 0
    ! mode 2 = 0
    ! mode 3 = 100 
    ! Case of  35E-2 m/s < u* < 42E-2 m/s
    !  0 % < mode 1 < 0.23 %
    !  0 % < mode 2 < 0.77 %
    !  99 % < mode 3 < 100 % 
    ! Case of  42E-2 m/s < u* < 50E-2 m/s
    !  0.23 % < mode 1 < 1.85 %
    !  0.77 % < mode 2 < 6.15 %
    !  92 % < mode 3 < 99 %    
    ! Case of  50E-2 m/s < u* < 66E-2 m/s
    !  1.85 % < mode 1 < 3.45 %
    !  6.15 % < mode 2 < 11.55 %
    !  85  % < mode 3 < 92 %
    ! Case of  u* > 66E-2 m/s 
    !   mode 1 = 15 %
    !   mode 2 = 76 %
    !   mode 3 = 9 %
  ENDIF

  DO JS = 1, SIZE(ZSEUIL) - 1
    WHERE (ZP_USTAR(:) >= ZSEUIL(JS) .AND. ZP_USTAR(:) < ZSEUIL(JS+1))
      ZP_INTER(:)         = (ZP_USTAR(:) - ZSEUIL(JS)) / (ZSEUIL(JS+1) - ZSEUIL(JS))
      ZP_MSS_FRC_SRC(:,1) = ZPCEN(JS,1) + (ZPCEN(JS+1,1) - ZPCEN(JS,1)) * ZP_INTER(:)
      ZP_MSS_FRC_SRC(:,2) = ZPCEN(JS,2) + (ZPCEN(JS+1,2) - ZPCEN(JS,2)) * ZP_INTER(:) 
    END WHERE
  ENDDO

  WHERE (ZP_USTAR(:) >= ZSEUIL(SIZE(ZSEUIL)))
    ZP_MSS_FRC_SRC(:,1) = ZPCEN(SIZE(ZSEUIL)+1,1) 
    ZP_MSS_FRC_SRC(:,2) = ZPCEN(SIZE(ZSEUIL)+1,2)
  END WHERE

  ZP_MSS_FRC_SRC(:,3) = 1. - ZP_MSS_FRC_SRC(:,1) - ZP_MSS_FRC_SRC(:,2)

ELSE 
  DO JMODE = 1,NDSTMDE
    ZP_MSS_FRC_SRC(:,JORDER_DST(JMODE)) = DSTK%XMSS_FRC_SRC(JMODE)
  ENDDO
END IF

DO JMODE=1,NDSTMDE
  ! Explicit mass fraction from u*
  ZP_SFDST_MDE(:,JMODE) = ZP_SFDST(:)                        &  !Total mass flux of dust
                          * ZP_MSS_FRC_SRC(:,JORDER_DST(JMODE)) !Fraction of dust going to each mode
ENDDO


!Transfer the fluxes to each tile
DO JMODE=1,NDSTMDE
  DO JJ=1,KSIZE
    ZSFDST_TILE(KMASK(JJ),JVEG,JMODE) = ZP_SFDST_MDE(JJ,JMODE)
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('COUPLING_DST_n:TREAT_SURF',1,ZHOOK_HANDLE)

END SUBROUTINE TREAT_SURF
   
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE AVG_FLUX_DST(   &
    KTILE                  & ! Number of different dust emitter vegetations
           )  

! Purpose: Average all fluxes from independent dust emitter surfaces (KTILE)
! Remember: XP_VEGTYPE_PATCH is m^2_{emittersurface}/m^{nature}
! The goal is to obtain PSFDST in kg_{dust}/m^2_{patch}

!INPUT
INTEGER, INTENT(IN)     :: KTILE                    ! Number of different dust emitter vegetation types

!LOCAL
INTEGER                 :: II                       ! Counter for points in patch-vector
INTEGER                 :: JJ                       ! Counter for vegetation types
INTEGER                 :: JMODE                    ! Counter for modes
INTEGER                 :: JSV_IDX                  ! Index for scalar variable
INTEGER                 :: NMOMENT                  ! Number of moments
REAL                    :: VEGFRAC_IN_PATCH         ! fraction of total vegetation in this patch (m^2_{patch}/m^2_{nature})
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!Remember: XP_VEGTYPE_PATCH is m^2_{emittersurface}/m^{nature}
!The goal is to obtain PSFDST in kg_{dust}/m^2_{patch}
!
!Start loop on modes
IF (LHOOK) CALL DR_HOOK('AVG_FLUX_DST',0,ZHOOK_HANDLE)
!
NMOMENT = INT(SIZE(PSFDST,2) / NDSTMDE)
!
DO JMODE=1,NDSTMDE
  !Make sure dust mass emission is put in index 2, 5, 8 etc if 3 moments per mode
  !Make sure dust mass emission is put in index 2, 4, 6 etc if 2 moments per mode
  IF (NMOMENT == 1) THEN
    JSV_IDX = (JMODE-1)*NMOMENT + 1  !Counter for scalar variable
  ELSE
    JSV_IDX = (JMODE-1)*NMOMENT + 2  !Counter for scalar variable
  END IF
        
  !Start loop on number of dust emitter surfaces
  DO JJ=1,KTILE
    !Loop on points inside patch
    DO II=1,KI
               
      !Get sum of vegetation fraction in this patch
      !fxm: VERY BAD LOOP ORDER
      VEGFRAC_IN_PATCH = SUM(PK%XVEGTYPE_PATCH(II,:))
               
      !Get production of flux by adding up the contribution 
      !from the different tiles (here, "tiles" are dust emitter surfaces)
      PSFDST(II,JSV_IDX) = PSFDST(II,JSV_IDX)                  & ![kg/m^2_{patch}/sec] dust flux per patch 
                           + (ZSFDST_TILE(II,JJ,JMODE)         & ![kg/m^2_{emittersurface}/sec] Dust flux per surface area of dust emitter surface
                           * PK%XVEGTYPE_PATCH(II,DSTK%NVT_DST(JJ))  & ![frc] m^2_{emittersurface}/m^2_{nature}
                           / VEGFRAC_IN_PATCH )                  ![frc] m^2_{patch}/m^2_{nature}  
    ENDDO !loop on point in patch
  ENDDO    !loop on different dust emitter surfaces
         
ENDDO !Loop on modes

IF (LHOOK) CALL DR_HOOK('AVG_FLUX_DST',1,ZHOOK_HANDLE)
      
END SUBROUTINE AVG_FLUX_DST
    
END SUBROUTINE COUPLING_DST_n

