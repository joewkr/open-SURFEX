!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE REPROJ_DIAG_ISBA_n (DK, DEK, DMK, PEK, OSURF_BUDGET, OSURF_EVAP_BUDGET, &
                               OWATER_BUDGET, OSURF_MISC_BUDGET, OPROSNOW, &
                               OMEB_PATCH, PSLOPECOS    )  
!     ###############################################################################
!
!!****  *REPROJ_DIAG-ISBA_n * - additional diagnostics for ISBA
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Faroux 
!!
!!------------------------------------------------------------------
!
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_t), INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
LOGICAL, INTENT(IN) :: OSURF_BUDGET
LOGICAL, INTENT(IN) :: OSURF_EVAP_BUDGET 
LOGICAL, INTENT(IN) :: OWATER_BUDGET 
LOGICAL, INTENT(IN) :: OSURF_MISC_BUDGET
LOGICAL, INTENT(IN) :: OPROSNOW
!
LOGICAL, INTENT(IN) :: OMEB_PATCH
!
REAL, DIMENSION(:), INTENT(IN) :: PSLOPECOS ! cosine of the slope for Crocus
!    
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XPSN))    :: ZCORR_SLOPE
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZCORR_SLOPE_2D
!
INTEGER :: JL, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('REPROJ_DIAG_ISBA_N',0,ZHOOK_HANDLE)
!
IF ( OPROSNOW ) THEN
  !
  !this variable is used further to project diagnostics on the verticale
  ZCORR_SLOPE(:) = 1. / PSLOPECOS(:)
  DO JL = 1,SIZE(PEK%TSNOW%WSNOW,2)
    WHERE (PEK%TSNOW%WSNOW(:,JL)>0.) 
      ZCORR_SLOPE_2D(:,JL) = ZCORR_SLOPE(:)
    ELSEWHERE
      ZCORR_SLOPE_2D(:,JL) = 1.
    ENDWHERE
  ENDDO
  !
  IF ( OSURF_BUDGET ) THEN
    !
    DK%XRN    (:)    = DK%XRN         (:) * ZCORR_SLOPE(:)
    DK%XH     (:)    = DK%XH          (:) * ZCORR_SLOPE(:)
    DK%XGFLUX (:)    = DK%XGFLUX      (:) * ZCORR_SLOPE(:)
    DK%XLEI   (:)    = DK%XLEI        (:) * ZCORR_SLOPE(:)
    DK%XSWD   (:)    = DK%XSWD        (:) * ZCORR_SLOPE(:)
    DK%XSWU   (:)    = DK%XSWU        (:) * ZCORR_SLOPE(:)
    DK%XLWD   (:)    = DK%XLWD        (:) * ZCORR_SLOPE(:)
    DK%XLWU   (:)    = DK%XLWU        (:) * ZCORR_SLOPE(:)
    DK%XFMU   (:)    = DK%XFMU        (:) * ZCORR_SLOPE(:)
    DK%XFMV   (:)    = DK%XFMV        (:) * ZCORR_SLOPE(:)
    !
    DO JSW=1,SIZE(DK%XSWBD,2)
      DK%XSWBD   (:, JSW) = DK%XSWBD  (:,JSW) * ZCORR_SLOPE(:)
      DK%XSWBU   (:, JSW) = DK%XSWBU  (:,JSW) * ZCORR_SLOPE(:)
    ENDDO
    !
  END IF
  !
  IF ( OSURF_EVAP_BUDGET ) THEN
     !
     DK%XEVAP       (:)  =  DK%XEVAP        (:) * ZCORR_SLOPE(:)
     DK%XSUBL       (:)  =  DK%XSUBL        (:) * ZCORR_SLOPE(:)
     !
     DEK%XLEG       (:)  =  DEK%XLEG        (:) * ZCORR_SLOPE(:)
     DEK%XLEGI      (:)  =  DEK%XLEGI       (:) * ZCORR_SLOPE(:)
     DEK%XLEV       (:)  =  DEK%XLEV        (:) * ZCORR_SLOPE(:)
     DEK%XLES       (:)  =  DEK%XLES        (:) * ZCORR_SLOPE(:)
     DEK%XLER       (:)  =  DEK%XLER        (:) * ZCORR_SLOPE(:)
     DEK%XLETR      (:)  =  DEK%XLETR       (:) * ZCORR_SLOPE(:)
     DEK%XDRAIN     (:)  =  DEK%XDRAIN      (:) * ZCORR_SLOPE(:)
     DEK%XQSB       (:)  =  DEK%XQSB        (:) * ZCORR_SLOPE(:)
     DEK%XRUNOFF    (:)  =  DEK%XRUNOFF     (:) * ZCORR_SLOPE(:)
     DEK%XHORT      (:)  =  DEK%XHORT       (:) * ZCORR_SLOPE(:)
     DEK%XDRIP      (:)  =  DEK%XDRIP       (:) * ZCORR_SLOPE(:)
     DEK%XRRVEG     (:)  =  DEK%XRRVEG      (:) * ZCORR_SLOPE(:)
     DEK%XMELT      (:)  =  DEK%XMELT       (:) * ZCORR_SLOPE(:)
     DEK%XIFLOOD    (:)  =  DEK%XIFLOOD     (:) * ZCORR_SLOPE(:)
     DEK%XPFLOOD    (:)  =  DEK%XPFLOOD     (:) * ZCORR_SLOPE(:)
     DEK%XLE_FLOOD  (:)  =  DEK%XLE_FLOOD   (:) * ZCORR_SLOPE(:)
     DEK%XLEI_FLOOD (:)  =  DEK%XLEI_FLOOD  (:) * ZCORR_SLOPE(:)
     DEK%XIRRIG_FLUX(:)  =  DEK%XIRRIG_FLUX (:) * ZCORR_SLOPE(:)
     !
     IF ( OMEB_PATCH ) THEN
       !
       DEK%XLEV_CV   (:) = DEK%XLEV_CV   (:) * ZCORR_SLOPE(:)
       DEK%XLES_CV   (:) = DEK%XLES_CV   (:) * ZCORR_SLOPE(:)
       DEK%XLETR_CV  (:) = DEK%XLETR_CV  (:) * ZCORR_SLOPE(:)
       DEK%XLELITTER (:) = DEK%XLELITTER (:) * ZCORR_SLOPE(:)
       DEK%XLELITTERI(:) = DEK%XLELITTERI(:) * ZCORR_SLOPE(:)
       DEK%XDRIPLIT  (:) = DEK%XDRIPLIT  (:) * ZCORR_SLOPE(:)
       DEK%XRRLIT    (:) = DEK%XRRLIT    (:) * ZCORR_SLOPE(:)
       DEK%XLER_CV   (:) = DEK%XLER_CV   (:) * ZCORR_SLOPE(:)
       DEK%XLE_CA    (:) = DEK%XLE_CA    (:) * ZCORR_SLOPE(:)
       DEK%XLE_CV    (:) = DEK%XLE_CV    (:) * ZCORR_SLOPE(:)
       DEK%XLE_GV    (:) = DEK%XLE_GV    (:) * ZCORR_SLOPE(:)
       DEK%XLE_GN    (:) = DEK%XLE_GN    (:) * ZCORR_SLOPE(:)
       !
       DEK%XSWNET_V  (:) = DEK%XSWNET_V  (:) * ZCORR_SLOPE(:)
       DEK%XSWNET_G  (:) = DEK%XSWNET_G  (:) * ZCORR_SLOPE(:)
       DEK%XSWNET_N  (:) = DEK%XSWNET_N  (:) * ZCORR_SLOPE(:)
       DEK%XSWNET_NS (:) = DEK%XSWNET_NS (:) * ZCORR_SLOPE(:)
       DEK%XLWNET_V  (:) = DEK%XLWNET_V  (:) * ZCORR_SLOPE(:)
       DEK%XLWNET_G  (:) = DEK%XLWNET_G  (:) * ZCORR_SLOPE(:)
       DEK%XLWNET_N  (:) = DEK%XLWNET_N  (:) * ZCORR_SLOPE(:)
       DEK%XSWDOWN_GN(:) = DEK%XSWDOWN_GN(:) * ZCORR_SLOPE(:)
       DEK%XLWDOWN_GN(:) = DEK%XLWDOWN_GN(:) * ZCORR_SLOPE(:)
       DEK%XH_CV     (:) = DEK%XH_CV     (:) * ZCORR_SLOPE(:)
       DEK%XH_GV     (:) = DEK%XH_GV     (:) * ZCORR_SLOPE(:)
       DEK%XH_CA     (:) = DEK%XH_CA     (:) * ZCORR_SLOPE(:)
       DEK%XH_GN     (:) = DEK%XH_GN     (:) * ZCORR_SLOPE(:)
       DEK%XSR_GN    (:) = DEK%XSR_GN    (:) * ZCORR_SLOPE(:)
       DEK%XMELT_CV  (:) = DEK%XMELT_CV  (:) * ZCORR_SLOPE(:)
       DEK%XFRZ_CV   (:) = DEK%XFRZ_CV   (:) * ZCORR_SLOPE(:)
       !
     ENDIF
     !
     IF ( PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') THEN
       !
       DEK%XLESL    (:)  =  DEK%XLESL       (:) * ZCORR_SLOPE(:)
       DEK%XSNDRIFT (:)  =  DEK%XSNDRIFT    (:) * ZCORR_SLOPE(:)
       !
     END IF
     !
     IF ( OWATER_BUDGET )THEN
       !
       DEK%XDWG   (:)  =  DEK%XDWG   (:) * ZCORR_SLOPE(:)
       DEK%XDWGI  (:)  =  DEK%XDWGI  (:) * ZCORR_SLOPE(:)
       DEK%XDWR   (:)  =  DEK%XDWR   (:) * ZCORR_SLOPE(:)
       DEK%XDSWE  (:)  =  DEK%XDSWE  (:) * ZCORR_SLOPE(:)
       DEK%XWATBUD(:)  =  DEK%XWATBUD(:) * ZCORR_SLOPE(:)
       !
     ENDIF
     !
  END IF
  !
  IF (OSURF_MISC_BUDGET) THEN
    !
    DMK%XTWSNOW(:) = DMK%XTWSNOW(:) * ZCORR_SLOPE(:)
    DMK%XTDSNOW(:) = DMK%XTDSNOW(:) * ZCORR_SLOPE(:)
    !
    IF ( PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO' ) THEN
      !
      WHERE(DMK%XSNOWDZ/=XUNDEF)
        DMK%XSNOWLIQ(:,:) = DMK%XSNOWLIQ(:,:) * ZCORR_SLOPE_2D(:,:)
        DMK%XSNOWDZ (:,:) = DMK%XSNOWDZ (:,:) * ZCORR_SLOPE_2D(:,:)
      ENDWHERE
      !
      IF ( PEK%TSNOW%SCHEME=='CRO' ) THEN
        !PRINT*,ZCORR_SLOPE(:)
        !PRINT*,DMK%XSNDPT_1DY     (:)
        !PRINT*,DMK%XTDSNOW(:)
        !PRINT*,DMK%XTSNOW(:)
        WHERE(DMK%XTWSNOW>0.)
          DMK%XSNDPT_1DY     (:) = DMK%XSNDPT_1DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNDPT_3DY     (:) = DMK%XSNDPT_3DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNDPT_5DY     (:) = DMK%XSNDPT_5DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNDPT_7DY     (:) = DMK%XSNDPT_7DY     (:) * ZCORR_SLOPE(:)   
          DMK%XSNSWE_1DY     (:) = DMK%XSNSWE_1DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNSWE_3DY     (:) = DMK%XSNSWE_3DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNSWE_5DY     (:) = DMK%XSNSWE_5DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNSWE_7DY     (:) = DMK%XSNSWE_7DY     (:) * ZCORR_SLOPE(:)
          DMK%XSNRAM_SONDE   (:) = DMK%XSNRAM_SONDE   (:) * ZCORR_SLOPE(:)
          DMK%XSN_REFRZNTHCKN(:) = DMK%XSN_REFRZNTHCKN(:) * ZCORR_SLOPE(:) 
          DMK%XSN_WETTHCKN   (:) = DMK%XSN_WETTHCKN   (:) * ZCORR_SLOPE(:)
        ENDWHERE
      ENDIF
      !
    ENDIF
    !
  ENDIF
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('REPROJ_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE REPROJ_DIAG_ISBA_n
