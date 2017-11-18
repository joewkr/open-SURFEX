!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_SOILSTRESS
CONTAINS
SUBROUTINE SOILSTRESS( HISBA, PF2, KK, PK, PEK, PF2WGHT, PF5        )
!     ####################################################################
!
!!****  *SOILSTRESS*
!!
!!    PURPOSE
!!    -------
!
!     Calculates the vegetation stress due to soil water
!
!
!!**  METHOD
!!    ------
!
!     Calculates the F2 coefficient.
!
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    none
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
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      13/03/95
!!     (P.Jabouille)  13/11/96    mininum value for ZF1
!!     (V. Masson)    28/08/98    add PF2 for Calvet (1998) CO2 computations
!!     (B. Decharme)     07/15    Suppress numerical adjustement for PF2
!!     (B. Decharme)     01/17    Suppress soil/vegetation parameters modification
!!                                for DIF due to the presence of ice to ensure maximum
!!                                vegetation stress when soil ice is important. Indeed,
!!                                soil ice acts as drought events for vegetation stress
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_ISBA_PAR,  ONLY : XDENOM_MIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=*),     INTENT(IN)   :: HISBA   ! type of soil (Force-Restore OR Diffusion)
!                                             ! '2-L'
!                                             ! '3-L'
!                                             ! 'DIF'   ISBA-DF
!
REAL, DIMENSION(:), INTENT(OUT)  :: PF2      ! water stress coefficient
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
REAL, DIMENSION(:), INTENT(OUT)  :: PF5      ! water stress coefficient for Hv (based on F2):
!                                            ! Verify that Etv=>0 as F2=>0
!
REAL, DIMENSION(:,:), INTENT(OUT):: PF2WGHT  ! water stress coefficient profile (ISBA-DF)
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(KK%XWFC,1)) ::  ZWFC_AVGZ, ZWSAT_AVGZ, ZWWILT_AVGZ
!                                  ZWFC_AVGZ   = field capacity averaged over entire soil column
!                                  ZWSAT_AVGZ  = porosity averaged over entire soil column
!                                  ZWWILT_AVGZ = wilting point averaged over entire soil column
!
REAL    :: ZROOTFRACN
!          ZROOTFRACN = Normalized root fraction weights
!
INTEGER :: INI, INL, JJ, JL, IDEPTH
!
!
!*      0.3    declarations of local parameters:
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization of variables:
!               ----------------------------
!
IF (LHOOK) CALL DR_HOOK('SOILSTRESS',0,ZHOOK_HANDLE)
!
INI=SIZE(PEK%XWG,1)
IF (SIZE(PK%NWG_LAYER)>0) THEN
  INL=MAXVAL(PK%NWG_LAYER(:))
ELSE
  INL=0
ENDIF
!
PF2    (:)      = 0.0
PF2WGHT(:,:)    = 0.0
!
ZWFC_AVGZ(:)    = 0.
ZWSAT_AVGZ(:)   = 0.
ZWWILT_AVGZ(:)  = 0.
!
!-------------------------------------------------------------------------------
!
!*       2.     THE 'PF2' FACTOR
!               ----------------
!               This factor takes into account the effect
!               of the water stress on the surface
!               resistance
!
! - For humid soils (> WFC), this factor does not
!   increase the stomatal resistance
! - The stomatal resistance should be large
!   when the soil is very dry (< WILT)
!
IF(HISBA =='DIF')THEN
!
! If using the diffusion option, then calculate transpiration weights
! and the mean root-zone soil water stress factor F2:
!
!---------------------------------------------------------
! First layer
!---------------------------------------------------------
!
! Calculate the soil water stress factor for each layer:
  PF2WGHT(:,1) = (PEK%XWG(:,1)-KK%XWWILT(:,1))/(KK%XWFC(:,1)-KK%XWWILT(:,1))
!
! Normalize the transpiration weights by root fraction:
  PF2WGHT(:,1) = PK%XROOTFRAC(:,1)*MAX(0.0,MIN(1.0,PF2WGHT(:,1)))
!
! Net soil water stress for entire root zone:
  PF2(:) = PF2WGHT(:,1)
!
!---------------------------------------------------------
! Other layers
!---------------------------------------------------------
!
  DO JL=2,INL
     DO JJ=1,INI
!
      IDEPTH=PK%NWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
!
!       Calculate normalized root fraction weights:
        ZROOTFRACN = PK%XROOTFRAC(JJ,JL) - PK%XROOTFRAC(JJ,JL-1)
!
!       Calculate the soil water stress factor for each layer:
        PF2WGHT(JJ,JL) = (PEK%XWG(JJ,JL)-KK%XWWILT(JJ,1))/(KK%XWFC(JJ,1)-KK%XWWILT(JJ,1))
!
!       Normalize the transpiration weights by root fraction:
        PF2WGHT(JJ,JL) = ZROOTFRACN*MAX(0.0,MIN(1.0,PF2WGHT(JJ,JL)))
!
!       Net soil water stress for entire root zone:
        PF2(JJ)        = PF2(JJ) + PF2WGHT(JJ,JL)
!
      ENDIF
!
     ENDDO
  ENDDO
!
ELSE
!
! When using the Force-Restore (FR) soil option, the soil properties
! are assumed to be homogeneous in the vertical. Therefore, simply
! use the properties for the uppermost soil layer when defining
! soil properties for local computations.
!
! Due to the presence of ice, modify soil parameters:
!
   ZWSAT_AVGZ(:)  = KK%XWSAT (:,1) - PEK%XWGI(:,2)
   ZWFC_AVGZ(:)   = KK%XWFC  (:,1)*ZWSAT_AVGZ(:)/KK%XWSAT(:,1)
   ZWWILT_AVGZ(:) = KK%XWWILT(:,1)*ZWSAT_AVGZ(:)/KK%XWSAT(:,1)
!
! Compute the water stress factor:
!
   PF2(:) = (PEK%XWG(:,2)-ZWWILT_AVGZ(:))/(ZWFC_AVGZ(:)-ZWWILT_AVGZ(:))
   PF2(:) = MAX(0.0,MIN(1.0, PF2(:)))
!
!
ENDIF
!
! Function to cause Etv to approach 0 as F2 goes to 0:
!
PF5(:) = PF2(:)
!
IF (LHOOK) CALL DR_HOOK('SOILSTRESS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOILSTRESS
END MODULE MODI_SOILSTRESS
