!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################################################################
      SUBROUTINE DRY_WET_SOIL_ALBEDOS( KK  )  
!     ##################################################################
!
!!****  *DRY_WET_SOIL_ALBEDOS*  
!!
!!    PURPOSE
!!    -------
!       computes the albedo of bare soil, for dry or wet conditions
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
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/12/99 
!       
!      (V. Masson)  16/02/01 Better fit with ISLSCP2; 
!                                            Ba et al 2001; 
!                                            Pinty et al 2000
!      (V. Masson) 01/2004  Add UV albedo
!      (R. Alkama) 05/2012  Add 7 new vegtype (19 rather than 12)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_PARK, NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, &
                                NVT_C3, NVT_C4, NVT_IRR, NVT_GRAS, NVT_BOGR,      &
                                NVT_TROG, NVT_C3W, NVT_C3S, NVT_FLTR, NVT_FLGR                 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
!
REAL :: ZSUM
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DRY_WET_SOIL_ALBEDOS',0,ZHOOK_HANDLE)
!
DO JJ=1,SIZE(KK%XVEGTYPE,1)
  !
  ZSUM = KK%XVEGTYPE(JJ,NVT_C4) + KK%XVEGTYPE(JJ,NVT_GRAS) + KK%XVEGTYPE(JJ,NVT_TROG) &
       + KK%XVEGTYPE(JJ,NVT_TRBE) + KK%XVEGTYPE(JJ,NVT_BONE) + KK%XVEGTYPE(JJ,NVT_TEBD) &
       + KK%XVEGTYPE(JJ,NVT_TRBD) + KK%XVEGTYPE(JJ,NVT_TEBE) + KK%XVEGTYPE(JJ,NVT_TENE) &
       + KK%XVEGTYPE(JJ,NVT_BOBD) + KK%XVEGTYPE(JJ,NVT_BOND) + KK%XVEGTYPE(JJ,NVT_BOGR) &
       + KK%XVEGTYPE(JJ,NVT_SHRB)
  !
  IF (NVT_C3/=0 .AND. NVT_IRR/=0) THEN
    ZSUM = ZSUM + KK%XVEGTYPE(JJ,NVT_C3) + KK%XVEGTYPE(JJ,NVT_IRR)
  ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
    ZSUM = ZSUM + KK%XVEGTYPE(JJ,NVT_C3W) + KK%XVEGTYPE(JJ,NVT_C3S)
  ENDIF
  !
  IF (NVT_PARK/=0) THEN
    ZSUM = ZSUM + KK%XVEGTYPE(JJ,NVT_PARK)
  ELSEIF (NVT_FLTR/=0 .AND. NVT_FLGR/=0) THEN
    ZSUM = ZSUM + KK%XVEGTYPE(JJ,NVT_FLTR) + KK%XVEGTYPE(JJ,NVT_FLGR)
  ENDIF
  !
  KK%XALBVIS_DRY(JJ) = 0.05 +  (   0.05 + MAX(0.30 * KK%XSAND(JJ,1), 0.10) )  &
                         * ( 1. - 0.9 * ZSUM**2 )
  !
ENDDO
!
KK%XALBNIR_DRY(:) = KK%XALBVIS_DRY(:) + 0.10
!
KK%XALBUV_DRY (:) = 0.06 + 0.14 * KK%XSAND(:,1)
!
KK%XALBVIS_WET(:) = KK%XALBVIS_DRY(:) / 2.
KK%XALBNIR_WET(:) = KK%XALBNIR_DRY(:) / 2.
KK%XALBUV_WET (:) = KK%XALBUV_DRY (:) / 2.
!
IF (LHOOK) CALL DR_HOOK('DRY_WET_SOIL_ALBEDOS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DRY_WET_SOIL_ALBEDOS
