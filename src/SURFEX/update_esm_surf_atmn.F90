!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE UPDATE_ESM_SURF_ATM_n (F, IM, S, U, W, HPROGRAM, KI, KSW, PZENITH, PSW_BANDS,     &
                                   PTRAD, PDIR_ALB, PSCA_ALB, PEMIS, PTSURF )  
!     #################################################################################
!
!!****  *UPDATE_ESM_SURF_ATM_n * - Routine to update radiative properties in Earth
!!                                 System Model (SEA, WATER, NATURE, TOWN) after
!!                                 the call to OASIS coupler in order to close the
!!                                 energy budget between radiative scheme and surfex
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      B. Decharme 06/2013 new coupling variables
!!-------------------------------------------------------------
!
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
!
USE MODI_AVERAGE_RAD
!
USE MODI_AVERAGE_TSURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_UPDATE_ESM_ISBA_n
USE MODI_UPDATE_ESM_SEAFLUX_n
USE MODI_UPDATE_ESM_WATFLUX_n
USE MODI_UPDATE_ESM_FLAKE_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
 CHARACTER(LEN=6),       INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                INTENT(IN)  :: KI        ! number of points
INTEGER,                INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI),     INTENT(IN) :: PZENITH   ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(KSW),    INTENT(IN) :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
!*      0.2    declarations of local variables
!
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
!
! Tile outputs:
!
REAL, DIMENSION(KI,NTILESFC) :: ZTRAD_TILE     ! radiative surface temperature
REAL, DIMENSION(KI,NTILESFC) :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC) :: ZFRAC_TILE     ! fraction of each surface type
REAL, DIMENSION(KI,NTILESFC) :: ZTSURF_TILE    ! surface effective temperature
!
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZDIR_ALB_TILE ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZSCA_ALB_TILE ! diffuse albedo
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------
! FLAGS for the various surfaces:
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N',0,ZHOOK_HANDLE)
GSEA      = (U%NSIZE_SEA    >0 .AND. U%CSEA/='NONE')
GWATER    = (U%NSIZE_WATER  >0 .AND. U%CWATER/='NONE')
GNATURE   = (U%NSIZE_NATURE >0 .AND. U%CNATURE/='NONE')
!
GTOWN     = U%NSIZE_TOWN   >0
IF(GTOWN)THEN
  CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: TOWN SCHEME NOT YET AVAILABLE FOR EARTH SYSTEM MODEL')
ENDIF
!
! Tile counter:
!
JTILE     = 0 
!
! Initialization: Outputs to atmosphere over each tile:
!
ZTRAD_TILE(:,:)       = XUNDEF
ZDIR_ALB_TILE(:,:,:)  = XUNDEF
ZSCA_ALB_TILE(:,:,:)  = XUNDEF
ZEMIS_TILE(:,:)       = XUNDEF
ZTSURF_TILE(:,:)      = XUNDEF
!
! Fractions for each tile:
!
ZFRAC_TILE(:,:)    = 0.0
!
!--------------------------------------------------------------------------------------
! Call arrange interfaces for sea, water, nature and town here...
!--------------------------------------------------------------------------------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GSEA)THEN
!
   ZFRAC_TILE(:,JTILE) = U%XSEA(:)
!
   CALL TREAT_SURF(U%NSIZE_SEA,U%NR_SEA,JTILE)   ! pack variables which are arguments to this routine
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GWATER)THEN
!
   ZFRAC_TILE(:,JTILE) = U%XWATER(:)
!
   CALL TREAT_SURF(U%NSIZE_WATER,U%NR_WATER,JTILE)  
!
ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GNATURE)THEN
!
   ZFRAC_TILE(:,JTILE) = U%XNATURE(:)
!
   CALL TREAT_SURF(U%NSIZE_NATURE,U%NR_NATURE,JTILE)
!   
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Not yet implemented
!
!JTILE = JTILE + 1
!
!IF(GTOWN)THEN
!
!   ZFRAC_TILE(:,JTILE) = XTOWN(:)
!
!   CALL TREAT_SURF(NSIZE_TOWN,NR_TOWN,JTILE)  
!
!ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average radiative properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL AVERAGE_RAD(ZFRAC_TILE,                                           &
                   ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTRAD_TILE, &
                   PDIR_ALB,      PSCA_ALB,      PEMIS,      PTRAD       )  
!
 CALL AVERAGE_TSURF(ZFRAC_TILE, ZTSURF_TILE, PTSURF)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N',1,ZHOOK_HANDLE)
CONTAINS
!=======================================================================================
SUBROUTINE TREAT_SURF(KSIZE,KMASK,KTILE)
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
INTEGER, INTENT(IN)               :: KTILE
!
REAL, DIMENSION(KSIZE) :: ZP_ZENITH   ! zenithal angle       (radian from the vertical)
!
REAL, DIMENSION(KSIZE)     :: ZP_TRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KSIZE,KSW) :: ZP_DIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KSIZE,KSW) :: ZP_SCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KSIZE)     :: ZP_EMIS     ! emissivity
REAL, DIMENSION(KSIZE)     :: ZP_TSURF    ! effective temperature                 (K)
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! input arguments:
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N:TREAT_SURF',0,ZHOOK_HANDLE)
!
ZP_TRAD    = XUNDEF
ZP_DIR_ALB = XUNDEF
ZP_SCA_ALB = XUNDEF
ZP_EMIS    = XUNDEF
ZP_TSURF   = XUNDEF
!
DO JJ=1,KSIZE
  ZP_ZENITH(JJ)     = PZENITH     (KMASK(JJ))
ENDDO
!
!
IF (KTILE==1) THEN
  !
  IF (U%CSEA=='SEAFLX') THEN
    CALL UPDATE_ESM_SEAFLUX_n(S, U%NSIZE_SEA,KSW,ZP_ZENITH,ZP_DIR_ALB, &
                              ZP_SCA_ALB,ZP_EMIS,ZP_TRAD,ZP_TSURF )
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: SEA SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
ELSEIF (KTILE==2) THEN
  !
  IF (U%CWATER=='WATFLX') THEN   
    CALL UPDATE_ESM_WATFLUX_n(W, U%NSIZE_WATER,KSW,ZP_ZENITH,ZP_DIR_ALB, &
                              ZP_SCA_ALB,ZP_EMIS,ZP_TRAD,ZP_TSURF   )
  ELSEIF (U%CWATER=='FLAKE ') THEN
    CALL UPDATE_ESM_FLAKE_n(F, U%NSIZE_WATER,KSW,ZP_ZENITH,ZP_DIR_ALB, &
                            ZP_SCA_ALB,ZP_EMIS,ZP_TRAD,ZP_TSURF   )
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: INLAND WATER SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
ELSEIF (KTILE==3) THEN
  !          
  IF (U%CNATURE=='ISBA') THEN   
    CALL UPDATE_ESM_ISBA_n(IM%O, IM%S, IM%K, IM%NK, IM%NP, IM%NPE, U%NSIZE_NATURE,&
                           KSW,ZP_ZENITH,PSW_BANDS,ZP_DIR_ALB, &
                           ZP_SCA_ALB,ZP_EMIS,ZP_TRAD,ZP_TSURF              )
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: NATURE SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
!ELSEIF (KTILE==4) THEN
!  !
!  IF (CTOWN=='TEB   ') THEN   
!    CALL UPDATE_ESM_TEB_n(NSIZE_SEA,KSW,ZP_ZENITH,ZP_TRAD,ZP_DIR_ALB,ZP_SCA_ALB,ZP_EMIS,ZP_TSURF)
!  ELSE
!    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: TEB SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
!  ENDIF
!  !        
ENDIF
!
DO JJ=1,KSIZE
   ZTRAD_TILE      (KMASK(JJ),KTILE)  = ZP_TRAD      (JJ)
   ZDIR_ALB_TILE   (KMASK(JJ),:,KTILE)= ZP_DIR_ALB   (JJ,:)
   ZSCA_ALB_TILE   (KMASK(JJ),:,KTILE)= ZP_SCA_ALB   (JJ,:)
   ZEMIS_TILE      (KMASK(JJ),KTILE)  = ZP_EMIS      (JJ)
   ZTSURF_TILE     (KMASK(JJ),KTILE)  = ZP_TSURF     (JJ)
ENDDO
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!=======================================================================================
!
END SUBROUTINE UPDATE_ESM_SURF_ATM_n


