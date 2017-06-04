!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE INIT_COUPL_TOPD (DEC, IO, S, K, NP, NPE, UG, U, HPROGRAM )
!     #######################
!
!!****  *INIT_COUPL_TOPD*  
!!
!!    PURPOSE
!!    -------
!!     This routine aims at initialising the variables 
!     needed for coupling with Topmodel.
! 
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * LTHE / Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   16/10/2003
!!      Modif BV : supression of variables specific to Topmodel
!!      20/12/2007 - mll : Adaptation between a lonlat grid system for ISBA
!!                         and lambert II projection for topmodel
!!      11/2011: Modif BV : Creation of masks between ISBA and TOPODYN
!                transfered in PGD step (routine init_pgd_topd)
!!      03/2014: Modif BV : New organisation for first time step (displacement
!!                          in coupl_topd)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
! Modules
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t

!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_COUPLING_TOPD, ONLY : XWSTOPI, XWFCTOPI, XDTOPI, XAS_NATURE, XATOP,&
                                 XCSTOPI, XWTOPT, XAVG_RUNOFFCM, XAVG_DRAINCM,&
                                 XDTOPT, XKA_PRE, XKAC_PRE, NMASKI, XDMAXFC, &
                                 XWG_FULL, XWSTOPT, XWFCTOPT, NMASKT, & 
                                 NNBV_IN_MESH, XBV_IN_MESH, XTOTBV_IN_MESH,&
                                 XRUNOFF_TOP, NNPIX,&
                                 XFRAC_D2, XFRAC_D3, XWGI_FULL,&
                                 XRUN_TOROUT, XDR_TOROUT,&
                                 LSTOCK_TOPD,NNB_STP_RESTART,NMASKT_PATCH 
USE MODD_DUMMY_EXP_PROFILE,ONLY :XF_PARAM, XC_DEPTH_RATIO
USE MODD_TOPODYN,       ONLY : NNCAT, XMPARA, XCSTOPT, NMESHT, XDXT,&
                                 NNMC, XRTOP_D2, NNB_TOPD_STEP,  XDMAXT
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF

!
! Interfaces
USE MODI_GET_LUOUT
USE MODI_READ_FILE_MASKTOPD
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ISBA_TO_TOPD
USE MODI_RESTART_COUPL_TOPD
USE MODI_AVG_PATCH_WG
USE MODI_DG_DFTO3L
!
USE MODE_SOIL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEC
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=*), INTENT(IN) :: HPROGRAM    ! 
!
!*      0.2    declarations of local variables
!
!REAL, DIMENSION(:), ALLOCATABLE   :: ZDTAV                            ! Averaged depth soil on TOP-LAT grid
REAL, DIMENSION(:), ALLOCATABLE   :: ZSAND_FULL, ZCLAY_FULL, ZDG_FULL ! Isba variables on the full domain
REAL, DIMENSION(:), ALLOCATABLE   :: ZFRAC    ! fraction of SurfEx mesh that covers one or several catchments
REAL, DIMENSION(:), ALLOCATABLE   :: ZDMAXAV  ! dificit maximal moyen par bassin
REAL, DIMENSION(:),ALLOCATABLE    :: ZSANDTOPI, ZCLAYTOPI!, ZWWILTTOPI !sand and clay fractions on TOPMODEL layers
!
!ludo
REAL, DIMENSION(:), ALLOCATABLE   :: ZKSAT, ZKSAT_NAT       !ksat surf 
REAL, DIMENSION(:), ALLOCATABLE   :: ZDG2_FULL, ZDG3_FULL, ZWG2_FULL, ZWG3_FULL, ZRTOP_D2
REAL,DIMENSION(:), ALLOCATABLE    :: ZWGI_FULL, Z_WFCTOPI, Z_WSTOPI
!
REAL                              :: ZCOEF_ANIS  !coefficient anisotropie Ksat:
                                                 ! Ksat horiz=ZCOEF*Ksat vert
INTEGER                   :: JJ,JI           ! loop control 
INTEGER                   :: JCAT,JMESH      ! loop control 
INTEGER                   :: ILUOUT          ! Logical unit for output filr
!
REAL, DIMENSION(U%NDIM_NATURE,3)  :: ZWG_3L, ZWGI_3L, ZDG_3L   
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_COUPL_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'INITIALISATION INIT_COUPL_TOPD'
!
ALLOCATE(NMASKT(NNCAT,NMESHT))
NMASKT(:,:) = NUNDEF
!
!*       1    Initialization:
!               ---------------
!
ZWG_3L (:,:) = XUNDEF 
ZWGI_3L(:,:) = XUNDEF
ZDG_3L (:,:) = XUNDEF  
!
ALLOCATE(NMASKT_PATCH(U%NDIM_NATURE))
!
IF (IO%CISBA=='DIF') THEN
 CALL DG_DFTO3L(IO, NP, ZDG_3L)
ELSEIF (IO%CISBA=='3-L') THEN
 CALL AVG_PATCH_WG(IO, NP, NPE, ZWG_3L, ZWGI_3L, ZDG_3L)
ENDIF
! la surface saturee, à l'initialisation est nulle, donc on initialise 
!les lambdas de telle sorte qu'aucun pixel ne soit sature
ALLOCATE(XKA_PRE (NNCAT,NMESHT))
ALLOCATE(XKAC_PRE(NNCAT))
XKA_PRE(:,:) = 0.0
XKAC_PRE(:)  = MAXVAL(XKA_PRE) + 1.
!
!Cumulated runoff initialisation
ALLOCATE(XRUNOFF_TOP(U%NDIM_NATURE))
XRUNOFF_TOP  (:) = DEC%XRUNOFF(:)
!
IF(.NOT.ALLOCATED(XAVG_RUNOFFCM)) ALLOCATE(XAVG_RUNOFFCM(U%NDIM_NATURE))
XAVG_RUNOFFCM(:) = DEC%XRUNOFF(:)
!
IF(.NOT.ALLOCATED(XAVG_DRAINCM )) ALLOCATE(XAVG_DRAINCM (U%NDIM_NATURE))
XAVG_DRAINCM (:) = DEC%XDRAIN(:)
!
!
! Reading masks
 CALL READ_FILE_MASKTOPD(U%NDIM_FULL)
!
!*      2.1     Fraction of SurfEx mesh with TOPMODEL
!               -------------------------------------
!
ALLOCATE(NNBV_IN_MESH  (U%NDIM_FULL,NNCAT))
ALLOCATE(XBV_IN_MESH   (U%NDIM_FULL,NNCAT))
ALLOCATE(XTOTBV_IN_MESH(U%NDIM_FULL))
!
XTOTBV_IN_MESH(:) = 0.0
!
DO JJ=1,U%NDIM_FULL
  !
  XBV_IN_MESH(JJ,:) = 0.0
  !
  DO JI=1,NNCAT
    NNBV_IN_MESH(JJ,JI) = COUNT( NMASKI(JJ,JI,:)/=NUNDEF )
    XBV_IN_MESH (JJ,JI) = REAL(NNBV_IN_MESH(JJ,JI)) * XDXT(JI)**2
    XTOTBV_IN_MESH (JJ) = XTOTBV_IN_MESH(JJ) + XBV_IN_MESH(JJ,JI)
  ENDDO
  !
  IF (XTOTBV_IN_MESH(JJ)> UG%G%XMESH_SIZE(JJ)) THEN
    XBV_IN_MESH(JJ,:) = XBV_IN_MESH(JJ,:) * UG%G%XMESH_SIZE(JJ)/XTOTBV_IN_MESH(JJ)
    XTOTBV_IN_MESH (JJ) = UG%G%XMESH_SIZE(JJ)
  ENDIF
ENDDO
!
!*      2.2     Fraction of SurfEx mesh with each catchment
!               -------------------------------------------
!
ALLOCATE(ZFRAC(U%NDIM_FULL))  ! fraction not covered by catchments
ZFRAC(:) = ( UG%G%XMESH_SIZE(:)-XTOTBV_IN_MESH(:) ) / UG%G%XMESH_SIZE(:)
ZFRAC(:) = MIN(MAX(ZFRAC(:),0.),1.)
!
ALLOCATE(XATOP(U%NDIM_NATURE)) ! fraction covered by catchments part nature
 CALL PACK_SAME_RANK(U%NR_NATURE,(1.-ZFRAC),XATOP)
!
!
IF (HPROGRAM=='POST  ') GOTO 10
!
!*      3.0     Wsat, Wfc and depth for TOPODYN on ISBA grid
!               --------------------------------------------
!*      3.1     clay, sand fraction, depth hydraulic conductivity at saturation of the layer for TOPODYN
!               ---------------------------------------------------------
!
ALLOCATE(ZSAND_FULL(U%NDIM_FULL))
ALLOCATE(ZCLAY_FULL(U%NDIM_FULL))
 CALL UNPACK_SAME_RANK(U%NR_NATURE,K%XSAND(:,2),ZSAND_FULL)
 CALL UNPACK_SAME_RANK(U%NR_NATURE,K%XCLAY(:,2),ZCLAY_FULL)
!
!ludo prof variable pour tr lat (OK car sol homogene verticalement, faux sinon)
ALLOCATE(ZDG2_FULL(U%NDIM_FULL))
ALLOCATE(ZDG3_FULL(U%NDIM_FULL))
CALL UNPACK_SAME_RANK(U%NR_NATURE,ZDG_3L(:,2),ZDG2_FULL)
CALL UNPACK_SAME_RANK(U%NR_NATURE,ZDG_3L(:,3),ZDG3_FULL)
!
!
ALLOCATE(ZRTOP_D2(U%NDIM_FULL))
ZRTOP_D2(:) = 0.
!
DO JMESH=1,U%NDIM_FULL
  IF ( ZDG2_FULL(JMESH)/=XUNDEF .AND. ZFRAC(JMESH)<1. ) THEN
!    ZRTOP_D2(JMESH) = 0.
    DO JCAT=1,NNCAT
     !moyenne ponderee pour cas ou plusieurs BV sur maille
       ZRTOP_D2(JMESH) = ZRTOP_D2(JMESH) + &
               XRTOP_D2(JCAT)*MIN(XBV_IN_MESH(JMESH,JCAT)/XTOTBV_IN_MESH(JMESH),1.)    
    END DO
  ENDIF   
ENDDO
!ZTOP_D2 * D2 < D3 : the depth concerned by lateral transfers is lower than D2
WHERE( ZDG2_FULL/=XUNDEF .AND. ZRTOP_D2*ZDG2_FULL>ZDG3_FULL ) &
                ZRTOP_D2(:) = ZDG3_FULL(:)/ZDG2_FULL(:)
!
DEALLOCATE(ZFRAC)
!
ALLOCATE(XFRAC_D2 (U%NDIM_FULL))
ALLOCATE(XFRAC_D3 (U%NDIM_FULL))
XFRAC_D2(:)=1.
XFRAC_D3(:)=0.
!
IF (IO%CISBA=='3-L') THEN
 WHERE( ZDG2_FULL/=XUNDEF  ) ! if the depth is < D2
  XFRAC_D2(:) = MIN(1.,ZRTOP_D2(:))
 END WHERE
 WHERE( ZDG2_FULL/=XUNDEF .AND. ZRTOP_D2*ZDG2_FULL>ZDG2_FULL  ) ! if the depth is > D2
  XFRAC_D3(:) = (ZRTOP_D2(:)*ZDG2_FULL(:)-ZDG2_FULL(:)) / (ZDG3_FULL(:)-ZDG2_FULL(:))
  XFRAC_D3(:) = MAX(0.,XFRAC_D3(:))
 END WHERE
ENDIF
 !
ALLOCATE(ZDG_FULL(U%NDIM_FULL))
WHERE (ZDG2_FULL/=XUNDEF)
  ZDG_FULL = XFRAC_D2*ZDG2_FULL + XFRAC_D3*(ZDG3_FULL-ZDG2_FULL)
ELSEWHERE
  ZDG_FULL = XUNDEF
END WHERE
!
ALLOCATE(ZSANDTOPI(U%NDIM_FULL))
ALLOCATE(ZCLAYTOPI(U%NDIM_FULL))
ZSANDTOPI(:)=0.0
ZCLAYTOPI(:)=0.0
ALLOCATE(XDTOPI(U%NDIM_FULL))
XDTOPI(:)=0.0
WHERE ( ZDG_FULL/=XUNDEF .AND. ZDG_FULL/=0. )
  XDTOPI = ZDG_FULL
  ZSANDTOPI = ZSANDTOPI + ZSAND_FULL * ZDG_FULL
  ZCLAYTOPI = ZCLAYTOPI + ZCLAY_FULL * ZDG_FULL
  ZSANDTOPI = ZSANDTOPI / XDTOPI
  ZCLAYTOPI = ZCLAYTOPI / XDTOPI
ELSEWHERE
  ZSANDTOPI = XUNDEF
  ZCLAYTOPI = XUNDEF
  XDTOPI = XUNDEF
END WHERE
DEALLOCATE(ZSAND_FULL)
DEALLOCATE(ZCLAY_FULL)
!
!*      4.1     depth of the Isba layer on TOP-LAT grid
!               ---------------------------------------
!
ALLOCATE(XDTOPT(NNCAT,NMESHT))
XDTOPT(:,:) = 0.0
 CALL ISBA_TO_TOPD(XDTOPI,XDTOPT)
!
!*      3.2     Wsat and Wfc on TOPODYN layer
!               -----------------------------
!
ALLOCATE(XWSTOPI   (U%NDIM_FULL))
ALLOCATE(XWFCTOPI  (U%NDIM_FULL))
XWSTOPI (:) = 0.0
XWFCTOPI(:) = 0.0
XWSTOPI    = WSAT_FUNC_1D (ZCLAYTOPI,ZSANDTOPI,IO%CPEDOTF)
IF (IO%CISBA=='2-L' .OR. IO%CISBA=='3-L') THEN
  !  field capacity at hydraulic conductivity = 0.1mm/day
 XWFCTOPI   = WFC_FUNC_1D  (ZCLAYTOPI,ZSANDTOPI,IO%CPEDOTF)
ELSE IF (IO%CISBA=='DIF') THEN
  !  field capacity at water potential = 0.33bar        
 XWFCTOPI   = W33_FUNC_1D  (ZCLAYTOPI,ZSANDTOPI,IO%CPEDOTF)
END IF
!
!modif ludo test ksat exp
WRITE(ILUOUT,*) 'CKSAT==',IO%CKSAT

ALLOCATE(XCSTOPI(U%NDIM_FULL))
XCSTOPI(:) = 0.0
IF( IO%CKSAT=='SGH' .OR. IO%CKSAT=='EXP' ) THEN
  !
  ALLOCATE(ZKSAT_NAT(U%NDIM_NATURE))
  ZKSAT_NAT(:) = 0.
  ALLOCATE(ZKSAT(U%NDIM_FULL))
  ZKSAT   (:) = 0.0
  !
  !valeur patch 1 (idem wsat wfc) a voir cas ou il existe plusieurs patchs 
  CALL UNPACK_SAME_RANK(NP%AL(1)%NR_P,NP%AL(1)%XCONDSAT(:,1),ZKSAT_NAT)
  CALL UNPACK_SAME_RANK(U%NR_NATURE,ZKSAT_NAT,ZKSAT)
  !
  !passage de definition Ksat(profondeur) en Ksat(deficit)
  WHERE ( ZDG_FULL/=XUNDEF .AND. (XWSTOPI-XWFCTOPI/=0.) )
    XCSTOPI(:) = ZKSAT(:) / (XWSTOPI(:)-XWFCTOPI(:))
  END WHERE
  !
  DEALLOCATE(ZKSAT, ZKSAT_NAT)
  !
ELSE
  !
  XCSTOPI(:) = HYDCONDSAT_FUNC(ZCLAYTOPI,ZSANDTOPI,IO%CPEDOTF)
  !passage de definition Ksat(profondeur) en Ksat(deficit)
  WHERE ( ZDG_FULL/=XUNDEF .AND. (XWSTOPI-XWFCTOPI/=0.) )
    XCSTOPI(:) = XCSTOPI(:) / (XWSTOPI(:)-XWFCTOPI(:))
  END WHERE
  !
ENDIF
!
DEALLOCATE(ZSANDTOPI)
DEALLOCATE(ZCLAYTOPI)
DEALLOCATE(ZRTOP_D2)
!
!*      4.3     Ko on TOP-LAT grid
!               ------------------
!
ALLOCATE(XCSTOPT(NNCAT,NMESHT))
 CALL ISBA_TO_TOPD(XCSTOPI,XCSTOPT)
WHERE (XCSTOPT == XUNDEF) XCSTOPT = 0.0
  ZCOEF_ANIS = 1.
  XCSTOPT = XCSTOPT*ZCOEF_ANIS
!
!
ALLOCATE(XWG_FULL(U%NDIM_FULL))
ALLOCATE(XWGI_FULL(U%NDIM_FULL))
ALLOCATE(XWTOPT(NNCAT,NMESHT))
XWTOPT(:,:) = 0.0
ALLOCATE(XWSTOPT (NNCAT,NMESHT))
ALLOCATE(XWFCTOPT(NNCAT,NMESHT))
ALLOCATE(XDMAXFC(NNCAT,NMESHT))
XDMAXFC(:,:) = XUNDEF
ALLOCATE(XDMAXT(NNCAT,NMESHT))
XDMAXT(:,:)=XUNDEF
!
ALLOCATE(ZWG2_FULL(U%NDIM_FULL))
ALLOCATE(ZWG3_FULL(U%NDIM_FULL))
!
CALL UNPACK_SAME_RANK(U%NR_NATURE,ZWG_3L(:,2),ZWG2_FULL)
CALL UNPACK_SAME_RANK(U%NR_NATURE,ZWG_3L(:,3),ZWG3_FULL)
!
WHERE ( ZDG_FULL/=XUNDEF .AND. ZDG_FULL/=0. )
  XWG_FULL = XFRAC_D2*(ZDG2_FULL/ZDG_FULL)*ZWG2_FULL + &
                XFRAC_D3*((ZDG3_FULL-ZDG2_FULL)/ZDG_FULL)*ZWG3_FULL
ELSEWHERE
  XWG_FULL = XUNDEF
END WHERE

XWGI_FULL = 0.
!
!
!*      4.4     Initialisation of the previous time step water storage on topodyn-lat grid
!               --------------------------------------------------------------------------
!*      4.5     M parameter on TOPODYN grid
!               ------------------------
!*      4.5.1   Mean depth soil on catchment
!
ALLOCATE(XMPARA (NNCAT))
XMPARA  (:) = 0.0
IF (.NOT.ALLOCATED(XF_PARAM)) ALLOCATE(XF_PARAM(SIZE(S%XF_PARAM)))
!
! 
!*      5.0      Initial saturated area computation
!               -----------------------------------------------------------
!
ALLOCATE(XAS_NATURE(U%NDIM_NATURE))
XAS_NATURE(:) = 0.0
!
!*      6.0     Stock management in case of restart
!               -----------------------------------------------------------
!
10 CONTINUE
!
!stock
ALLOCATE(XRUN_TOROUT(NNCAT,NNB_TOPD_STEP+NNB_STP_RESTART))
ALLOCATE(XDR_TOROUT (NNCAT,NNB_TOPD_STEP+NNB_STP_RESTART))
XRUN_TOROUT(:,:) = 0.
XDR_TOROUT (:,:) = 0.
!
IF (HPROGRAM=='POST  ') GOTO 20
!
IF (LSTOCK_TOPD) CALL RESTART_COUPL_TOPD(UG, U%NR_NATURE, HPROGRAM,U%NDIM_FULL)
!
!*      7.0     deallocate
!               ----------
!
DEALLOCATE(ZDG2_FULL)
DEALLOCATE(ZDG3_FULL)
DEALLOCATE(ZWG2_FULL)
DEALLOCATE(ZWG3_FULL)
!
20 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('INIT_COUPL_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_COUPL_TOPD







