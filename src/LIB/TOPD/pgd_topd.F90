!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     #############################################################
      SUBROUTINE PGD_TOPD (HISBA, HGRID, PGRID_PAR, KDIM_FULL, PSSO_SLOPE, HPROGRAM)
!     #############################################################
!
!!****  *PGD_TOPD* - routine to determine the masks that permit to couple ISBA grid with Topmodel one
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!   (from initial version of init_coupl_topo.f90 by K. Chancibault, modified by
!!     M. Lelay and B. Vincendon)
!!
!!    AUTHOR
!!    ------
!!      B. Vincendon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2011
!!                 03/2014 (E. Artinian) manages the option CGRID='IGN'
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN,         ONLY : CCAT, NNCAT, XRTOP_D2, NMESHT, XDXT
USE MODD_COUPLING_TOPD,   ONLY : LCOUPL_TOPD, NIMAX, NJMAX, &
                                 XXI, XYI, NMASKI, NMASKT, NNPIX,&
                                 NNBV_IN_MESH, XBV_IN_MESH, XTOTBV_IN_MESH
USE MODD_DUMMY_EXP_PROFILE, ONLY : XF_PARAM_BV, XC_DEPTH_RATIO_BV
!
USE MODD_SURF_PAR,          ONLY : NUNDEF
!
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODE_GRIDTYPE_LONLAT_REG
USE MODE_GRIDTYPE_IGN
!
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_TOPD
USE MODI_INIT_TOPD_PGD
USE MODI_ABOR1_SFX
USE MODI_MAKE_MASK_TOPD_TO_ISBA
USE MODI_MAKE_MASK_ISBA_TO_TOPD
USE MODI_WRITE_FILE_MASKTOPD
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_TOPD_TO_ISBA_SLOPE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HISBA
 CHARACTER(LEN=*), INTENT(IN) :: HGRID
REAL, DIMENSION(:), INTENT(IN) :: PGRID_PAR
 INTEGER, INTENT(IN) :: KDIM_FULL
 REAL, DIMENSION(:), INTENT(INOUT) :: PSSO_SLOPE
!
CHARACTER(LEN=*),  INTENT(IN)     :: HPROGRAM    !
!
CHARACTER(LEN=50),DIMENSION(NNCAT) :: CNAME
INTEGER                   :: IL                     ! number of points
INTEGER                   :: JJ,JI,JK,JWRK ! loop control 
INTEGER                   :: JCAT,JMESH,JPIX ! loop control 
INTEGER                           :: ILUOUT       ! Logical unit for output filr
INTEGER                           :: IMESHL       !  number of ISBA grid nodes
INTEGER                           :: ILAMBERT     ! Lambert projection type
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZXI, ZYI     ! natural coordinates of ISBA grid (conformal projection or latlon)
REAL, DIMENSION(:), ALLOCATABLE   :: ZDXI, ZDYI   ! Isba grid resolution in the conformal projection
REAL, DIMENSION(:), ALLOCATABLE   :: ZXN, ZYN     ! isba nodes coordinates in the Lambert II coordinates
REAL, DIMENSION(:), ALLOCATABLE   :: ZLAT,ZLON    ! Isba nodes geographical coordinates
REAL, DIMENSION(:), ALLOCATABLE   :: ZDTAV        ! Averaged depth soil on TOP-LAT grid
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL                              :: ZLONMIN,ZLONMAX  ! min and max longitude values (latlon coordinates)
REAL                              :: ZLATMIN,ZLATMAX  ! min and max latitude values (latlon coordinates)
REAL                              :: ZRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL                              :: ZBETA    ! angle between grid and reference longitude
REAL                              :: ZLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL                              :: ZLONOR   ! longitude of point of coordinates X=0, Y=0!
REAL, DIMENSION(:), ALLOCATABLE   :: ZF_PARAM,ZC_DEPTH_RATIO  !
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_TOPD',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!  
CALL READ_NAM_PGD_TOPD(HPROGRAM,LCOUPL_TOPD,CCAT,XF_PARAM_BV,XC_DEPTH_RATIO_BV)
!
IF (LCOUPL_TOPD .AND. (HISBA/='3-L'.AND. HISBA/='DIF')) &
  CALL ABOR1_SFX("PGD_TOPD: coupling with topmodel only runs with CISBA=3-L or CISBA=DIF  ")
!
!         1.   Reads the namelists
!              --------------------
IF (LCOUPL_TOPD) THEN
  !
  WRITE(ILUOUT,*) 'Debut pgd_topd'
  !
  !         3.   Initialises variables specific to Topmodel
  !              -------------------------------------------
  WRITE(ILUOUT,*) 'NNCAT',NNCAT
  !
  CALL INIT_TOPD_PGD(HPROGRAM)
  !
  !         4.   Compute masks to couple ISBA and TOPODYN grids
  !              -------------------------------------------------------
  !
  !*      1.      Calcul of the plane coordinates 
  !               -------------------------------
  !
  !*      1.1 Gestion des projections conformes
  !           ---------------------------------
  !
  ALLOCATE(NMASKT(NNCAT,NMESHT))
  NMASKT(:,:)=NUNDEF
  !
  IF(HGRID.EQ.'CONF PROJ') THEN
    !
    WRITE(ILUOUT,*) 'GRILLE PROJ CONF (application Cevennes)'
    !
    !*      1.1.1   lecture des coordonnees X et Y conformes 
    !               -----------------------------------------
    !
    ALLOCATE(ZXI(KDIM_FULL))
    ALLOCATE(ZYI(KDIM_FULL))
    ZXI(:)=0.0
    ZYI(:)=0.0
    !
    ALLOCATE(ZDXI(KDIM_FULL))
    ALLOCATE(ZDYI(KDIM_FULL))
    !
    CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,PLAT0=ZLAT0,PLON0=ZLON0,PRPK=ZRPK, &
                                PBETA=ZBETA,PLATOR=ZLATOR,PLONOR=ZLONOR,     &
                                KIMAX=NIMAX,KJMAX=NJMAX,PX=ZXI,PY=ZYI,       &
                                PDX=ZDXI,PDY=ZDYI)
    !
    IMESHL = (NIMAX+1)*(NJMAX+1)
    !
    !*      1.1.2   calcul des coordonnees X et Y conformes des noeuds de la grille ISBA
    !               --------------------------------------------------------------------
    ALLOCATE(ZXN(IMESHL))
    ALLOCATE(ZYN(IMESHL))
    !
    DO JJ=1,NIMAX
      ZXN(JJ) = ZXI(JJ) - ZDXI(JJ)/2.
    ENDDO
    ZXN(NIMAX+1) = ZXI(NIMAX) + ZDXI(NIMAX)/2.
    !
    DO JJ=1,NJMAX
      JWRK = (JJ-1)*(NIMAX+1)+1   ! indice sur la grille des noeuds
      JI = (JJ-1)*NIMAX+1         ! indice sur la grille des mailles
      ZYN(JWRK) = ZYI(JI) - ZDYI(JI)/2.
    ENDDO
    !
    JJ = ((NJMAX+1)-1)*(NIMAX+1)+1  ! Indice sur la grille des noeuds
    JI = (NJMAX-1)*NIMAX+1
    ZYN(JJ) = ZYI(JI) + ZDYI(JI)/2.
    !
    DEALLOCATE(ZDXI)
    DEALLOCATE(ZDYI)
    DEALLOCATE(ZXI)
    DEALLOCATE(ZYI)
    !
    DO JJ=1,NIMAX+1
      DO JI=2,NJMAX+1
        JK = (JI-1)*(NIMAX+1)+JJ
        ZXN(JK) = ZXN(JJ)
      ENDDO
    ENDDO
    !
    DO JI=1,NJMAX+1
      DO JJ=2,NIMAX+1
        JK = (JI-1)*(NIMAX+1)+JJ
        JWRK = (JI-1)*(NIMAX+1)+1
        ZYN(JK) = ZYN(JWRK)
      ENDDO
    ENDDO
    !    
    !*      1.1.3   calcul des coordonnées géographiques des noeuds de la grille ISBA
    !               -----------------------------------------------------------------
    ALLOCATE(ZLAT(IMESHL))
    ALLOCATE(ZLON(IMESHL))
    CALL LATLON_CONF_PROJ(ZLAT0,ZLON0,ZRPK,ZBETA,ZLATOR,ZLONOR,ZXN,ZYN,ZLAT,ZLON)
    DEALLOCATE(ZXN)
    DEALLOCATE(ZYN)
    !
    !*      1.2 Gestion des coordonnees geographiques
    !           -------------------------------------
    !
  ELSE IF(HGRID.EQ.'LONLAT REG') THEN
    !
    WRITE(ILUOUT,*) 'GRILLE LONLAT REG (application AMMA)' 
    !
    ALLOCATE(ZXI(KDIM_FULL))
    ALLOCATE(ZYI(KDIM_FULL))
    ZXI(:)=0.0
    ZYI(:)=0.0
    CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,PLONMIN=ZLONMIN,PLONMAX=ZLONMAX,             &
                                 PLATMIN=ZLATMIN,PLATMAX=ZLATMAX,KLON=NIMAX,KLAT=NJMAX, &
                                 KL=IL,PLON=ZXI,PLAT=ZYI)
    !
    IMESHL=(NIMAX+1)*(NJMAX+1)
    !
    ALLOCATE(ZLON(IMESHL))
    ALLOCATE(ZLAT(IMESHL))
    ALLOCATE(ZDXI(KDIM_FULL))
    ALLOCATE(ZDYI(KDIM_FULL))
    !
    ZDXI(:)=(ZLONMAX-ZLONMIN)/(NIMAX-1)
    ZDYI(:)=(ZLATMAX-ZLATMIN)/(NJMAX-1)
    !
    DO JJ=1,NIMAX
      ZLON(JJ) = ZXI(JJ) - ZDXI(JJ)/2.
    ENDDO
    ZLON(NIMAX+1) = ZXI(NIMAX) + ZDXI(NIMAX)/2.
    !
    DO JJ=1,NJMAX
      JWRK=(JJ-1)*(NIMAX+1)+1   ! indice sur la grille des noeuds
      JI=(JJ-1)*NIMAX+1        ! indice sur la grille des mailles
      ZLAT(JWRK) = ZYI(JI) - ZDYI(JI)/2.
    ENDDO
    !
    JJ=((NJMAX+1)-1)*(NIMAX+1)+1  ! Indice sur la grille des noeuds
    JI=(NJMAX-1)*NIMAX+1
    ZLAT(JJ) = ZYI(JI) + ZDYI(JI)/2.
    !
    DEALLOCATE(ZDXI)
    DEALLOCATE(ZDYI)
    DEALLOCATE(ZXI)
    DEALLOCATE(ZYI)
    !
    DO JJ=1,NIMAX+1
      DO JI=2,NJMAX+1
        JK = (JI-1)*(NIMAX+1)+JJ
        ZLON(JK) = ZLON(JJ)
      ENDDO
    ENDDO
    !
    DO JI=1,NJMAX+1
      DO JJ=2,NIMAX+1
        JK=(JI-1)*(NIMAX+1)+JJ
        JWRK=(JI-1)*(NIMAX+1)+1
        ZLAT(JK)=ZLAT(JWRK)
      ENDDO
    ENDDO
    !
  ! Modification by Eram Artinian to take into account IGN grid 1
  ELSE IF (HGRID=='IGN') THEN 
    WRITE(ILUOUT,*) 'GRILLE IGN (application Bulgarie)' 
    ALLOCATE(ZXN(KDIM_FULL))
    ALLOCATE(ZYN(KDIM_FULL))
    CALL GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT=ILAMBERT,&
                          KL=IL,PX=ZXN,PY=ZYN,KDIMX=NIMAX)
    IMESHL=IL
    ALLOCATE(ZLAT(IMESHL))
    ALLOCATE(ZLON(IMESHL))
    CALL LATLON_IGN(ILAMBERT,ZXN,ZYN,ZLAT,ZLON)
  !End modification by Eram Artinian to take into account IGN grid
  ELSE 
    !       
    WRITE(ILUOUT,*) 'ERREUR: TYPE DE GRILLE NON GERE PAR LE CODE'
    CALL ABOR1_SFX("PGD_TOPD: TYPE DE GRILLE NON GERE PAR LE CODE")
    !
  ENDIF
  !
  !*      2.0  calcul des coordonnées lambert II étendu des noeuds de la grille ISBA
  !            ----------------------------------------------------------------------
  !
  ALLOCATE(XXI(IMESHL))
  ALLOCATE(XYI(IMESHL))
  ! Modification by Eram Artinian to take into account IGN grid 2
  IF (HGRID/='IGN') THEN
    CALL XY_IGN(5,XXI,XYI,ZLAT,ZLON)
  ELSE
    XXI=ZXN
    XYI=ZYN
    DEALLOCATE(ZXN)
    DEALLOCATE(ZYN)
  ENDIF
  !End modification by Eram Artinian to take into account IGN grid 2
  DEALLOCATE(ZLAT)
  DEALLOCATE(ZLON)
  !
  !*      2.0     mask
  !               ----
  !***
  CALL MAKE_MASK_TOPD_TO_ISBA(HGRID, PGRID_PAR, KDIM_FULL)
  !
  ALLOCATE(NNPIX(KDIM_FULL))
  NNPIX(:) = NUNDEF
  DO JJ=1,KDIM_FULL
    NNPIX(JJ) = COUNT(NMASKT(:,:)==JJ)
  ENDDO
  !
  CALL MAKE_MASK_ISBA_TO_TOPD(KDIM_FULL)
  !
  CALL WRITE_FILE_MASKTOPD(KDIM_FULL)
  !
  !*        3.0 Compute Mean slope over each ISBA_MESH
!            ----------------------------------------------------------------------
   CALL TOPD_TO_ISBA_SLOPE(PSSO_SLOPE, KDIM_FULL)
!
!*        4.0  Compute F and DC for each ISBA mesh
  !            ----------------------------------------------------------------------
  !
  ALLOCATE(NNBV_IN_MESH(KDIM_FULL,NNCAT))
  ALLOCATE(XBV_IN_MESH(KDIM_FULL,NNCAT))
  ALLOCATE(XTOTBV_IN_MESH(KDIM_FULL))
  !
  XTOTBV_IN_MESH(:) = 0.0
  !
  DO JMESH=1,KDIM_FULL
    XBV_IN_MESH(JMESH,:)=0.0
    DO JCAT=1,NNCAT
      NNBV_IN_MESH(JMESH,JCAT) = COUNT(NMASKI(JMESH,JCAT,:)/=NUNDEF)
      XBV_IN_MESH(JMESH,JCAT) = REAL(NNBV_IN_MESH(JMESH,JCAT))*XDXT(JCAT)**2
      XTOTBV_IN_MESH(JMESH) = XTOTBV_IN_MESH(JMESH) + XBV_IN_MESH(JMESH,JCAT)
    ENDDO
  ENDDO
  !
  ALLOCATE (ZF_PARAM(KDIM_FULL))
  ALLOCATE (ZC_DEPTH_RATIO(KDIM_FULL))
  !
  ZF_PARAM(:) = 0.
  ZC_DEPTH_RATIO(:) = 0.
  DO JCAT=1,NNCAT
    DO JMESH=1,KDIM_FULL
      IF ( XTOTBV_IN_MESH(JMESH)/=0. ) THEN
        ZF_PARAM(JMESH) = ZF_PARAM(JMESH) + XF_PARAM_BV(JCAT)*XBV_IN_MESH(JMESH,JCAT)/XTOTBV_IN_MESH(JMESH)
        ZC_DEPTH_RATIO(JMESH) = ZC_DEPTH_RATIO(JMESH) + XC_DEPTH_RATIO_BV(JCAT)*XBV_IN_MESH(JMESH,JCAT)/XTOTBV_IN_MESH(JMESH)
      ENDIF
    ENDDO
  ENDDO
  !
  WHERE (ZF_PARAM==0.)
    ZF_PARAM=2.5
    ZC_DEPTH_RATIO=1.
  ENDWHERE
  !
  !write(*,*) 'f min max isba',MINVAL(ZF_PARAM),MAXVAL(ZF_PARAM)
  !write(*,*) 'dc min max isba',MINVAL(ZC_DEPTH_RATIO),MAXVAL(ZC_DEPTH_RATIO)
  !
  CALL OPEN_FILE('ASCII ',NUNIT,'carte_f_dc.txt','FORMATTED',HACTION='WRITE')
  DO JMESH=1,KDIM_FULL
    WRITE(NUNIT,*) ZF_PARAM(JMESH),ZC_DEPTH_RATIO(JMESH)
  ENDDO
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
  DEALLOCATE(ZF_PARAM)
  DEALLOCATE(ZC_DEPTH_RATIO)
  !
  WRITE(ILUOUT,*) 'Couplage avec TOPMODEL active'
  !
ELSE
  !
  WRITE(ILUOUT,*) 'Pas de couplage avec TOPMODEL'
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_TOPD
