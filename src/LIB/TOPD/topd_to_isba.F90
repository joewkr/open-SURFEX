!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ####################
      SUBROUTINE TOPD_TO_ISBA (K, UG, U, KI,KSTEP,GTOPD)
!     ####################
!
!!****  *TOPD_TO_ISBA*  
!!
!!    PURPOSE
!!    -------
!    
!     
!         
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
!!    
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
!!      Original   09/10/2003
!!                 03/2014 (B. Vincendon) correction for meshes covered by several watersheds
!!                 03/2015 (E. Artinyan) YSTEP jusqu'a 99999 steps
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_UNPACK_SAME_RANK
!
USE MODI_WRITE_FILE_ISBAMAP
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN,         ONLY : NNCAT, NNMC, NNB_TOPD_STEP
USE MODD_COUPLING_TOPD,   ONLY : XWG_FULL, XDTOPT, XWTOPT, XWSUPSAT,&
                                 NMASKT, XTOTBV_IN_MESH, NNPIX,&
                                 NFREQ_MAPS_WG, XBV_IN_MESH,NNBV_IN_MESH
!
USE MODD_SURF_PAR,        ONLY : XUNDEF,NUNDEF
USE MODD_ISBA_PAR,        ONLY : XWGMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER, INTENT(IN)                 :: KI      ! Grid dimensions
INTEGER, INTENT(IN)                 :: KSTEP   ! Topodyn current time step
LOGICAL, DIMENSION(:), INTENT(INOUT)   :: GTOPD     ! 
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ, JI , JMESH, JCAT         ! loop control 
REAL, DIMENSION(KI)       :: ZW              ! TOPODYN water content on ISBA grid (mm)
REAL, DIMENSION(KI)       :: ZWSAT_FULL      ! Water content at saturation on the layer 2 
                                          ! on the full grid
REAL, DIMENSION(KI)       :: ZWG_OLD
REAL, DIMENSION(KI)       :: ZDG_FULL
!
REAL, DIMENSION(KI,NNCAT) :: ZCOUNT, ZW_CAT
!
CHARACTER(LEN=5)          :: YSTEP
INTEGER                   :: JCAT_IN

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ZW(:)= 0.0
ZW_CAT(:,:)= 0.0
ZCOUNT(:,:)=0.0
!
ZWG_OLD(:) = XWG_FULL(:)
!
!
!*       1.     TOPODYN-LAT => ISBA
!               -------------------
!*       1.1    mobilizable water
!               -----------------
!
DO JJ=1,NNCAT
 IF (GTOPD(JJ)) THEN
  DO JI=1,NNMC(JJ)
   IF ( (XDTOPT(JJ,JI) /= XUNDEF).AND. (NMASKT(JJ,JI) /= NUNDEF) )THEN
    ZW_CAT(NMASKT(JJ,JI),JJ) = ZW_CAT(NMASKT(JJ,JI),JJ) + XWTOPT(JJ,JI)
    ZCOUNT(NMASKT(JJ,JI),JJ) = ZCOUNT(NMASKT(JJ,JI),JJ) + 1.0
   ENDIF
  ENDDO
 ENDIF
ENDDO
!
!
JCAT_IN=1
DO JMESH=1,KI
 IF (XTOTBV_IN_MESH(JMESH)/=0.0 .AND. XTOTBV_IN_MESH(JMESH)/=XUNDEF ) THEN
  ! at least 1 catchment over mesh
  DO JCAT=1,NNCAT
   IF (XTOTBV_IN_MESH(JMESH)==XBV_IN_MESH(JMESH,JCAT)) THEN ! only 1 catchment on mesh
    JCAT_IN=JCAT
    IF (GTOPD(JCAT).AND. NNBV_IN_MESH(JMESH,JCAT) /=0.) THEN
     IF (XBV_IN_MESH(JMESH,JCAT)>=UG%G%XMESH_SIZE(JMESH)*0.75.AND. ZCOUNT(JMESH,JCAT)/=0.) THEN ! catchment covers totaly mesh
     ZW(JMESH) = ZW_CAT(JMESH,JCAT) / ZCOUNT(JMESH,JCAT) 
     ELSEIF(ZCOUNT(JMESH,JCAT)/=0.)THEN
      ZW(JMESH) = ZW_CAT(JMESH,JCAT) /  ZCOUNT(JMESH,JCAT) 
     ENDIF

    ELSE
     ZW(JMESH)=ZWG_OLD(JMESH)
    ENDIF
   ENDIF
  ENDDO
  !
  IF(ZW(JMESH)==0.0) JCAT_IN=0 ! several catchments on the same mesh
  !
  IF (JCAT_IN==0) THEN  ! several catchments on the same mesh
   IF (XTOTBV_IN_MESH(JMESH)>=UG%G%XMESH_SIZE(JMESH)*0.75) THEN ! catchmentS cover totaly mesh
    DO JCAT=1,NNCAT
     IF (GTOPD(JCAT).AND. ZCOUNT(JMESH,JCAT)/=0.) THEN
      ZW(JMESH) = ZW(JMESH) + ZW_CAT(JMESH,JCAT) / ZCOUNT(JMESH,JCAT) *&
                 MIN(1.0,(XBV_IN_MESH(JMESH,JCAT)/UG%G%XMESH_SIZE(JMESH)))
     ELSE
      ZW(JMESH)=0.
     ENDIF
    ENDDO
    IF (ZW(JMESH)==0.) ZW(JMESH)=ZWG_OLD(JMESH)
   ELSE
    DO JCAT=1,NNCAT
     IF (GTOPD(JCAT).AND. ZCOUNT(JMESH,JCAT)/=0.) THEN
      ZW(JMESH) = ZW(JMESH) + ZW_CAT(JMESH,JCAT) / ZCOUNT(JMESH,JCAT)*&
                 MIN(1.0,(XBV_IN_MESH(JMESH,JCAT)/UG%G%XMESH_SIZE(JMESH)))
     ELSE
      ZW(JMESH)=0.
     ENDIF
    ENDDO
    IF (ZW(JMESH)==0.) ZW(JMESH)=ZWG_OLD(JMESH)
    !
   ENDIF
  ENDIF

 ELSE
  ZW(JMESH)=ZWG_OLD(JMESH)
 ENDIF
ENDDO
!
XWG_FULL(:) = MAX(ZW(:),XWGMIN)
!
!
 CALL UNPACK_SAME_RANK(U%NR_NATURE,K%XWSAT(:,2),ZWSAT_FULL)
!
XWSUPSAT=0.
!ludo glace Wsat varie
WHERE ( XWG_FULL(:) > ZWSAT_FULL(:) .AND. XWG_FULL(:)/=XUNDEF )
  !ludo calcul sat avant wg
  XWSUPSAT(:) = XWG_FULL(:) - ZWSAT_FULL(:)
  XWG_FULL(:) = ZWSAT_FULL(:)
ENDWHERE
!
IF ( (NFREQ_MAPS_WG/=0 .AND. MOD(KSTEP,NFREQ_MAPS_WG)==0) .OR.&
     ( KSTEP==NNB_TOPD_STEP)  ) THEN
  ! writing of YSTEP to be able to write maps
  IF (KSTEP<10) THEN
    WRITE(YSTEP,'(I1)') KSTEP
  ELSEIF (KSTEP < 100) THEN
    WRITE(YSTEP,'(I2)') KSTEP
  ELSEIF (KSTEP < 1000) THEN
    WRITE(YSTEP,'(I3)') KSTEP
  ELSEIF (KSTEP < 10000) THEN
    WRITE(YSTEP,'(I4)') KSTEP
  ELSE
    WRITE(YSTEP,'(I5)') KSTEP
  ENDIF
  !
  CALL OPEN_FILE('ASCII ',NUNIT,HFILE='carte_w'//YSTEP,HFORM='FORMATTED',HACTION='WRITE')
  CALL WRITE_FILE_ISBAMAP(UG, NUNIT,XWG_FULL,KI)
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
ENDIF
!

IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOPD_TO_ISBA
