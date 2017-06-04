!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE INIT_TOPD(HPROGRAM)
!     #######################
!
!!****  *INIT_TOPD*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at initialising the variables 
!     needed of running Topmodel.
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
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   11/2006
!!      Modification 04/2007 : Supression of 2 arguments KSTEP,PSTEP 
!!                             that are now module arguments from MODD_TOPDDYN_n
!!                             NNB_TOPD_STEP,XTOPD_STEP
!!      Modification 11/2011 : Exfiltration option removed (B. Vincendon)
!!      Modification 03/2014 : common init_topd routine  called in init_topd_pgd
!!                             and init_topd_ol (B. Vincendon)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_COUPLING_TOPD, ONLY : NNB_STP_RESTART
USE MODD_TOPODYN,       ONLY : CCAT, NNCAT, NNB_TOPD_STEP, XTOPD_STEP,&
                               XDXT, NNXC, NNYC,&
                               XNUL, XX0, XY0, NNPT,&
                               NX_STEP_ROUT, XSPEEDR,&
                               XSPEEDH, NNMC, NMESHT, NPMAX,&
                               NLINE,  XDMAXT,&
                               XTOPD, XDRIV, XDHIL, XTIME_TOPD,&
                               XDGRD, XSPEEDG, XTIME_TOPD_DRAIN,&
                               XQTOT, XTANB, XSLOP, XDAREA,&
                               XLAMBDA, XCONN, XQB_DR, XQB_RUN
!
USE MODD_TOPD_PAR, ONLY : NDIM
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_READ_TOPD_HEADER_DTM
USE MODI_READ_TOPD_FILE
USE MODI_READ_TOPD_HEADER_CONNEX
USE MODI_READ_CONNEX_FILE
USE MODI_READ_SLOPE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HPROGRAM    !
!
!*      0.2    declarations of local variables
!
!
 CHARACTER(LEN=50), DIMENSION(NNCAT) :: YFILETOP ! topographic file names
 CHARACTER(LEN=50), DIMENSION(NNCAT) :: YFILECON ! topographic file names
 CHARACTER(LEN=50), DIMENSION(NNCAT) :: YFILESLO
 CHARACTER(LEN=50), DIMENSION(NNCAT) :: YFILEDH
 CHARACTER(LEN=50), DIMENSION(NNCAT) :: YFILEDR
INTEGER                   :: JJ,JCAT ! loop control 
INTEGER                   :: ILUOUT                 ! Unit of the files
!
REAL, DIMENSION(:),ALLOCATABLE    :: ZTOPD_READ !Topgraphic variable read
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_TOPD',0,ZHOOK_HANDLE)
!
!*       1    Initialization:
!               ---------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'INITIALISATION INIT_TOPD'
!
!
DO JCAT=1,NNCAT
  YFILETOP(JCAT)=TRIM(CCAT(JCAT))//'_FilledDTM.map'
  YFILECON(JCAT)=TRIM(CCAT(JCAT))//'_connections.vec'
  YFILESLO(JCAT)=TRIM(CCAT(JCAT))//'_slope.vec'
  YFILEDR(JCAT)=TRIM(CCAT(JCAT))//'_RiverDist.map'
  YFILEDH(JCAT)=TRIM(CCAT(JCAT))//'_HillDist.map'
ENDDO

ALLOCATE(NNMC(NNCAT))
NNMC(:)=0
ALLOCATE(NNXC(NNCAT))
NNXC(:)=0
ALLOCATE(NNYC(NNCAT))
NNYC(:)=0
ALLOCATE(XX0(NNCAT))
XX0(:)=0.0
ALLOCATE(XY0(NNCAT))
XY0(:)=0.0
ALLOCATE(NNPT(NNCAT))
NNPT(:)=0
ALLOCATE(XNUL(NNCAT))
ALLOCATE(XDXT(NNCAT))
!
!
!*       2      Topographic files
!               -----------------------------
DO JCAT=1,NNCAT
  CALL READ_TOPD_HEADER_DTM(HPROGRAM,YFILETOP(JCAT),'FORMATTED',&
                            XX0(JCAT),XY0(JCAT),NNXC(JCAT),NNYC(JCAT),&
                            XNUL(JCAT),XDXT(JCAT))
ENDDO
!
NNPT(:) = NNXC(:) * NNYC(:)
!
NPMAX = MAXVAL(NNPT(:))
!
!
ALLOCATE(NLINE(NNCAT,NPMAX))
NLINE(:,:)=0
ALLOCATE(XTOPD(NNCAT,NPMAX))
XTOPD(:,:)=0.0
ALLOCATE(ZTOPD_READ(NPMAX))
DO JCAT = 1,NNCAT
  !
  CALL READ_TOPD_FILE(HPROGRAM,YFILETOP(JCAT),'FORMATTED',NNPT(JCAT),ZTOPD_READ)
  DO JJ = 1,NNPT(JCAT)
    XTOPD(JCAT,JJ) = ZTOPD_READ(JJ) ! XTOPD can only be >=0
  ENDDO
  !
ENDDO
! for XTOPD, we do not have to use NLINE, all the pixels of the rectancle around
! the catchment are read.
!

!*      3       Connection files
!               ----------------
!
DO JCAT=1,NNCAT
  CALL READ_TOPD_HEADER_CONNEX(HPROGRAM,YFILECON(JCAT),'FORMATTED',NNMC(JCAT))
ENDDO
!
NMESHT=MAXVAL(NNMC(:))
!
ALLOCATE(XCONN(NNCAT,NMESHT,NDIM))
XCONN(:,:,:)=0.0
!
DO JCAT=1,NNCAT
  CALL READ_CONNEX_FILE(HPROGRAM,YFILECON(JCAT),'FORMATTED',NNMC(JCAT),XCONN(JCAT,:,:),NLINE(JCAT,:))
ENDDO

!
!*       4      Slope files
!               -----------
  ALLOCATE(XTANB(NNCAT,NMESHT))
  ALLOCATE(XSLOP(NNCAT,NMESHT))
  ALLOCATE(XDAREA(NNCAT,NMESHT))
  ALLOCATE(XLAMBDA(NNCAT,NMESHT))
  !
  DO JCAT=1,NNCAT
    CALL READ_SLOPE_FILE(HPROGRAM,YFILESLO(JCAT),'FORMATTED',NNMC(JCAT),&
                       XTANB(JCAT,:),XSLOP(JCAT,:),XDAREA(JCAT,:),XLAMBDA(JCAT,:))
  ENDDO
  !
ALLOCATE(XDRIV(NNCAT,NPMAX))
XDRIV(:,:)=0.0
ALLOCATE(XDHIL(NNCAT,NPMAX))
XDHIL(:,:)=0.0
ALLOCATE(XDGRD(NNCAT,NPMAX))
XDGRD(:,:)=0.0
ALLOCATE(XQTOT(NNCAT,NNB_TOPD_STEP))
XQTOT(:,:)=0.0
!
!*     5          River Distance file
  !               -------------------
  !
  DO JCAT=1,NNCAT
    CALL  READ_TOPD_FILE(HPROGRAM,YFILEDR(JCAT),'FORMATTED',NNPT(JCAT),ZTOPD_READ)
    DO JJ=1,NPMAX
      IF ( NLINE(JCAT,JJ)/=0. .AND. NLINE(JCAT,JJ)/=XUNDEF ) &
       XDRIV(JCAT,NLINE(JCAT,JJ)) = ZTOPD_READ(JJ)
    ENDDO
  ENDDO
  ! for XDRIV, we must use NLINE, online pixels inside de the catchment are read.
  !
  !*      6       Hillslope Distance file
  !  -----------------------
  !
  DO JCAT=1,NNCAT
    CALL  READ_TOPD_FILE(HPROGRAM,YFILEDH(JCAT),'FORMATTED',NNPT(JCAT),ZTOPD_READ)
    DO JJ=1,NPMAX
      IF ( NLINE(JCAT,JJ)/=0. .AND. NLINE(JCAT,JJ)/=XUNDEF ) &
        XDHIL(JCAT,NLINE(JCAT,JJ)) = ZTOPD_READ(JJ)
    ENDDO
  ENDDO
  !
  ! for XDHIL, we must use NLINE, online pixels inside de the catchment are read.
  XDGRD(:,:) = XDHIL(:,:)

IF (LHOOK) CALL DR_HOOK('INIT_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TOPD
