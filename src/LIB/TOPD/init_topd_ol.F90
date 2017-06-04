!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE INIT_TOPD_OL(HPROGRAM)
!     #######################
!
!!****  *INIT_TOPD_OL*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at initialising the variables 
!     needed of running Topmodel for OFFLINE step.
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
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   03/2014
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
                               XTOPD, XDRIV, XDHIL, XTIME_TOPD, &
                               XDGRD, XSPEEDG, XTIME_TOPD_DRAIN,&
                               XQTOT, XTANB, XSLOP, XDAREA,&
                               XLAMBDA, XCONN, XQB_DR, XQB_RUN
!
USE MODD_TOPD_PAR, ONLY : NDIM
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_INIT_TOPD
USE MODI_READ_TOPD_HEADER_DTM
USE MODI_READ_TOPD_FILE
USE MODI_READ_TOPD_HEADER_CONNEX
USE MODI_READ_CONNEX_FILE
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
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
INTEGER                   :: JI,JJ,JCAT,JO ! loop control 
INTEGER                   :: IOVER                  ! Unit of the files
INTEGER                   :: ILUOUT                 ! Unit of the files
INTEGER                   :: IOS
!
CHARACTER(LEN=28) :: YFILE
!
REAL   :: ZTMP !Temporary variable read
!
REAL :: ZDHIL  ! distance along slope
REAL :: ZDRIV  ! distance along rivers
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_TOPD_OL',0,ZHOOK_HANDLE)
!


!*       1    Initialization:
!               ---------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'INITIALISATION INIT_TOPD_OL'
!
CALL INIT_TOPD('ASCII ')
!
  !*      2       Calculations for routing by geomorpho
  !               -------------------------------------
  !
  ALLOCATE(NX_STEP_ROUT(NNCAT))
  ALLOCATE(XTIME_TOPD(NNCAT,NMESHT))
  ALLOCATE(XTIME_TOPD_DRAIN(NNCAT,NMESHT))
  !
  XTIME_TOPD(:,:) = 0.0
  XTIME_TOPD_DRAIN(:,:) = 0.0
  !
  !
  DO JCAT=1,NNCAT
    !
    IF ( XSPEEDR(JCAT)/=0. .AND. XSPEEDG(JCAT)/=0. ) THEN
      WHERE ( XDHIL(JCAT,1:NNMC(JCAT))/=XUNDEF .AND. XDRIV(JCAT,1:NNMC(JCAT))/=XUNDEF ) &
        XTIME_TOPD(JCAT,1:NNMC(JCAT)) = XDHIL(JCAT,1:NNMC(JCAT)) / XSPEEDH(JCAT) + &
                                        XDRIV(JCAT,1:NNMC(JCAT)) / XSPEEDR(JCAT)
      WHERE ( XDGRD(JCAT,1:NNMC(JCAT))/=XUNDEF .AND. XDRIV(JCAT,1:NNMC(JCAT))/=XUNDEF ) &
        XTIME_TOPD_DRAIN(JCAT,1:NNMC(JCAT)) = XDGRD(JCAT,1:NNMC(JCAT)) / XSPEEDG(JCAT) + &
                                              XDRIV(JCAT,1:NNMC(JCAT)) / XSPEEDR(JCAT)
    ELSE 
      WRITE(ILUOUT,*) 'You have to choose some values for routing velocities'
    ENDIF
    !
    IF (XTOPD_STEP/=0.) &
      NX_STEP_ROUT(JCAT) = INT(MAXVAL(XTIME_TOPD(JCAT,1:NNMC(JCAT))) / XTOPD_STEP) + 1
    !
  ENDDO
  !
  IF ( NNB_STP_RESTART==0 ) NNB_STP_RESTART = MAX(NNB_TOPD_STEP,MAXVAL(NX_STEP_ROUT(:)))
  !
  !
  ALLOCATE(XQB_DR(NNCAT,NNB_TOPD_STEP))
  XQB_DR(:,:)=0.0
  ALLOCATE(XQB_RUN(NNCAT,NNB_TOPD_STEP))
  XQB_RUN(:,:)=0.0
  !

!
IF (LHOOK) CALL DR_HOOK('INIT_TOPD_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TOPD_OL
