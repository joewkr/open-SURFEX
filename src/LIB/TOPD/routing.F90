!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     ####################
      SUBROUTINE ROUTING(PRO,PDR,KSTEP)
!     ####################
!
!!****  *ROUTING*  
!!
!!    PURPOSE
!!    -------
!     To route the runoff and the exfiltration discharge to the catchment outlet
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
!!    REFERENCE
!!    ---------
!!
!!    
!!     
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * Meteo-France *
!!      G-M. Saulnier  * LTHE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   23/11/2005
!!      M. Le Lay     02/2008 Compatibility with the RESTART option (to update the
!!                            discharge between two runs)
!!      Modif B Vincendon 11/2011 : stock managed in three distinct variables
!!      Modif B Vincendon 03/2014 : correction of a bug on drainage still in the
!                                   river
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,        ONLY:XUNDEF
!
USE MODD_TOPODYN, ONLY : XTOPD_STEP, NNCAT, XQTOT, NNMC, &
                         XTIME_TOPD, XQB_RUN, XQB_DR, XTIME_TOPD_DRAIN, NNB_TOPD_STEP
USE MODD_COUPLING_TOPD, ONLY: XRUN_TOROUT, XDR_TOROUT, NNB_STP_RESTART
!
USE MODI_GET_LUOUT
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
REAL, DIMENSION(:,:), INTENT(IN) :: PRO     ! Total water for runoff for each pixel (m3/s)
!ludo
REAL, DIMENSION(:,:), INTENT(IN) :: PDR     ! Total water for drainage for each pixel
INTEGER, INTENT(IN)              :: KSTEP   ! current integration step
!
!
!*      0.2    declarations of local variables
!
!
INTEGER                            :: JCAT, JJ, JI ! Loop variables
INTEGER                            :: JSTEP        ! current or future integration steps
REAL, DIMENSION(NNCAT,NNB_TOPD_STEP+NNB_STP_RESTART) :: ZRUN_TOROUT,ZDR_TOROUT ! Kg/m2
                                                   ! water of runoff and drainage resp. still in the river 
                                                   ! and added to the discharge for the current simulation
 CHARACTER(LEN=3)                  :: YSTEP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ROUTING',0,ZHOOK_HANDLE)
!
!*       1.0.     Initialization :
!               --------------
!
ZRUN_TOROUT(:,:) = 0.
ZDR_TOROUT (:,:) = 0.
!
DO JCAT=1,NNCAT
  !
  !*       2.0    Runoff by geomorpho transfer function
  !               -------------------------------------
  DO JJ=1,NNMC(JCAT)
  !
    IF ( PRO(JCAT,JJ) > 0.0 .AND. PRO(JCAT,JJ) < XUNDEF ) THEN
      !
      JSTEP = INT(XTIME_TOPD(JCAT,JJ) / XTOPD_STEP) + KSTEP 
      !
      IF ( JSTEP.LE.NNB_TOPD_STEP ) THEN
        !
        XQB_RUN(JCAT,JSTEP) = XQB_RUN(JCAT,JSTEP) + PRO(JCAT,JJ)
        XQTOT(JCAT,JSTEP) = XQTOT(JCAT,JSTEP) + PRO(JCAT,JJ)
        !
      ELSEIF (JSTEP.LE.NNB_TOPD_STEP+NNB_STP_RESTART) THEN
        !
        ZRUN_TOROUT(JCAT,JSTEP) = ZRUN_TOROUT(JCAT,JSTEP) + PRO(JCAT,JJ)  !m3
        !
      ENDIF
      !
    ENDIF
    !
  !
  !*       3.0    Drainage by geomorpho transfer function
  !               -------------------------------------
    !
    IF ((PDR(JCAT,JJ) > 0.0).AND.(PDR(JCAT,JJ)<XUNDEF)) THEN
      !
      JSTEP = INT(XTIME_TOPD_DRAIN(JCAT,JJ) / XTOPD_STEP) + KSTEP
      !
      IF (JSTEP.LE.NNB_TOPD_STEP) THEN
        !
        XQB_DR(JCAT,JSTEP) = XQB_DR(JCAT,JSTEP) + PDR(JCAT,JJ)
        XQTOT(JCAT,JSTEP) = XQTOT(JCAT,JSTEP) + PDR(JCAT,JJ) 
        !
      ELSEIF (JSTEP.LE.NNB_TOPD_STEP+NNB_STP_RESTART) THEN
        !
        ZDR_TOROUT(JCAT,JSTEP) = ZDR_TOROUT(JCAT,JSTEP) + PDR(JCAT,JJ) !m3
        !
      ENDIF
    ENDIF
    !
  ENDDO
  ! 
  XQB_RUN(JCAT,KSTEP) = XQB_RUN(JCAT,KSTEP) + XRUN_TOROUT(JCAT,KSTEP)
  XQB_DR(JCAT,KSTEP)  = XQB_DR(JCAT,KSTEP)  + XDR_TOROUT(JCAT,KSTEP)
  XQTOT(JCAT,KSTEP)   = XQTOT(JCAT,KSTEP) + XRUN_TOROUT(JCAT,KSTEP) + XDR_TOROUT(JCAT,KSTEP)
  !
  XRUN_TOROUT(JCAT,:) = XRUN_TOROUT(JCAT,:) + ZRUN_TOROUT(JCAT,:)
  XDR_TOROUT(JCAT,:)  = XDR_TOROUT(JCAT,:)  + ZDR_TOROUT(JCAT,:)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ROUTING',1,ZHOOK_HANDLE)
!
END SUBROUTINE ROUTING
