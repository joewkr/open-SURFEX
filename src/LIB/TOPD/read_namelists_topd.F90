!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
SUBROUTINE READ_NAMELISTS_TOPD(HPROGRAM)
!     #######################################################
!
!!**** *READ_NAMELISTS_TOPD* reads useful namelists for coupling with TOPD
!!
!!    PURPOSE
!!    -------
!!
!!    This routine aims at reading and initialising variables.
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Vincendon        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/2006
!!    B. Vincendon 02/2014 : adding possibility to choose the speed of water on hillslopes
!!                           and ti write runoff map on watersheds
!----------------------------------------------------------------------------
!
USE MODI_READ_NAM_PGD_TOPD
USE MODI_READ_NAM_TOPD
!
USE MODD_TOPODYN, ONLY       : CCAT, XTOPD_STEP, NNB_TOPD_STEP,& 
                               XSPEEDR, XSPEEDG, XSPEEDH,&
                               XQINIT, XRTOP_D2
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, NNB_TOPD,&
                               LBUDGET_TOPD, LSTOCK_TOPD,&
                               NNB_STP_STOCK, NNB_STP_RESTART,&
                               NFREQ_MAPS_WG, NFREQ_MAPS_ASAT, NFREQ_MAPS_RUNOFF
!
USE MODD_DUMMY_EXP_PROFILE,ONLY:XF_PARAM_BV,XC_DEPTH_RATIO_BV
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TOPD',0,ZHOOK_HANDLE)
!
 CALL READ_NAM_PGD_TOPD(HPROGRAM,LCOUPL_TOPD,CCAT,&
                        XF_PARAM_BV,XC_DEPTH_RATIO_BV)
IF (LCOUPL_TOPD) &
  CALL READ_NAM_TOPD(HPROGRAM,LBUDGET_TOPD,NNB_TOPD,&
                     LSTOCK_TOPD,NNB_STP_STOCK,NNB_STP_RESTART,&
                     NFREQ_MAPS_WG, NFREQ_MAPS_ASAT,NFREQ_MAPS_RUNOFF,&
                     XSPEEDR,XSPEEDG,XSPEEDH,XQINIT,XRTOP_D2)
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NAMELISTS_TOPD
