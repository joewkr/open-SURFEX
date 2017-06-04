!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     #############################################################
      SUBROUTINE INIT_SURF_TOPD (DEC, IO, S, K, NP, NPE, UG, U, HPROGRAM,KI)
!     #############################################################
!
!!****  *INIT_SURF_TOPD* - routine to initialize variables needed for coupling with Topmodel
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    The routine open and read the namelists NAM_COUPL_TOPD and NAM_TOPD,
!! calculates the number of catchments concerned, the different time step 
!! variables and all the variables nedded for coupling with Topmodel. 
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Vincendon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2006
!!      Modif 04/2007: Arguments PTOPD_STEP,KNB_TOPD_STEP become module
!!                     variables from MODD_TOPDDYN
!!      Modif 03/2014: New organisation of routines with init_topd_ol and
!!                     displacement of init_budget
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NPROC
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODD_TOPODYN, ONLY :CCAT, XSPEEDR, XSPEEDH, NNCAT, &
                        XRTOP_D2, XSPEEDG
USE MODD_COUPLING_TOPD, ONLY :  LCOUPL_TOPD, NNB_TOPD, LBUDGET_TOPD
!
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
USE MODI_INIT_TOPD_OL
USE MODI_INIT_COUPL_TOPD
USE MODI_INIT_BUDGET_COUPL_ROUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
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
 CHARACTER(LEN=*),  INTENT(IN)     :: HPROGRAM      !
INTEGER,           INTENT(IN)     :: KI            ! grid dimension
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_SURF_TOPD',0,ZHOOK_HANDLE)
!
IF (LCOUPL_TOPD) THEN
  IF (NPROC>1) CALL ABOR1_SFX('INIT_SURF_TOPD: TOPD CANNOT RUN WITH MORE THAN 1 MPI TASK') 
  IF (NBLOCKTOT>1) CALL ABOR1_SFX("INIT_SURF_TOPD: TOPD CANNOT RUN WITH NUMEROUS OPENMP BLOCKS")
  IF (NPE%AL(1)%TSNOW%SCHEME/='3-L') &
        CALL ABOR1_SFX("INIT_SURF_TOPD: coupling with topmodel only runs with TSNOW%SCHEME=3-L")
ENDIF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!            
!         1.   Reads the namelists
!              --------------------
!
WRITE(ILUOUT,*) 'Debut init_surf_topo_n'
!
IF (LCOUPL_TOPD) THEN
  !
  !         3.   Initialises variables specific to Topmodel
  !              -------------------------------------------
  WRITE(ILUOUT,*) 'NNCAT',NNCAT
  !
  CALL INIT_TOPD_OL(HPROGRAM)
  !
  !         4.   Initialises variables nedded for coupling with Topmodel
  !              -------------------------------------------------------
  !
  CALL INIT_COUPL_TOPD(DEC, IO, S, K, NP, NPE, UG, U, HPROGRAM)
  !
  WRITE(ILUOUT,*) 'Couplage avec TOPMODEL active'
  !
  !IF (LBUDGET_TOPD) CALL INIT_BUDGET_COUPL_ROUT(KI)
  !
ELSE
  !
  WRITE(ILUOUT,*) 'Pas de couplage avec TOPMODEL'
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_SURF_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_SURF_TOPD
