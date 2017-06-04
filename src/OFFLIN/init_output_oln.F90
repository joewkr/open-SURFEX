!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_OUTPUT_OL_n (YSC)
!     ######################
!
!!****  *INIT_OUTPUT_OL* Keep in memory the netcdf ID of the output files
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      modified 05/04 by P. LeMoigne *Meteo France*
!!      modified 06/10 by S. Faroux *Meteo France*
!!=================================================================
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODI_INIT_OUTFN_FLAKE_n
USE MODI_INIT_OUTFN_ISBA_n
USE MODI_INIT_OUTFN_SEA_n
USE MODI_INIT_OUTFN_SURF_ATM_n
USE MODI_INIT_OUTFN_TEB_n
USE MODI_INIT_OUTFN_WATER_n
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
!
INTEGER           :: IRET
INTEGER           :: ILUOUT
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!------------------------------------------------------------------------------ 
IF (LHOOK) CALL DR_HOOK('INIT_OUTPUT_OL_N',0,ZHOOK_HANDLE)
!
ILUOUT = 0
!
IF (NRANK==NPIO) THEN
  !       
  CALL INIT_OUTFN_SURF_ATM_n(YSC%DUO, YSC%UG, YSC%U, YSC%CHE, YSC%CHU, YSC%SV, YSC%DUO%CSELECT, "NC    ",ILUOUT)
  !
  IF (YSC%U%NDIM_NATURE>0) THEN
    IF (YSC%U%CNATURE=='ISBA  '.OR.YSC%U%CNATURE=='TSZ0  ') THEN 
      CALL INIT_OUTFN_ISBA_n(YSC%IM, YSC%UG, YSC%U, YSC%DUO%CSELECT, YSC%DUO%LSNOWDIMNC, "NC    ",ILUOUT)
    ENDIF
  ENDIF
  !
  IF (YSC%U%NDIM_SEA>0) THEN
    IF (YSC%U%CSEA=='SEAFLX') THEN 
      CALL INIT_OUTFN_SEA_n(YSC%SM, YSC%UG, YSC%U, YSC%DUO%CSELECT, "NC    ",ILUOUT)
    ENDIF
  ENDIF
  !
  IF (YSC%U%NDIM_WATER>0) THEN
    IF (YSC%U%CWATER=='WATFLX') CALL INIT_OUTFN_WATER_n(YSC%WM, YSC%UG, YSC%U, YSC%DUO%CSELECT, "NC    ",ILUOUT)
    IF (YSC%U%CWATER=='FLAKE ') CALL INIT_OUTFN_FLAKE_n(YSC%FM, YSC%UG, YSC%U, YSC%DUO%CSELECT, "NC    ",ILUOUT)
  ENDIF
  !
  IF (YSC%U%NDIM_TOWN>0) THEN
    IF (YSC%U%CTOWN=='TEB   ') THEN 
      CALL INIT_OUTFN_TEB_n(YSC%TM, YSC%GDM, YSC%GRM, YSC%UG, YSC%U, YSC%DUO%CSELECT, "NC    ",ILUOUT)
    ENDIF
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_OUTPUT_OL_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_OUTPUT_OL_n
