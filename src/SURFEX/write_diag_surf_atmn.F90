!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE WRITE_DIAG_SURF_ATM_n (YSC,HPROGRAM,HWRITE)
!     #################################################################################
!
!!****  *WRITE_DIAG_SURF_ATM_n * - Chooses the surface schemes for diagnostics writing
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
USE MODI_WRITE_DIAG_NATURE_n 
USE MODI_WRITE_DIAG_SEA_n 
USE MODI_WRITE_DIAG_INLAND_WATER_n 
USE MODI_WRITE_DIAG_TOWN_n 
!
USE MODI_WRITE_DIAG_SEB_SURF_ATM_n
!
USE MODI_WRITE_DIAG_CH_AGGR_n
USE MODI_WRITE_DIAG_CH_SNAP_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE    ! 'PGD' : only physiographic fields are written
!                                            ! 'ALL' : all fields are written
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SURF_ATM_N',0,ZHOOK_HANDLE)
!
CPROGNAME = HPROGRAM
!
IF (YSC%U%NDIM_SEA    >0) CALL WRITE_DIAG_SEA_n(YSC%DTCO, YSC%DUO, YSC%U, YSC%SM, & 
                                                HPROGRAM,HWRITE)
IF (YSC%U%NDIM_WATER  >0) CALL WRITE_DIAG_INLAND_WATER_n(YSC%DTCO, YSC%DUO, YSC%U, &
                                                         YSC%WM, YSC%FM, HPROGRAM,HWRITE)
IF (YSC%U%NDIM_NATURE >0) CALL WRITE_DIAG_NATURE_n(YSC%DTCO, YSC%DUO, YSC%U, YSC%IM, &
                                                   YSC%NDST, HPROGRAM,HWRITE)
IF (YSC%U%NDIM_TOWN   >0) CALL WRITE_DIAG_TOWN_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%U, YSC%TM, &
                                                 YSC%GDM, YSC%GRM, HPROGRAM,HWRITE)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Writing
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (YSC%DUO%XDIAG_TSTEP==XUNDEF .OR. &
        ABS(NINT(YSC%U%TTIME%TIME/YSC%DUO%XDIAG_TSTEP)*YSC%DUO%XDIAG_TSTEP-YSC%U%TTIME%TIME)<1.E-3 ) THEN
  !
  IF (YSC%DUO%LFRAC) THEN
    CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','WRITE','SURF_ATM.OUT.nc')          
    YCOMMENT = '(fraction)'
    CALL WRITE_SURF(YSC%DUO%CSELECT,HPROGRAM,'FRAC_SEA   ',YSC%U%XSEA, IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(YSC%DUO%CSELECT,HPROGRAM,'FRAC_NATURE',YSC%U%XNATURE,IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(YSC%DUO%CSELECT,HPROGRAM,'FRAC_WATER ',YSC%U%XWATER, IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(YSC%DUO%CSELECT,HPROGRAM,'FRAC_TOWN  ',YSC%U%XTOWN, IRESP,HCOMMENT=YCOMMENT)
    CALL END_IO_SURF_n(HPROGRAM)
  END IF
  !
  IF (HWRITE/='PGD'.AND.YSC%DUO%LDIAG_GRID) &
          CALL WRITE_DIAG_SEB_SURF_ATM_n(YSC%DTCO, YSC%DUO, YSC%DU, YSC%DUC, YSC%U, &
                                         YSC%UG%G%CGRID, HPROGRAM)
  !
  IF (YSC%CHU%LCH_EMIS .AND. YSC%SV%NBEQ>0 .AND. YSC%CHU%LCH_SURF_EMIS) THEN
    IF (YSC%CHU%CCH_EMIS=='AGGR') THEN 
      CALL WRITE_DIAG_CH_AGGR_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%U, YSC%CHE, HPROGRAM)
    ELSE IF (YSC%CHU%CCH_EMIS=='SNAP') THEN
      CALL WRITE_DIAG_CH_SNAP_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%U, YSC%CHN, HPROGRAM)
    END IF
  END IF
  !  
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_SURF_ATM_n
