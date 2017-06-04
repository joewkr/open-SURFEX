!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ########
SUBROUTINE DIAG_SURF_ATM_n (YSC, HPROGRAM)
!     #################################################################################
!
!!****  *DIAG_SURF_ATM_n * - Chooses the surface schemes for diagnostics
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
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2008 : cumulated fluxes
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!!------------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
!
USE MODI_DIAG_NATURE_n 
USE MODI_DIAG_SEA_n 
USE MODI_DIAG_INLAND_WATER_n 
USE MODI_DIAG_TOWN_n 
USE MODI_AVERAGE_DIAG
!
USE MODI_MINZS_VERT_SHIFT
!
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
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
INTEGER :: JSW                          ! number of spectral whort wave bands
!
REAL, DIMENSION(SIZE(YSC%U%XSEA),NTILESFC) :: ZFRAC_TILE! fraction of each tile
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
! FLAGS for the various surfaces:
!
GSEA      = YSC%U%NDIM_SEA    >0
GWATER    = YSC%U%NDIM_WATER  >0
GTOWN     = YSC%U%NDIM_TOWN   >0
GNATURE   = YSC%U%NDIM_NATURE >0
!
! Tile counter:
!
JTILE     = 0 
!
! Fractions for each tile:
!
ZFRAC_TILE(:,:)    = 0.0
!
! Number of spectral short wave bands for detailed radiation budget
JSW = SIZE(YSC%DUP%AL(1)%XSWBD,2)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
! first, pack vector...then call ALMA routine
!
JTILE               = JTILE + 1
!
IF(GSEA)THEN
! 
  ZFRAC_TILE(:,JTILE) = YSC%U%XSEA(:)
!
  CALL DIAG_SEA_n(YSC%DLO, YSC%DL, YSC%DLC, YSC%SM%SD, &
                  YSC%U%CSEA, HPROGRAM, YSC%DUP%AL(1), YSC%DUPC%AL(1), YSC%U%NR_SEA)
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GWATER)THEN
!
  ZFRAC_TILE(:,JTILE) = YSC%U%XWATER(:)
!
  CALL DIAG_INLAND_WATER_n(YSC%DLO, YSC%DL, YSC%DLC, YSC%FM, YSC%WM, &
                           YSC%U%CWATER, HPROGRAM, YSC%DUP%AL(2), YSC%DUPC%AL(2), YSC%U%NR_WATER)
!
ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GNATURE)THEN
!
    ZFRAC_TILE(:,JTILE) = YSC%U%XNATURE(:)
!
  CALL DIAG_NATURE_n(YSC%DLO, YSC%DL, YSC%DLC, YSC%IM%ID, &
                     YSC%U%CNATURE, HPROGRAM, YSC%DUP%AL(3), YSC%DUPC%AL(3), YSC%U%NR_NATURE)   
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE               = JTILE + 1
!
IF(GTOWN)THEN
!
    ZFRAC_TILE(:,JTILE) = YSC%U%XTOWN(:)
!
  CALL DIAG_TOWN_n(YSC%DLO, YSC%DL, YSC%DLC, YSC%TM%TD, &
                   YSC%U%CTOWN, HPROGRAM, YSC%DUP%AL(4), YSC%DUPC%AL(4), YSC%U%NR_TOWN)  
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average fluxes/properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
CALL AVERAGE_DIAG(ZFRAC_TILE, YSC%DUO, YSC%DU, YSC%DUP, YSC%DUC, YSC%DUPC)              
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Quantities at 2 meters above the minimum orography of the grid mesh
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (YSC%DUO%L2M_MIN_ZS) CALL GET_2M
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
CONTAINS
!=======================================================================================
SUBROUTINE GET_2M
!
REAL, DIMENSION(SIZE(YSC%U%XSEA)) :: ZPS         ! surface air pressure
REAL, DIMENSION(SIZE(YSC%U%XSEA)) :: ZRHOA       ! surface air density
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_2M',0,ZHOOK_HANDLE)
!
CALL MINZS_VERT_SHIFT(YSC%DU, YSC%U%XZS, YSC%USS%XMIN_ZS, ZPS, ZRHOA)  
YSC%DU%XHU2M_MIN_ZS = YSC%DU%XHU2M
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_ATM_n:GET_2M',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_2M
!
!=======================================================================================
END SUBROUTINE DIAG_SURF_ATM_n
