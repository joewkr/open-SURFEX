!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_n (DTCO, U, UG, GCP, TOP, TG, &
                                 BOP, BDD, DTB, DTT, HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_TEB_n* - reads TEB physiographic fields
!!                       
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_PACK_INIT
USE MODI_READ_PGD_TEB_PAR_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_READ_LECOCLIMAP
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(GRID_t), INTENT(INOUT) :: TG
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
INTEGER           :: IRESP          ! Error code after redding
!
LOGICAL :: GECOSG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',TG%NDIM)
!
!*       2.     Other dimension initializations:
!               --------------------------------
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!* number of TEB patches
!
IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<=2) THEN
  TOP%NTEB_PATCH=1
ELSE
  YRECFM='TEB_PATCH'
  CALL READ_SURF(HPROGRAM,YRECFM,TOP%NTEB_PATCH,IRESP)
END IF
!
!* number of road and roof layers
!
YRECFM='ROAD_LAYER'
 CALL READ_SURF( HPROGRAM,YRECFM,TOP%NROAD_LAYER,IRESP)

YRECFM='ROOF_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,TOP%NROOF_LAYER,IRESP)

YRECFM='WALL_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,TOP%NWALL_LAYER,IRESP)
!
!
!* type of averaging for Buildings (to allow ascendant compatibility)
!* type of Building Energy Model
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
  TOP%CBLD_ATYPE='ARI'
  TOP%CBEM = 'DEF'
ELSE
  YRECFM='BLD_ATYPE'
  CALL READ_SURF(HPROGRAM,YRECFM,TOP%CBLD_ATYPE,IRESP)
  YRECFM='BEM'
  CALL READ_SURF(HPROGRAM,YRECFM,TOP%CBEM,IRESP)
END IF
!
IF (TOP%CBEM=="BEM") THEN
  YRECFM='FLOOR_LAYER'
  CALL READ_SURF(HPROGRAM,YRECFM,BOP%NFLOOR_LAYER,IRESP)
  YRECFM='COOL_COIL'
  CALL READ_SURF(HPROGRAM,YRECFM,BOP%CCOOL_COIL,IRESP)
  YRECFM='HEAT_COIL'
  CALL READ_SURF(HPROGRAM,YRECFM,BOP%CHEAT_COIL,IRESP)
  YRECFM='AUTOSIZE'
  CALL READ_SURF(HPROGRAM,YRECFM,BOP%LAUTOSIZE,IRESP)
ENDIF
!
!* Case of urban green roofs
!
IF (TOP%LGARDEN) THEN
  IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
    TOP%LGREENROOF = .FALSE.
  ELSE
    YRECFM='LGREENROOF'
    CALL READ_SURF(HPROGRAM,YRECFM,TOP%LGREENROOF,IRESP)
  END IF
!
!* Case of urban hydrology
!
  IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=3)) THEN
    TOP%LHYDRO = .FALSE.
  ELSE
    YRECFM='LURBAN_HYDRO'
    CALL READ_SURF(HPROGRAM,YRECFM,TOP%LHYDRO,IRESP)
  END IF
ENDIF
!
!* Solar panels
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=3)) THEN
  TOP%LSOLAR_PANEL = .FALSE.
ELSE
  YRECFM='SOLAR_PANEL'
  CALL READ_SURF(HPROGRAM,YRECFM,TOP%LSOLAR_PANEL,IRESP)
END IF
!
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(TOP%LCOVER(JPCOVER))
ALLOCATE(TOP%XZS(TG%NDIM))
ALLOCATE(TG%XLAT       (TG%NDIM))
ALLOCATE(TG%XLON       (TG%NDIM))
ALLOCATE(TG%XMESH_SIZE (TG%NDIM))
CALL PACK_INIT(DTCO,U,UG,HPROGRAM,'TOWN  ',TG, TOP%LCOVER,TOP%XCOVER,TOP%XZS )
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
 CALL READ_LECOCLIMAP(HPROGRAM,TOP%LECOCLIMAP,GECOSG)
!
 CALL READ_PGD_TEB_PAR_n(DTCO, U, GCP, BDD, DTB, DTT, TG%NDIM, TOP, &
                         HPROGRAM,TG%NDIM,'-')
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_n
