!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################################
      SUBROUTINE WRITE_SURF_ATM_n (YSC, HPROGRAM,HWRITE,OLAND_USE)
!     ####################################
!
!!****  *WRITE_SURF_ATM_n* - routine to write surface variables 
!!                           in their respective files or in file
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
!!      Modified    06/2007, P.LeMoigne: do not write pgd fields in
!!                                       historical files
!!      Modified    03/2009, B.Decharme: keys for arrange cover
!!      Modified    04/2009, B.Decharme: write precipitation forcing into the restart file for ARPEGE/ALADIN run
!       Modified    06/2009, B.Decharme: flag to desactivate writing of horizontal grid 
!       Modified    08/2009, B.Decharme: BUDGETC for all tiles
!       Modified    07/2011, B.Decharme: delete write pgd fields
!       Modified    07/2011, B.Decharme: land_use key for writing semi-prognostic variables
!       Modified    05/2012, B.Decharme: supress LPROVAR_TO_DIAG to write prognostic fields if user want
!       Modified    05/2013, B.Decharme: WRITESURF_PRECIP becomes WRITESURF_CPL_GCM
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODN_PREP_SURF_ATM,   ONLY : LWRITE_EXTERN
!
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_SURF_PAR,        ONLY : NVERSION, NBUGFIX
USE MODD_WRITE_SURF_ATM,  ONLY : LNOWRITE_CANOPY, LSPLIT_PATCH
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_SEA_n
USE MODI_WRITE_INLAND_WATER_n
USE MODI_WRITE_NATURE_n
USE MODI_WRITE_TOWN_n
USE MODI_END_IO_SURF_n
USE MODI_WRITE_GRID
!
USE MODI_WRITESURF_ATM_CONF_n
USE MODI_WRITESURF_SSO_CANOPY_n
USE MODI_WRITESURF_CPL_GCM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
LOGICAL,             INTENT(IN)  :: OLAND_USE !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IRESP
LOGICAL            :: LSAVE_SELECT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
!*       1.     Configuration and cover fields:
!               ------------------------------
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, HPROGRAM,'FULL  ','SURF  ','WRITE')
!
LSAVE_SELECT=YSC%DUO%LSELECT
YSC%DUO%LSELECT     =.FALSE.
!
YCOMMENT='(-)'
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'VERSION',NVERSION,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'BUG    ',NBUGFIX ,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'STORAGETYPE',HWRITE,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'WRITE_EXT ',LWRITE_EXTERN,IRESP,HCOMMENT=YCOMMENT)   
!
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'SPLIT_PATCH',LSPLIT_PATCH,IRESP,HCOMMENT=YCOMMENT)  
!
YCOMMENT='s'
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'DTCUR',YSC%U%TTIME,IRESP,YCOMMENT)
!
YSC%DUO%LSELECT=LSAVE_SELECT
!
 CALL WRITE_GRID(YSC%DUO%CSELECT, HPROGRAM,YSC%UG%G%CGRID,YSC%UG%G%XGRID_PAR,&
                 YSC%UG%G%XLAT,YSC%UG%G%XLON,YSC%UG%G%XMESH_SIZE,IRESP)
!
 CALL WRITESURF_ATM_CONF_n(YSC%CHU, YSC%DUO, YSC%USS, HPROGRAM)
!
IF (HWRITE/='PRE') CALL WRITESURF_SSO_CANOPY_n(YSC%DUO%CSELECT, YSC%SB, HPROGRAM,&
                                (YSC%USS%CROUGH=='BE04' .AND. .NOT. LNOWRITE_CANOPY))
!
 CALL WRITESURF_CPL_GCM_n(YSC%DUO%CSELECT, YSC%U, HPROGRAM)
!
YCOMMENT='flag for accumulated variables'
 CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'BUDC',YSC%DUO%LSURF_BUDGETC,IRESP,HCOMMENT=YCOMMENT)
!
IF (YSC%DUO%LSURF_BUDGETC) THEN
   YCOMMENT='time of beginning of accumulation'
   CALL WRITE_SURF(YSC%DUO%CSELECT, HPROGRAM,'TBUDC',YSC%DUO%TIME_BUDGETC,IRESP,HCOMMENT=YCOMMENT)   
END IF
!  
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!*       3.     Sea
!               ---
!
IF (YSC%U%NDIM_SEA>0) CALL WRITE_SEA_n(YSC%DTCO,YSC%DUO%CSELECT,YSC%U,YSC%SM,HPROGRAM,HWRITE)
!
!
!*       4.     Inland water
!               ------------
!
IF (YSC%U%NDIM_WATER>0) CALL WRITE_INLAND_WATER_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%U, &
                                                  YSC%WM, YSC%FM, HPROGRAM,HWRITE)
!
!
!*       5.     Vegetation scheme
!               -----------------
!
IF (YSC%U%NDIM_NATURE>0) CALL WRITE_NATURE_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%DUO%LSNOWDIMNC, &
                                             YSC%U, YSC%IM, YSC%NDST, HPROGRAM,HWRITE,OLAND_USE)
!
!
!*       6.     Urban scheme
!               ------------
!
IF (YSC%U%NDIM_TOWN>0) CALL WRITE_TOWN_n(YSC%DTCO, YSC%DUO%CSELECT, YSC%DUO%LSNOWDIMNC, &
                                         YSC%U, YSC%TM, YSC%GDM, YSC%GRM, HPROGRAM,HWRITE)
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_SURF_ATM_n
