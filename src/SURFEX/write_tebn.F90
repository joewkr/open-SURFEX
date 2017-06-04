!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_TEB_n (DTCO, HSELECT, OSNOWDIMNC, U, TM, GDM, GRM, HPROGRAM,HWRITE)
!     ####################################
!
!!****  *WRITE_TEB_n* - routine to write surface variables in their respective files
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
!!      B. Decharme 07/2011 : Suppress pgd output
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY
!
USE MODI_END_IO_SURF_n
USE MODI_INIT_IO_SURF_n
USE MODI_WRITESURF_TEB_n
USE MODI_WRITESURF_TEB_CONF_n
USE MODI_WRITESURF_SBL_n
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
LOGICAL, INTENT(IN) :: OSNOWDIMNC
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_TEB_N',0,ZHOOK_HANDLE)
!
 CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','WRITE','TEB_PROGNOSTIC.OUT.nc')
!
!*       1.     Selection of surface scheme
!               ---------------------------
!
 CALL WRITESURF_TEB_CONF_n(TM%CHT, TM%TD%MTO, TM%TD%O, TM%TD%DUT, TM%NT%AL(1), TM%TOP,HPROGRAM)
!
DO JP=1,TM%TOP%NTEB_PATCH
  CALL WRITESURF_TEB_n(HSELECT, OSNOWDIMNC, DTCO, U, TM%TOP, TM%BOP, TM%NT%AL(JP), TM%NB%AL(JP), &
                       TM%DTT%LDATA_ROAD_DIR, TM%TPN, GDM%O, GDM%S, GDM%NPE%AL(JP), GRM%O, GRM%S, &
                       GRM%NPE%AL(JP), HPROGRAM, JP, HWRITE)
END DO
!     
IF ((.NOT.LNOWRITE_CANOPY).OR.SIZE(HSELECT)>0) THEN
   CALL END_IO_SURF_n(HPROGRAM)      
   CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','WRITE','TEB_CANOPY.OUT.nc')
  CALL WRITESURF_SBL_n(HSELECT, TM%TOP%LCANOPY, TM%SB, HPROGRAM, HWRITE, "TOWN  ")
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_TEB_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TEB_n
