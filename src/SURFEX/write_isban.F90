!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_ISBA_n (DTCO, HSELECT, OSNOWDIMNC, U, IM, NDST, HPROGRAM, HWRITE, OLAND_USE)
!     ####################################
!
!!****  *WRITE_ISBA_n* - routine to write surface variables in their respective files
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
!       B. Decharme 07/2011 : land_use key for writing semi-prognostic variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_DST_n, ONLY : DST_NP_t
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY
USE MODI_WRITESURF_ISBA_n
USE MODI_WRITESURF_ISBA_CONF_n
USE MODI_WRITESURF_SBL_n
!
USE MODI_END_IO_SURF_n
USE MODI_INIT_IO_SURF_n
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
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DST_NP_t), INTENT(INOUT) :: NDST
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
LOGICAL,             INTENT(IN)  :: OLAND_USE !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_ISBA_N',0,ZHOOK_HANDLE)
!
!*       1.     Selection of surface scheme
!               ---------------------------
!        
 CALL WRITESURF_ISBA_CONF_n(IM%CHI, IM%ID%DE, IM%ID%O, IM%ID%DM, IM%O, HPROGRAM)
!
 CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE','ISBA_PROGNOSTIC.OUT.nc')
!
 CALL WRITESURF_ISBA_n(HSELECT, OSNOWDIMNC, IM%CHI, NDST, IM%O, IM%S, IM%NP, IM%NPE, &
                       U%NSIZE_NATURE, HPROGRAM,OLAND_USE)
!
IF ((.NOT.LNOWRITE_CANOPY).OR.SIZE(HSELECT)>0) THEN
  CALL WRITESURF_SBL_n(HSELECT, IM%O%LCANOPY, IM%SB, HPROGRAM, HWRITE, "NATURE")
ENDIF
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_ISBA_n
