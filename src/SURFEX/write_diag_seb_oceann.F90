!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_OCEAN_n (DTCO, HSELECT, U, DGO, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_OCEAN_n* - write the oceanic diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      C. Lebeaupin Brossier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
!
USE MODD_OCEAN_GRID
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DGO
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_OCEAN_N',0,ZHOOK_HANDLE)
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'SEA   ','SEAFLX','WRITE','SEAFLUX_DIAGNOSTICS.OUT.nc')
!
!
!*       2.     Mean values in OML :
!               --------------------
!
  YRECFM='TOML'
  YCOMMENT='X_Y_'//YRECFM
!
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DGO%XTOCMOY(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='SOML'
  YCOMMENT='X_Y_'//YRECFM
!
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DGO%XSOCMOY(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='UOML'
  YCOMMENT='X_Y_'//YRECFM
!
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DGO%XUOCMOY(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='VOML'
  YCOMMENT='X_Y_'//YRECFM
!
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DGO%XVOCMOY(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='DOML'
  YCOMMENT='X_Y_'//YRECFM
!
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DGO%XDOCMOY(:),IRESP,HCOMMENT=YCOMMENT)
!
!------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_OCEAN_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_OCEAN_n
