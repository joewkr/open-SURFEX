!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MISC_FLAKE_n ( DTCO, HSELECT, U, DMF, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MISC_FLAKE* - writes the FLAKE miscellaneous diagnostic fields
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DMF
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: IZ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_FLAKE_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','FLAKE ','WRITE','FLAKE_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
!
!* Flake temperature profile
!
IF (DMF%LWATER_PROFILE) THEN      
   DO IZ=1,SIZE(DMF%XZW_PROFILE)
      WRITE(YRECFM,'(F5.1)') DMF%XZW_PROFILE(IZ)
      YRECFM='TW_'//TRIM(ADJUSTL(YRECFM))
      YCOMMENT='X_Y_'//YRECFM//' (K)'
      CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DMF%XTW_PROFILE(IZ,:),IRESP,HCOMMENT=YCOMMENT)
   END DO
END IF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DIAG_MISC_FLAKE_n
