!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_CH_SNAP_n (DTCO, HSELECT, U, CHN, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_CH_SNAP_n* - writes surface chemical emissions diagnostics
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson & S. Queguiner  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2012
!!    M.Leriche 04/2014  change emissions name EMIS_ -> E_ name for coherence with PGD
!!                       change length of CHARACTER for emission 6->12
!!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
!
USE MODD_CSTS,        ONLY : XAVOGADRO
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER           :: JSPEC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_CH_SNAP_n',0,ZHOOK_HANDLE)
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','WRITE','SURF_ATM_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
!
!         Writes Emissions of all species
!
DO JSPEC=1,CHN%NEMIS_NBR
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))
  YCOMMENT = "Emission data at time t (ppm*m/s)"
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,CHN%XEMIS_FIELDS(:,JSPEC),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_CH_SNAP_n',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_CH_SNAP_n
