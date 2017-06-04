!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_CH_AGGR_n (DTCO, HSELECT, U, CHE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_CH_AGGR_n* - writes surface chemical emissions diagnostics
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
!!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_t
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
TYPE(CH_EMIS_FIELD_t), INTENT(INOUT) :: CHE
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER           :: JSPEC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_CH_AGGR_n',0,ZHOOK_HANDLE)
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','WRITE','SURF_ATM_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
!
!         Writes Emissions of all species
!
DO JSPEC=1,SIZE(CHE%TSEMISS)
  YRECFM = "E_"//TRIM(CHE%TSEMISS(JSPEC)%CNAME)
  YCOMMENT = "Emission data at time t (ppm*m/s)"
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,CHE%TSEMISS(JSPEC)%XEMISDATA,IRESP,HCOMMENT=YCOMMENT,&
          HNAM_DIM="Temporal_emiss")
END DO
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_CH_AGGR_n',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_CH_AGGR_n
