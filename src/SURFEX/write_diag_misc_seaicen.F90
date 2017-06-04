!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MISC_SEAICE_n (DTCO, HSELECT, U, DGMSI, S, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_SEAICE_n* - write the seaice diagnostic fields
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
!!      S.Senesi                *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
!
!
!
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_MISC_SEAICE_t), INTENT(INOUT) :: DGMSI
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
INTEGER           :: JSV, JSW
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_SEAICE_N',0,ZHOOK_HANDLE)
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'SEA   ','SEAFLX','WRITE','SEAFLUX_DIAGNOSTICS.OUT.nc')
!
IF(LCPL_SEAICE.OR.S%LHANDLE_SIC)THEN      
!
  YCOMMENT='Sea-ice temperature (K)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'TSICE',S%XTICE(:),IRESP,YCOMMENT)
!
  YCOMMENT='Sea-ice albedo (-)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'IALB',S%XICE_ALB(:),IRESP,YCOMMENT)
!
ENDIF
!
IF (TRIM(S%CSEAICE_SCHEME) == 'GELATO') THEN 
    YCOMMENT='Sea-ice thickness (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SIT',DGMSI%XSIT(:),IRESP,YCOMMENT)
    !
    YCOMMENT='Sea-ice snow depth (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SND',DGMSI%XSND(:),IRESP,YCOMMENT)
    !
    YCOMMENT='Sea mixed layer temp for Glt (K)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'SIMLT',DGMSI%XMLT(:),IRESP,YCOMMENT)
    !
ENDIF
!               -------------
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)

IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_SEAICE_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_MISC_SEAICE_n
