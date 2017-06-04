SUBROUTINE SFX_XIOS_READNAM_OL(HNAMELIST)
!!
!!
!!     PURPOSE : Assess if XIOS output mode is required
!!     --------
!!
!!
!!     METHOD
!!     ------
!!
!!     Check namelist to assess if XIOS is required as diags output format and set 
!!     MODD_XIOS::LXIOS accordingly
!!
!!     EXTERNAL
!!     --------
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2015
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT

USE MODN_IO_OFFLINE

USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODE_POS_SURF
!
USE MODD_XIOS, ONLY : LXIOS, LXIOS_DEF_CLOSED
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=28), INTENT(IN )           :: HNAMELIST
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER            :: ILUNAM
INTEGER            :: ILUOUT
LOGICAL            :: GFOUND
!
! For now, some issued if DrHook is called before Oasis setup, hence :
!
LXIOS           = .FALSE.
LXIOS_DEF_CLOSED= .FALSE.
!
CALL OPEN_NAMELIST('ASCII ',ILUNAM,HNAMELIST)
!
CALL GET_LUOUT('OFFLINE',ILUOUT)
CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND,ILUOUT)
IF (GFOUND) THEN  
   READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
ELSE
   CALL ABOR1_SFX('SFX_XIOS_READNAM_OL : CANNOT READ NAMELIST FILE '//HNAMELIST//' FOR NAMELIST NAM_IO_OFFLINE')
ENDIF
LXIOS = (TRIM(CTIMESERIES_FILETYPE) == 'XIOS') 
!
!
CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
IF (LXIOS) THEN
!
#ifndef WXIOS
   CALL ABOR1_SFX('SFX_XIOS_READNAM_OL : CANNOT USE "CTIMESERIES_FILETYPE = XIOS" : THIS BINARY WAS COMPILED WITHOUT XIOS SUPPORT')
#endif
!
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_READNAM_OL
