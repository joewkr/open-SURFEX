!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_SEABATHY(HPROGRAM, HSEABATHY,             &
                                         HSEABATHYFILETYPE, HNCVARNAME,   &
                                         PUNIF_SEABATHY)  
!     ##############################################################
!
!!**** *READ_NAM_PGD_SEABATHY* reads namelist for SEABATHY
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    C. Lebeaupin Brossier        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2008
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM          ! Type of program
 CHARACTER(LEN=28),   INTENT(OUT)   :: HSEABATHY         ! file name for bathymetry
 CHARACTER(LEN=6),    INTENT(OUT)   :: HSEABATHYFILETYPE ! bathymetry data file type
 CHARACTER(LEN=28),   INTENT(OUT)   :: HNCVARNAME        ! variable to read in netcdf
                                                        ! file
REAL,                INTENT(OUT)   :: PUNIF_SEABATHY    ! uniform value of bathymetry
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YSEABATHY         ! file name for bathymetry
 CHARACTER(LEN=6)         :: YSEABATHYFILETYPE ! bathymetry data file type
 CHARACTER(LEN=28)        :: YNCVARNAME        ! variable to read in netcdf
                                              ! file
REAL                     :: XUNIF_SEABATHY    ! uniform value of bathymetry
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_SEABATHY/ YSEABATHY, YSEABATHYFILETYPE, YNCVARNAME, XUNIF_SEABATHY
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_SEABATHY',0,ZHOOK_HANDLE)
XUNIF_SEABATHY     = -300.
YSEABATHY          = '                          '
YSEABATHYFILETYPE  = '      '
YNCVARNAME='rose                      '
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_SEABATHY',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SEABATHY)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
HSEABATHY         = YSEABATHY         ! file name for bathymetry
HSEABATHYFILETYPE = YSEABATHYFILETYPE ! bathymetry data file type
HNCVARNAME        = YNCVARNAME        ! variable to read in netcdf
PUNIF_SEABATHY    = XUNIF_SEABATHY    ! uniform value of bathymetry
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_SEABATHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_SEABATHY
