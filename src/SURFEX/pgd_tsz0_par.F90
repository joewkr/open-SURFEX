!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TSZ0_PAR (DTZ, &
                               HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TSZ0_PAR* monitor for averaging and interpolations of sst
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
!!    P. Le Moigne        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     09/2007
!!    P. Le Moigne 03/2015 tsz0 time management
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
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
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JTIME     ! loop counter on time
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER            :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 37
!
REAL, DIMENSION(NTIME_MAX)     :: XUNIF_DTS
REAL, DIMENSION(NTIME_MAX)     :: XUNIF_DHUGRD
 CHARACTER(LEN=28), DIMENSION(NTIME_MAX)  :: CFNAM_DTS
 CHARACTER(LEN=28), DIMENSION(NTIME_MAX)  :: CFNAM_DHUGRD
 CHARACTER(LEN=6), DIMENSION(NTIME_MAX)   :: CFTYP_DTS
 CHARACTER(LEN=6), DIMENSION(NTIME_MAX)   :: CFTYP_DHUGRD
!
! name of files containing data
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TSZ0/NTIME, XUNIF_DTS, XUNIF_DHUGRD  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TSZ0_PAR',0,ZHOOK_HANDLE)
NTIME             = 25
XUNIF_DTS (:)     = -0.250
XUNIF_DHUGRD(:)   = 0.0
CFNAM_DTS   (:) = '                            '
CFNAM_DHUGRD(:) = '                            '
CFTYP_DTS   (:) = '      '
CFTYP_DHUGRD(:) = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TSZ0',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TSZ0)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (NTIME > NTIME_MAX) THEN
   WRITE(ILUOUT,*)'NTIME SHOULD NOT EXCEED',NTIME_MAX
   CALL ABOR1_SFX('PGD_TSZ0_PAR: NTIME TOO BIG')
ENDIF
!
ALLOCATE(DTZ%XDATA_DTS    (NTIME))
ALLOCATE(DTZ%XDATA_DHUGRD (NTIME))
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
IF (NTIME==1) THEN
  DTZ%XDATA_DTS   (:) = XUNIF_DTS   (1)
  DTZ%XDATA_DHUGRD(:) = XUNIF_DHUGRD(1)
ELSE
  DO JTIME=1,NTIME
    DTZ%XDATA_DTS   (JTIME) = XUNIF_DTS   (JTIME)
    DTZ%XDATA_DHUGRD(JTIME) = XUNIF_DHUGRD(JTIME)
  END DO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PGD_TSZ0_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TSZ0_PAR
