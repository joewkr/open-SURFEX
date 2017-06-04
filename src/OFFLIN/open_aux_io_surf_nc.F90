!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE OPEN_AUX_IO_SURF_NC (&
                                      HFILE,HFILETYPE,HMASK,HDIR)
!     #######################################################
!
!!****  *OPEN_AUX_IO_SURF_ASC* - chooses the routine to OPENialize IO
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_IO_SURF_NC,ONLY: NMASK,NFULL,CMASK, NLUOUT, &
                            CFILEIN_NC, NID_NC, NFULL_AUX 
USE MODI_GET_LUOUT
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_1D_MASK
!
USE NETCDF
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
 CHARACTER(LEN=1), INTENT(IN) :: HDIR  
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),ALLOCATABLE :: ZFULL  ! total cover
INTEGER                        :: ILU,IRET, IL
CHARACTER(LEN=28) :: YFILE
LOGICAL :: GOPENED
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_NC',0,ZHOOK_HANDLE)

YFILE = TRIM(HFILE)//".nc"
INQUIRE(FILE=YFILE,OPENED=GOPENED)
IF (.NOT.GOPENED) THEN
  IRET = NF90_OPEN(YFILE,NF90_NOWRITE,NID_NC)
ENDIF

 CALL GET_LUOUT('NC    ',NLUOUT)
!
CMASK = HMASK
CFILEIN_NC = YFILE
 CALL READ_SURF(&
                'NC    ','DIM_FULL',ILU,IRET,HDIR=HDIR)
NFULL_AUX = ILU
!
!------------------------------------------------------------------------------
NFULL = NFULL_AUX
!
IL = NFULL
ALLOCATE(ZFULL(IL))
ALLOCATE(NMASK(IL))
ZFULL=1.
 CALL GET_1D_MASK(IL,IL,ZFULL,NMASK)
!
DEALLOCATE(ZFULL)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_NC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF_NC
