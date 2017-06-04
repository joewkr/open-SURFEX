!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE OPEN_AUX_IO_SURF_LFI (&
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
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_IO_SURF_LFI,ONLY : CLUOUT_LFI,NMASK,NFULL,CMASK, NLUOUT, &
                            NFULL_AUX,CFILE_LFI 
USE MODI_GET_LUOUT
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_1D_MASK
!
IMPLICIT NONE
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
INTEGER                        :: INB ! number of articles in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_LFI',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('LFI   ',NLUOUT)
!
 CALL FMOPEN(HFILE,'OLD',CLUOUT_LFI,0,1,1,INB,IRET)
!
CMASK = HMASK
CFILE_LFI=HFILE
 CALL READ_SURF('LFI   ','DIM_FULL',ILU,IRET,HDIR=HDIR)
NFULL_AUX = ILU
!
!------------------------------------------------------------------------------
NFULL = NFULL_AUX
!
ALLOCATE(ZFULL(NFULL))
IL = NFULL
ZFULL = 1.
!
ALLOCATE(NMASK(IL))
 CALL GET_1D_MASK(IL,IL,ZFULL,NMASK)
!
DEALLOCATE(ZFULL)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_LFI',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF_LFI
