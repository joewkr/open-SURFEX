!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE OPEN_AUX_IO_SURF_FA (&
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
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA,CFILEIN_FA,NMASK,NLUOUT,NFULL,CMASK, &
                           CDNOMC,IVERBFA
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_IO_BUFF_CLEAN
USE MODI_GET_SURF_MASK_n
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
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
 CHARACTER(LEN=1), INTENT(IN) :: HDIR
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                :: INB ! number of articles in the file
INTEGER, DIMENSION(:),POINTER  :: IMASK
INTEGER                        :: ILU, IRET, IL
REAL, DIMENSION(:),ALLOCATABLE :: ZFULL  ! total cover
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_FA',0,ZHOOK_HANDLE)
CALL IO_BUFF_CLEAN
CALL GET_LUOUT(HFILETYPE,NLUOUT)
!
NUNIT_FA = 20+NRANK
CDNOMC = 'extern'
!
ILU  =0
!
CFILEIN_FA = ADJUSTL(ADJUSTR(HFILE)//'.fa')
 CALL FAITOU(IRET,NUNIT_FA,.TRUE.,CFILEIN_FA,'OLD',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
WRITE(NLUOUT,*)' FA OPEN_AUX_IO_SURF ',NUNIT_FA,CFILEIN_FA
 CALL FACAGE(CDNOMC,.TRUE.)
!
CMASK = 'FULL  '
CALL READ_SURF(HFILETYPE,'DIM_FULL',ILU,IRET,HDIR=HDIR)
NFULL = ILU
!
!------------------------------------------------------------------------------
ALLOCATE(ZFULL(NFULL))
IL = NFULL
ZFULL = 1.
!  
ALLOCATE(NMASK(IL))
CALL GET_1D_MASK(IL,NFULL,ZFULL,NMASK)
!
DEALLOCATE(ZFULL)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('OPEN_AUX_IO_SURF_FA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_AUX_IO_SURF_FA
