!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.

!     #########
MODULE MODI_MAKE_LCOVER
CONTAINS
      SUBROUTINE MAKE_LCOVER(OCOVER)
!     ##############################################################
!
!!**** *PGD_COVER* monitor for averaging and interpolations of cover fractions
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
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC, NCOMM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef SFX_MNH
USE MODE_FD_ll, ONLY : GETFD,FD_ll
USE MODD_IO_ll, ONLY : ISP, ISNPROC
USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD
#endif
!
IMPLICIT NONE
!
#if defined(SFX_MPI) || defined(SFX_MNH)
INCLUDE "mpif.h"
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
LOGICAL, DIMENSION(:), INTENT(INOUT) :: OCOVER
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
#ifdef SFX_MNH
TYPE(FD_ll), POINTER                     :: TZFD
#endif
!
INTEGER :: INFOMPI, JPROC, JCOVER
!
INTEGER :: IRANK_SAVE, IPROC_SAVE, IPIO_SAVE, ICOMM_SAVE
!
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GCOVER_ALL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('MAKE_LCOVER',0,ZHOOK_HANDLE)
!
#ifdef SFX_MNH
TZFD=>GETFD(NMNH_COMM_WORLD)
!
IRANK_SAVE = NRANK
IPROC_SAVE = NPROC
IPIO_SAVE = NPIO
ICOMM_SAVE = NCOMM
!
! on met les infos de mésonh
NRANK = ISP-1
NPROC = ISNPROC
NPIO = TZFD%OWNER-1
NCOMM = TZFD%COMM
#endif
!
ALLOCATE(GCOVER_ALL(SIZE(OCOVER),0:NPROC-1))
!
!
IF (NPROC>1) THEN
#if defined(SFX_MPI) || defined(SFX_MNH)
  CALL MPI_ALLGATHER(OCOVER,SIZE(OCOVER),MPI_LOGICAL,GCOVER_ALL,SIZE(OCOVER),&
                  MPI_LOGICAL,NCOMM,INFOMPI)
#endif
ELSE
  GCOVER_ALL(:,0) = OCOVER(:)
ENDIF
!
!
OCOVER(:) = .FALSE.
DO JPROC = 0,NPROC-1
  DO JCOVER=1,SIZE(OCOVER)
    IF (GCOVER_ALL(JCOVER,JPROC)) OCOVER(JCOVER) = .TRUE.
  ENDDO
ENDDO
!
DEALLOCATE(GCOVER_ALL)
!
!
IF (NPROC>1) THEN
#if defined(SFX_MPI) || defined(SFX_MNH)
  CALL MPI_BCAST(OCOVER,SIZE(OCOVER),MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
#ifdef SFX_MNH
NRANK = IRANK_SAVE
NPROC = IPROC_SAVE
NPIO = IPIO_SAVE
NCOMM = ICOMM_SAVE
#endif
!
IF (LHOOK) CALL DR_HOOK('MAKE_LCOVER',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_LCOVER
END MODULE MODI_MAKE_LCOVER
