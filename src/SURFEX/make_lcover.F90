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
INTEGER :: INFOMPI, JPROC, JCOVER, IPROC
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
IPROC = NPROC
!
#if defined(SFX_MPI) || defined(SFX_MNH)
NCOMM = MPI_COMM_WORLD
CALL MPI_COMM_SIZE(NCOMM,NPROC,INFOMPI)
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
OCOVER(:) = .FALSE.
DO JPROC = 0,NPROC-1
  DO JCOVER=1,SIZE(OCOVER)
    IF (GCOVER_ALL(JCOVER,JPROC)) OCOVER(JCOVER) = .TRUE.
  ENDDO
ENDDO
!
DEALLOCATE(GCOVER_ALL)
!
IF (NPROC>1) THEN
#if defined(SFX_MPI) || defined(SFX_MNH)
  CALL MPI_BCAST(OCOVER,SIZE(OCOVER),MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
NPROC = IPROC
!
IF (LHOOK) CALL DR_HOOK('MAKE_LCOVER',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_LCOVER
END MODULE MODI_MAKE_LCOVER
