!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SUM_ON_ALL_PROCS_OL(HGRID,KSIZE,KIN,KOUT,HNAME)
!     #######################################################
!
!
!!****  *SUM_ON_ALL_PROCS_OL* - sums the values of the integers provided on each processor
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
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, NDIM_FULL_INIT
!
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=10),         INTENT(IN)    :: HGRID ! grid type
INTEGER,                   INTENT(IN)    :: KSIZE ! size of integer array
INTEGER, DIMENSION(KSIZE), INTENT(IN)    :: KIN   ! integer array to sum
INTEGER,                   INTENT(INOUT) :: KOUT  ! sum of all integers
 CHARACTER(LEN=3),          INTENT(IN)    :: HNAME ! name of type of field
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(NDIM_FULL_INIT) :: IIN
INTEGER :: INFOMPI
REAL(KIND=JPRB)           :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUM_ON_ALL_PROCS_OL',0,ZHOOK_HANDLE)
!
!* sum of field
!
 CALL GATHER_AND_WRITE_MPI(KIN,IIN)
IF (NRANK==NPIO) THEN
  KOUT = SUM(IIN)
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(KOUT,KIND(KOUT)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SUM_ON_ALL_PROCS_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUM_ON_ALL_PROCS_OL
