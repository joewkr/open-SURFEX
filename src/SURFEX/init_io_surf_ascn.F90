!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_ASC_n (DTCO, U, HMASK,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_ASC* Keep in memory the output files
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      P. Le Moigne 04/2004: distinguish in and out file name
!!      P. Le Moigne 04/2006: special HACTION='GTMSK' to initialize
!!                            a mask different of 'FULL ' in order 
!!                            to read dimensions only.
!!      S. Faroux 06/2012 : implementations for MPI
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NINDEX, NSIZE, NPIO
!
USE MODD_IO_SURF_ASC,ONLY: NUNIT,CFILEIN,CFILEOUT,NMASK,NLUOUT,NFULL,CMASK, &
                           LCREATED,CFILE
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_GET_DIM_FULL_n
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_INIT_IO_SURF_MASK_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION    
!
INTEGER                        :: ILU,IRET, IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT('ASCII ',NLUOUT)
!
NUNIT=20
!
IF (HACTION=='GTMSK') THEN
  IF (NRANK==NPIO) THEN
    OPEN(UNIT=NUNIT,FILE=CFILEIN,FORM='FORMATTED')
  ENDIF
  CMASK = HMASK
  CFILE = CFILEIN
  IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (HACTION == 'READ ') THEN
  OPEN(UNIT=NUNIT,FILE=CFILEIN,FORM='FORMATTED')
  ! NFULL must be known even if HMASK/=FULL because it's no longer 
  ! updated in init_io_surf_maskn.
  CMASK = 'FULL ' 
  CALL READ_SURF(&
                 'ASCII ','DIM_FULL',NFULL,IRET,HDIR='A')
  CMASK = HMASK
  CFILE = CFILEIN
ELSE
  IF (NRANK==NPIO) THEN
    IF (LCREATED) THEN
      OPEN(UNIT=NUNIT,FILE=CFILEOUT,FORM='FORMATTED',POSITION='APPEND')
    ELSE
      OPEN(UNIT=NUNIT,FILE=CFILEOUT,FORM='FORMATTED')
      LCREATED=.TRUE.
    ENDIF
  ENDIF
  ! NFULL must be known in every case. 
  CALL GET_DIM_FULL_n(U%NDIM_FULL, NFULL)
  CMASK = HMASK
  CFILE = CFILEOUT
ENDIF
!
! nindex is needed for call to get_size_full_n. In init_index_mpi, 
! it's not initialized for first readings.  
IF (.NOT.ALLOCATED(NINDEX) .AND. NRANK==NPIO) THEN
  ALLOCATE(NINDEX(NFULL))
  NINDEX(:) = 0
ELSE
  CALL GET_DIM_FULL_n(U%NDIM_FULL,NFULL)  
ENDIF
!
!------------------------------------------------------------------------------
!
! MASK is sized according to the mpi task running
 CALL GET_SIZE_FULL_n('ASCII ',NFULL,U%NSIZE_FULL,ILU)
IF (ILU>NSIZE) NSIZE = ILU
!
IL = ILU
 CALL GET_TYPE_DIM_n(DTCO, U,HMASK,IL)
 CALL INIT_IO_SURF_MASK_n(DTCO, U, HMASK, IL, NLUOUT, ILU, NMASK)
!
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
END SUBROUTINE INIT_IO_SURF_ASC_n
