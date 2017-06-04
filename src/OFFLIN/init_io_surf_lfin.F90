!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_LFI_n (DTCO, U, &
                                     HMASK,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_LFI* Keep in memory the output files
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
USE MODD_SURFEX_MPI, ONLY : NRANK, NINDEX, NPIO, NSIZE
!
USE MODD_SURF_PAR,   ONLY: NUNDEF
!
USE MODD_IO_SURF_LFI,ONLY: CFILE_LFI, CFILEIN_LFI,CFILEOUT_LFI,   &
                           NMASK,CLUOUT_LFI,NFULL,CMASK, NLUOUT,  &
                           NFULL_SURF,                            &
                           NIB, NIE, NJB, NJE, NIU, NJU,          &
                           NIB_SURF, NIE_SURF, NJB_SURF, NJE_SURF,&
                           NIU_SURF, NJU_SURF  
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
INTEGER                :: INB ! number of articles in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_LFI_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT('LFI   ',NLUOUT)
!
IF (HACTION=='GTMSK') THEN
  IF (NRANK==NPIO) THEN 
    CALL FMOPEN(CFILEIN_LFI,'OLD',CLUOUT_LFI,0,1,1,INB,IRET)
    CFILE_LFI = CFILEIN_LFI
  ENDIF
  CMASK = HMASK
  IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_LFI_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (HACTION == 'READ ') THEN
  CALL FMOPEN(CFILEIN_LFI,'OLD',CLUOUT_LFI,0,1,1,INB,IRET)
  CFILE_LFI = CFILEIN_LFI
  CALL READ_SURF(&
                 'LFI   ','DIM_FULL',NFULL,IRET,HDIR='A')
  IF (HMASK == 'FULL  ') THEN
    NFULL_SURF = NFULL
    NIB_SURF = NIB
    NIE_SURF = NIE
    NJB_SURF = NJB
    NJE_SURF = NJE
    NIU_SURF = NIU
    NJU_SURF = NJU
   ENDIF
ELSE
  CALL GET_DIM_FULL_n(U%NDIM_FULL, NFULL)
ENDIF
!
!
IF (HACTION=='WRITE' .AND. NRANK==NPIO) THEN
  IF (NRANK==NPIO) THEN
    CALL FMOPEN(CFILEOUT_LFI,'UNKNOWN',CLUOUT_LFI,0,1,1,INB,IRET)
  ENDIF
  CFILE_LFI = CFILEOUT_LFI
ENDIF
!
!*       initialisation of 2D arrays
! 
IF (NIB_SURF/=NUNDEF) THEN
  NIB = NIB_SURF
  NIE = NIE_SURF
  NJB = NJB_SURF
  NJE = NJE_SURF
  NIU = NIU_SURF
  NJU = NJU_SURF
END IF
!
! nindex is needed for call to get_size_full_n. In init_index_mpi, 
! it's not initialized for first readings.
IF (.NOT.ALLOCATED(NINDEX).AND.NRANK==NPIO) THEN
  ALLOCATE(NINDEX(NFULL))
  NINDEX(:) = 0
ELSE
  CALL GET_DIM_FULL_n(U%NDIM_FULL,NFULL)  
ENDIF  
!
!------------------------------------------------------------------------------
!
! MASK is sized according to the mpi task running
 CALL GET_SIZE_FULL_n('LFI   ',NFULL,U%NSIZE_FULL,ILU)
IF (ILU>NSIZE) NSIZE = ILU
!
IL = ILU
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     HMASK,IL)
 CALL INIT_IO_SURF_MASK_n(DTCO, U, &
                          HMASK, IL, NLUOUT, ILU, NMASK)
!
!------------------------------------------------------------------------------
CMASK = HMASK
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_LFI_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IO_SURF_LFI_n
