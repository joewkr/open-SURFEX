!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_NC_n (DTCO, U, HMASK,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_NC* Keep in memory the netcdf ID of the output files
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
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      modified 05/04 by P. LeMoigne *Meteo France*
!!      modified 06/10 by S. Faroux *Meteo France*
!!=================================================================
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
!
USE MODD_SURFEX_MPI, ONLY : NINDEX, NPIO, NRANK
!
!
USE MODD_IO_SURF_NC, ONLY : NMASK, CFILEIN_NC, CFILEOUT_NC, LMASK, NID_NC, &
                            CMASK, LCREATED, CFILEOUT_NC_SAVE, LDEF, CFILE_NC, &
                            NFULL
!
USE MODE_GRIDTYPE_GAUSS
!
USE MODI_READ_SURF
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_INIT_IO_SURF_MASK_n
!
USE MODI_GET_DIM_FULL_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO

TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION 
!
INTEGER :: IL, ILU, ILUOUT, IRET
LOGICAL          :: GEXIST, GOPENED
!
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!------------------------------------------------------------------------------ 
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_NC_N',0,ZHOOK_HANDLE)
!
LMASK = .TRUE.
!
IF (HACTION=='READ ') THEN
  INQUIRE(FILE=CFILEIN_NC,EXIST=GEXIST)
  CFILE_NC = CFILEIN_NC
  IF (GEXIST) THEN 
    IRET = NF90_OPEN(CFILEIN_NC,NF90_NOWRITE,NID_NC)
    CALL READ_SURF('NC    ','DIM_FULL',NFULL,IRET,HDIR='A')
  ENDIF
ELSE 
  CALL GET_DIM_FULL_n(U%NDIM_FULL, NFULL)
  IF (NRANK==NPIO) THEN
    INQUIRE(FILE=CFILEOUT_NC,EXIST=GEXIST)
    INQUIRE(FILE=CFILEOUT_NC,OPENED=GOPENED)
    IF (.NOT.GOPENED) THEN
      IRET = NF90_OPEN(CFILEOUT_NC,NF90_WRITE,NID_NC)
    ENDIF
    IF (LDEF) IRET = NF90_REDEF(NID_NC)
  ENDIF
  CFILE_NC = CFILEOUT_NC
ENDIF
!
! nindex is needed for call to get_size_full_n. In init_index_mpi, 
! it's not initialized for first readings.  
IF (.NOT.ALLOCATED(NINDEX).AND.NRANK==NPIO) THEN
  ALLOCATE(NINDEX(NFULL))
  NINDEX(:) = 0
ELSE
  CALL GET_DIM_FULL_n(U%NDIM_FULL, NFULL)
ENDIF
!
! size by MPI task. NINDEX is supposed to be initialized at this step.  
 CALL GET_SIZE_FULL_n('OFFLIN',NFULL,U%NSIZE_FULL,ILU)
!
IL = ILU
 CALL GET_TYPE_DIM_n(DTCO, U, HMASK,IL)
!
 CALL INIT_IO_SURF_MASK_n(DTCO, U, HMASK, IL, ILUOUT, ILU, NMASK)
!
CMASK = HMASK
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_NC_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IO_SURF_NC_n
