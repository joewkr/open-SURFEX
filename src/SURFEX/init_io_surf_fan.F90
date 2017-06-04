!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      SUBROUTINE INIT_IO_SURF_FA_n (DTCO, U, &
                                    HPROGRAM,HMASK,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_FA* Keep in memory the output files
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
!!      B. Decharme   2008  : Change to switch between offline and online run
!!                            In online run, the mask must be always global
!!      B. Decharme   2013  : Allocate work variables to write in FA in AROME case
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NINDEX, NPIO, NSIZE
!
USE MODD_CSTS, ONLY : XPI
!
USE MODD_IO_SURF_FA,ONLY: NUNIT_FA, CFILEIN_FA,CFILEOUT_FA,CDNOMC,IVERBFA,  &
                          NLUOUT,NFULL,NFULL_EXT, CMASK, LOPEN, CFILE_FA,   &
                          NDGL, NDLON, NDLUX, NDGUX, PERPK, PEBETA,         &
                          PELON0, PELAT0, PEDELX, PEDELY, PELON1, PELAT1 
!
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_GET_DIM_FULL_n
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_GET_SURF_MASK_n
USE MODI_GET_1D_MASK
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION    
!
INTEGER                        :: ILU, IRET, IL
!
INTEGER                :: INB ! number of articles in the file
INTEGER                :: ITYPTR, ITRONC, INLATI, INXLON, INIVER
INTEGER, DIMENSION (1000) :: INLOPA, INOZPA
!
REAL, DIMENSION (1000)  :: ZSINLA
REAL, DIMENSION (200)   :: ZAHYBR, ZBHYBR
REAL                    :: ZSLAPO, ZCLOPO, ZSLOPO, ZCODIL, ZREFER
LOGICAL                 :: LOUTFAC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_FA_N',0,ZHOOK_HANDLE)
!
IF(HPROGRAM/='FA    '.AND.HPROGRAM/='AROME ') THEN
  CALL ABOR1_SFX('INIT_IO_SURF_FA_N -- HPROGRAM should be FA or AROME')
ENDIF
!
 CALL GET_LUOUT(HPROGRAM,NLUOUT)
!
NUNIT_FA = 19
!
LOPEN=.FALSE.
!
IF (HACTION=='GTMSK') THEN
  IF (NRANK==NPIO) THEN
    CALL FAITOU(IRET,NUNIT_FA,.TRUE.,CFILEIN_FA,'OLD',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
    WRITE(NLUOUT,*)'HPROGRAM ',HPROGRAM,' IO_INIT HACTION==GTMSK',NUNIT_FA,CFILEIN_FA
    LOPEN=.TRUE.
  ENDIF
  CMASK = HMASK
  CFILE_FA = CFILEIN_FA
  IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_FA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (HACTION == 'READ ') THEN
  CALL FAITOU(IRET,NUNIT_FA,.TRUE.,CFILEIN_FA,'OLD',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
  WRITE(NLUOUT,*)'HPROGRAM ',HPROGRAM,' IO_INIT HACTION==READ',NUNIT_FA,CFILEIN_FA
  CALL FACAGE(CDNOMC,.TRUE.)
  LOPEN=.TRUE.
  !
  CFILE_FA = CFILEIN_FA
  !
  IF (HMASK /= 'EXTZON') THEN
    CMASK = 'FULL '
    CALL READ_SURF(&
                   HPROGRAM,'DIM_FULL',NFULL,IRET,HDIR='A')
    !  
    NFULL_EXT = NFULL
    IF (HPROGRAM=='AROME ') THEN
      U%NDIM_FULL = NFULL
    ENDIF
  ENDIF
  !
ELSE
  ! NFULL must be known in every case. 
  CALL GET_DIM_FULL_n(U%NDIM_FULL, NFULL)
  !
  CFILE_FA = CFILEOUT_FA
ENDIF
!
IF (HMASK == 'EXTZON') THEN
  IF (NRANK==NPIO) THEN
    CALL FACIES(CDNOMC, ITYPTR, ZSLAPO, ZCLOPO, ZSLOPO,       &
                      ZCODIL, ITRONC, INLATI, INXLON, INLOPA, &
                      INOZPA, ZSINLA, INIVER, ZREFER, ZAHYBR, &
                      ZBHYBR, LOUTFAC)
    NFULL_EXT = INLATI*INXLON
    NDGL   = INLATI
    NDLON  = INXLON
    NFULL  = INLOPA(4)*INLOPA(6)
    NDLUX  = INLOPA(4)
    NDGUX  = INLOPA(6)
    PEBETA = ZSLAPO
    PERPK  = ZSINLA(2)
    PELON0 = ZSINLA(3)*180./XPI
    PELAT0 = ZSINLA(4)*180./XPI
    PEDELX = ZSINLA(7)
    PEDELY = ZSINLA(8)
    PELON1 = ZSINLA(13)*180./XPI 
    PELAT1 = ZSINLA(14)*180./XPI
  ENDIF 
ENDIF
!
IF (.NOT.ALLOCATED(NINDEX).AND.NRANK==NPIO) THEN
  ALLOCATE(NINDEX(NFULL))
  NINDEX(:) = 0
ELSEIF (HMASK /= 'EXTZON') THEN
  CALL GET_DIM_FULL_n(U%NDIM_FULL,NFULL)  
ENDIF
!
!------------------------------------------------------------------------------
CMASK=HMASK
!------------------------------------------------------------------------------
!
IF (HPROGRAM=='AROME ') THEN
  NFULL  = U%NDIM_FULL
  ILU    = NFULL
  IL     = NFULL
  NSIZE  = NFULL
ELSE
  CALL GET_SIZE_FULL_n(HPROGRAM,NFULL,U%NSIZE_FULL,ILU)
  IF (ILU>NSIZE) NSIZE = ILU
  IL = ILU
  CALL GET_TYPE_DIM_n(DTCO, U, &
                      HMASK,IL)
ENDIF
!
 CALL GET_MASK(ILU,IL)
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_FA_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE GET_MASK(KLU,KL)
!
USE MODD_MASK,       ONLY: NMASK_FULL
USE MODD_IO_SURF_FA, ONLY: NMASK
!
IMPLICIT NONE
!
INTEGER, INTENT(INOUT) :: KLU
INTEGER, INTENT(IN) :: KL
!
REAL, DIMENSION(KL) :: ZFULL
INTEGER, DIMENSION(KL) :: IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_FA_N:GET_MASK',0,ZHOOK_HANDLE)
!
IF (HPROGRAM=='AROME ') THEN
  ZFULL = 1.
  CALL GET_1D_MASK(KLU,KLU,ZFULL,IMASK)
  IF (ALLOCATED(NMASK_FULL)) THEN
    IF (KLU>SIZE(NMASK_FULL)) DEALLOCATE(NMASK_FULL)
  ENDIF  
ELSE
  CALL GET_SURF_MASK_n(DTCO, U, &
                       HMASK,KL,IMASK,KLU,NLUOUT)
  IF (ALLOCATED(NMASK_FULL)) THEN
    IF (KL>SIZE(NMASK_FULL)) DEALLOCATE(NMASK_FULL)
  ENDIF  
ENDIF
!
IF (.NOT.ALLOCATED(NMASK_FULL)) ALLOCATE(NMASK_FULL(KLU))
!
NMASK_FULL(1:KL) = IMASK(:)
!
NMASK => NMASK_FULL(1:KL)
!
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_FA_N:GET_MASK',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_MASK
!
END SUBROUTINE INIT_IO_SURF_FA_n

