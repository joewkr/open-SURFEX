!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_MASK_n (DTCO, U, &
                                      HMASK,KSIZE,KLUOUT,KFULL,KMASK)
!     ######################
!
!!****  *INIT_IO_SURF_MASK* Keep in memory the output files
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
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_MASK, ONLY: NMASK_FULL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
IMPLICIT NONE
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN) :: HMASK
INTEGER,           INTENT(IN) :: KSIZE
INTEGER,           INTENT(IN) :: KLUOUT
INTEGER,           INTENT(INOUT) :: KFULL
INTEGER, POINTER, DIMENSION(:):: KMASK
!
INTEGER, DIMENSION(KSIZE)     :: IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_MASK_N',0,ZHOOK_HANDLE)
!
 CALL GET_SURF_MASK_n(DTCO, U, &
                      HMASK,KSIZE,IMASK,KFULL,KLUOUT)
!
IF (ALLOCATED(NMASK_FULL)) THEN
  IF (KSIZE>SIZE(NMASK_FULL)) DEALLOCATE(NMASK_FULL)
ENDIF
IF (.NOT.ALLOCATED(NMASK_FULL)) ALLOCATE(NMASK_FULL(KFULL))
!
NMASK_FULL(1:KSIZE)=IMASK(:)
!
KMASK => NMASK_FULL(1:KSIZE)
!
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_MASK_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_IO_SURF_MASK_n
