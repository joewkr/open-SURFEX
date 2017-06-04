!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_PREP_OL (IO, S, UG, U, HPROGRAM,HALG_MPI)
!#############################################
!
!!****  *SFX_OASIS_PREP_OL* - Offline driver for definitions for exchange of coupling fields
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_SURFEX_MPI,     ONLY : NPROC, NRANK, NINDEX
!
!
USE MODI_SFX_OASIS_PREP
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
CHARACTER(LEN=4),   INTENT(IN)  :: HALG_MPI   ! decomposition algorithm
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IPARAL
INTEGER, DIMENSION(:), ALLOCATABLE :: ISEG_SIZE
INTEGER, DIMENSION(:), ALLOCATABLE :: ISEG_OFFSET
!
INTEGER, DIMENSION(U%NDIM_FULL+1) :: IINDEX
!
INTEGER :: JPROC, JI, JSEG
INTEGER :: ISEGMENT, ICOUNT, INPAR, INPTS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP_OL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS 
!-------------------------------------------------------------------------------
!
!*       1.     Define ORANGE parallel partitions:
!               ----------------------------------
!
! Number of segments for this proc
!
IINDEX(1:U%NDIM_FULL)=NINDEX(:)
IINDEX(U%NDIM_FULL+1)=NPROC+10
ISEGMENT=0
DO JI=1,U%NDIM_FULL
   IF(NINDEX(JI)==NRANK.AND.NINDEX(JI)/=IINDEX(JI+1))THEN
      ISEGMENT=ISEGMENT+1
   ENDIF
ENDDO
!  
INPAR=2+2*ISEGMENT
!
! Local offset and extent for this proc
!
ALLOCATE(ISEG_SIZE  (ISEGMENT))
ALLOCATE(ISEG_OFFSET(ISEGMENT))
!
ISEGMENT=0
ICOUNT=0
DO JI=1,U%NDIM_FULL
   IF(NINDEX(JI)==NRANK)THEN
     ICOUNT=ICOUNT+1
   ENDIF
   IF(NINDEX(JI)==NRANK.AND.NINDEX(JI)/=IINDEX(JI+1))THEN
      ISEGMENT             =ISEGMENT+1
      ISEG_SIZE  (ISEGMENT)=ICOUNT
      ISEG_OFFSET(ISEGMENT)=JI-ICOUNT
      ICOUNT=0
   ENDIF
ENDDO
!
ALLOCATE(IPARAL(INPAR))
!
! OASIS orange partition
!
IPARAL(CLIM_STRATEGY) = CLIM_ORANGE
!
! Number of proc segments for OASIS
!
IPARAL(2) = ISEGMENT
!
! Local offset and extent for OASIS
!
JI=2
INPTS=0
DO JSEG=1,ISEGMENT
   JI=JI+1
   IPARAL(JI) = ISEG_OFFSET(JSEG)
   JI=JI+1
   IPARAL(JI) = ISEG_SIZE(JSEG)
   INPTS=INPTS+ISEG_SIZE(JSEG)
ENDDO
!  
DEALLOCATE(ISEG_SIZE  )
DEALLOCATE(ISEG_OFFSET)
!
!
!*       2.     Put definitions for exchange of coupling fields :
!               -------------------------------------------------
!
CALL SFX_OASIS_PREP(IO, S, UG, U, HPROGRAM,INPTS,IPARAL)
!
DEALLOCATE(IPARAL)
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP_OL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_PREP_OL
