!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE GET_SIZES_PARALLEL (DTCO, UG, U, KPROC,KSIZE,KPROCMIN,KSIZE_TASK,OSHADOWS)
!
!
! Modified by B. Decharme  (08/2013): bug in KSIZE_TASK
!
! Modif Matthieu Lafaysse 04/2014
! For shadows routines, we need strictly rectangular subdomains

!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
#ifdef SFX_OL
USE MODD_SLOPE_EFFECT, ONLY : NIX,NIY
USE MODN_IO_OFFLINE, ONLY : CSURF_FILETYPE
#endif
USE MODI_INIT_IO_SURF_n
USE MODI_READ_SURF
USE MODI_READ_GRIDTYPE
USE MODI_GET_GRID_DIM
!RJ: missing modi
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
!
INTEGER, INTENT(IN) :: KPROC
INTEGER, INTENT(IN) :: KSIZE
INTEGER, INTENT(IN) :: KPROCMIN
LOGICAL, INTENT(IN),OPTIONAL :: OSHADOWS

LOGICAL::GSHADOWS

LOGICAL :: GRECT

INTEGER, DIMENSION(0:KPROC-1), INTENT(OUT) :: KSIZE_TASK
!

INTEGER::IRESP

INTEGER :: ISIZE, IRESTE, INRESTE
INTEGER :: J, IPROC
INTEGER :: ISIZE_Y,INY_THREAD,INY_RESTE

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_SIZES_PARALLEL',0,ZHOOK_HANDLE)
!

IF (PRESENT(OSHADOWS)) THEN
  GSHADOWS=OSHADOWS
ELSE
  GSHADOWS=.FALSE.
ENDIF

IF (GSHADOWS) THEN
  ! We want only rectangular subdomains
  
  ! Get x and y dimension lengths
#ifdef SFX_OL
  IF (NIX==0) THEN

    !CALL SET_SURFEX_FILEIN(CSURF_FILETYPE,'PREP ') ! not necessary, it works with PGD or PREP file
CALL INIT_IO_SURF_n(DTCO, U, &
                        CSURF_FILETYPE,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(&
                   CSURF_FILETYPE,'GRID_TYPE',UG%G%CGRID,IRESP,HDIR='A')
    CALL READ_GRIDTYPE(&
                       CSURF_FILETYPE,UG%G%CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,.FALSE.,HDIR="A")
    ALLOCATE(UG%XGRID_FULL_PAR(UG%NGRID_FULL_PAR))
    CALL READ_GRIDTYPE(&
                       CSURF_FILETYPE,UG%G%CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,.TRUE.,UG%XGRID_FULL_PAR,IRESP,HDIR="A")  
    CALL END_IO_SURF_n(CSURF_FILETYPE)
    CALL GET_GRID_DIM(UG%G%CGRID,SIZE(UG%XGRID_FULL_PAR),UG%XGRID_FULL_PAR,GRECT,NIX,NIY)  
    UG%XGRID_FULL_PAR=>NULL()
  ENDIF

  !If get_sizes_parallel is called by init_index_mpi
  !ISIZE_Y represents the number of lines of the total domain
  !INY_THREAD represents the number of lines for 1 MPI thread
  !If get_sizes_parallel is called by offline
  !ISIZE_Y represents the number of lines of 1 MPI thread
  !INY_THREAD represent the number of lines for 1 OPEN-MP thread
  
  ISIZE_Y=KSIZE/NIX

  ! Number of lines (y) for one thread
  INY_THREAD=ISIZE_Y/KPROC
  INY_RESTE=ISIZE_Y-KPROC*INY_THREAD
  
  KSIZE_TASK(:)=INY_THREAD*NIX
  
  DO J=KPROCMIN+KPROC-INY_RESTE,KPROCMIN+KPROC-1
    KSIZE_TASK(MOD(J,KPROC))=KSIZE_TASK(MOD(J,KPROC))+NIX
  END DO
#endif
ELSE
  ISIZE = CEILING(KSIZE*1./KPROC) !nb of points by task

  INRESTE = KPROC*ISIZE - KSIZE ! nb of tasks containing ireste points
  IF (INRESTE>0) THEN
    IRESTE = ISIZE - 1
  ELSE
    IRESTE = ISIZE
  ENDIF
!
  KSIZE_TASK(:) = ISIZE
  IF (INRESTE>0) THEN
    DO J = KPROCMIN+KPROC-INRESTE,KPROCMIN+KPROC-1
      KSIZE_TASK(MOD(J,KPROC)) = IRESTE
    ENDDO
  ENDIF
!
!so:
!(nproc-nreste)*isize + nreste*ireste = ndim_full
!if nreste==1: 
!(nproc-1)*isize + ireste = 
!(nproc-1)*isize + NDIM_FULL - (nproc-1)*isize = NDIM_FULL
!if (ireste==isize-1):
!nproc*isize - nreste*isize + nreste*isize - nreste = 
!nproc*isize - nreste = NDIM_FULL


ENDIF
!
IF (LHOOK) CALL DR_HOOK('GET_SIZES_PARALLEL',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_SIZES_PARALLEL
