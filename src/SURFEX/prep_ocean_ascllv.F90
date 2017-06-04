!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE PREP_OCEAN_ASCLLV (DTCO, UG, U, HPROGRAM,HSURF,HFILE, KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_OCEAN_ASCLLDV* - prepares oceanic fields from personal data in ascii
!!                            formed as lat,lon, depth, value
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!   Read the input file which must be ascii typed, lat,lon,depth, value
!!   Version 1: 
!!              The data must be on the same grid as the pgd and on the same
!!              vertical grid as prescribed in oceanvergrid.f90
!!              NDEPTH= a definirtn nlev=NOCKMAX (modd_ocean_gridn)
!!              
!!  Version 2: (not done yet)
!!              - dummy or namlist for nb verticals levels
!!              - file prescribing the vertical grid 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. PEYRILLE
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2011
!!      J.Escobar   11/2013   Add USE MODI_ABOR1_SFX and USE MODI_GET_SURF_MASK_N
!!------------------------------------------------------------------
!
USE MODD_SURFEX_MPI, ONLY : NPROC, NINDEX, NNUM, NCOMM, NPIO, NRANK
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_PGD_GRID,       ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_OCEAN_GRID , ONLY : NOCKMAX
USE MODD_PGDWORK,        ONLY : CATYPE
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_GET_LUOUT
USE MODI_GET_LATLONMASK_n
USE MODI_PACK_SAME_RANK
USE MODI_ABOR1_SFX
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
!CHARACTER(LEN=28),  INTENT(IN), OPTIONAL   :: HNCVARNAME!var to read 
REAL, POINTER, DIMENSION(:,:,:)   :: PFIELD    ! field to interpolate horizontally
!
!
!*      0.2    declarations of local variables
REAL,DIMENSION(:), ALLOCATABLE :: ZLAT
REAL,DIMENSION(:), ALLOCATABLE :: ZLON
REAL,DIMENSION(:), ALLOCATABLE :: ZDEPTH
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD
REAL,DIMENSION(:,:,:,:), ALLOCATABLE :: ZFIELDR
!
INTEGER, DIMENSION(0:NPROC-1) :: INB
!
INTEGER :: IL
INTEGER :: IGLB  ! logical unit
INTEGER :: IDIM, ILU
INTEGER :: JI,JK
INTEGER :: INFOMPI, JJ
!
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
 CHARACTER(LEN=6)    :: YMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_ASCLLV',0,ZHOOK_HANDLE)
!
IF (.NOT.ALLOCATED(NNUM)) THEN
  ALLOCATE(NNUM(U%NDIM_FULL))
  IF (NRANK/=NPIO) THEN
    IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
    ALLOCATE(NINDEX(U%NDIM_FULL))
  ENDIF  
  IF (NRANK==NPIO) THEN
    INB(:) = 0
    DO JJ=1,U%NDIM_FULL
      INB(NINDEX(JJ)) = INB(NINDEX(JJ))+1
      NNUM(JJ) = INB(NINDEX(JJ))
    ENDDO
  ENDIF
  IF (NPROC>1) THEN
#ifdef SFX_MPI          
    CALL MPI_BCAST(NINDEX,SIZE(NINDEX)*KIND(NINDEX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NNUM,SIZE(NNUM)*KIND(NNUM)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(UG%NGRID_FULL_PAR,KIND(UG%NGRID_FULL_PAR)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
    IF (NRANK/=NPIO) ALLOCATE(UG%XGRID_FULL_PAR(UG%NGRID_FULL_PAR))
#ifdef SFX_MPI    
    CALL MPI_BCAST(UG%XGRID_FULL_PAR,&
      SIZE(UG%XGRID_FULL_PAR)*KIND(UG%XGRID_FULL_PAR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)   
#endif 
  ENDIF
ENDIF
!
CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
 CALL GET_TYPE_DIM_n(DTCO, U, 'FULL  ',NL)
!*      2.    get Ocean dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, 'SEA   ',IL)

!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(UG, LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!!

WRITE(KLUOUT,*) "==================================== "
WRITE(KLUOUT,*) "Control print in prep_ocean_ascllv "
WRITE(KLUOUT,*) "NL, NOCKMAX", NL,NOCKMAX

ALLOCATE(ZLAT(NL))
ALLOCATE(ZLON(NL))
ALLOCATE(ZDEPTH(NOCKMAX))

ALLOCATE(ZFIELDR(NL,NOCKMAX,4, 1))
ALLOCATE(ZFIELD(NL,NOCKMAX, 1))


WRITE(KLUOUT,*) "ZFIELDR",SHAPE(ZFIELDR)
WRITE(KLUOUT,*) "File name used in ocean ascllv", HFILE

WRITE(KLUOUT,*)  "USURF= " , HSURF
WRITE(KLUOUT,*) "NL (dim)=", NL
WRITE(KLUOUT,*) "IL (dim)=", IL
!
!*      2.     Reading of field
!              ----------------
 CALL OPEN_FILE(HPROGRAM,IGLB,HFILE,'FORMATTED',HACTION='READ')
!

DO JI=1,NL
  DO JK=1,NOCKMAX
    READ(IGLB,*,END=99) ZLAT(JI),ZLON(JI), ZDEPTH(JK), & 
    ZFIELDR(JI,JK,1,1), ZFIELDR(JI,JK,2,1),ZFIELDR(JI,JK,3,1), & 
    ZFIELDR(JI,JK,4,1)
  END DO
END DO


 
!      3. Close the file

99 CONTINUE
CALL CLOSE_FILE (HPROGRAM,IGLB)

WRITE(KLUOUT,*) MINVAL(ZFIELDR), MAXVAL(ZFIELDR)

!
! Get the correct varaibles
SELECT CASE (HSURF)
  CASE('TEMP_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,1,1)
  CASE('SALT_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,2,1)
  CASE('UCUR_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,3,1)
  CASE('VCUR_OC') 
  ZFIELD(:,:,1) = ZFIELDR(:,:,4,1)
END SELECT



!*      3.     Interpolation method
!              --------------------
!
CINTERP_TYPE='NONE  '
!CINTERP_TYPE='HORIBL'
!

YMASK = 'SEA   '
 CALL GET_TYPE_DIM_n(DTCO, U, YMASK,IDIM)
WRITE(KLUOUT,*) "IDIM (dim sea) =", IDIM

ALLOCATE(PFIELD(1:IDIM,1:SIZE(ZFIELD,2),1:SIZE(ZFIELD,3)))

IF (IDIM/=SIZE(PFIELD,1)) THEN
   WRITE(KLUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PFIELD)
   CALL ABOR1_SFX('PGD_FIELD: WRONG DIMENSION OF MASK')
ENDIF

ALLOCATE(IMASK(IDIM))
ILU=0
 CALL GET_SURF_MASK_n(DTCO, U, YMASK,IDIM,IMASK,ILU,KLUOUT)
DO JK=1,NOCKMAX
  CALL PACK_SAME_RANK(IMASK,ZFIELD(:,JK,1),PFIELD(:,JK,1))
END DO
DEALLOCATE(IMASK)

!*      4.     Deallocations
!              -------------
!
IF (ALLOCATED(ZLON       ))  DEALLOCATE(ZLON  )
IF (ALLOCATED(ZLAT       ))  DEALLOCATE(ZLAT  )
IF (ALLOCATED(ZDEPTH      ))  DEALLOCATE(ZDEPTH )
IF (ALLOCATED(ZFIELD      ))  DEALLOCATE(ZFIELD )
IF (ALLOCATED(ZFIELDR     ))  DEALLOCATE(ZFIELDR )
!
DEALLOCATE(NNUM)
IF (NRANK/=NPIO) THEN
  DEALLOCATE(NINDEX,UG%XGRID_FULL_PAR)
  ALLOCATE(NINDEX(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_OCEAN_ASCLLV
