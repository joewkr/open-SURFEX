!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_NETCDF (DTCO, U, HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_NETCDF* - prepares ISBA fields from initialization files in NETCDF
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     M. Lafaysse
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!      J.Escobar   11/2013  Add USE MODI_GET_TYPE_DIM_n
!!------------------------------------------------------------------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PREP,           ONLY : CINTERP_TYPE
!
USE MODI_ABOR1_SFX
USE MODI_GET_TYPE_DIM_n
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
!
USE MODE_READ_CDF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE

!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE :: ZNATURE
REAL, DIMENSION(:), ALLOCATABLE :: ZFIELD, ZFIELD0   ! field read

! CHARACTER(LEN=28) :: YNCVAR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JI, ICPT
INTEGER::IERROR !error status
INTEGER::JJ,JK,JLOOP ! loop counters
INTEGER::INLAYERS ! vertical dimension length
INTEGER::IL ! nature dimension length
INTEGER::ID_FILE,ID_VAR ! Netcdf IDs for file and variable
INTEGER::INVARDIMS !number of dimensions of netcdf input variable
INTEGER,DIMENSION(:),ALLOCATABLE::IVARDIMSID
INTEGER::ILENDIM,ILENDIM1,ILENDIM2

SELECT CASE (TRIM(HSURF))
  CASE ('TG','WG','WGI')
    INLAYERS=3 ! 3 soil layers for initialization
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_NETCDF: '//TRIM(HSURF)//" initialization not implemented !")
END SELECT
!
INLAYERS=3
!
!------------------------------------------------------------------------------------
!              ---------
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_NETCDF',0,ZHOOK_HANDLE)

!*      1.    get nature dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, 'NATURE',IL)
!
!*      2.     Reading of field
!              ----------------
!
IF (NRANK==NPIO) THEN
  ! Open netcdf file
  IERROR=NF90_OPEN(HFILE,NF90_NOWRITE,ID_FILE)
   CALL HANDLE_ERR_CDF(IERROR,"can't open file "//TRIM(HFILE))
 
  ! Look for variable ID
  IERROR=NF90_INQ_VARID(ID_FILE,TRIM(HSURF),ID_VAR)
   CALL HANDLE_ERR_CDF(IERROR,"can't find variable "//TRIM(HSURF))

  ! Number of dimensions
  IERROR=NF90_INQUIRE_VARIABLE(ID_FILE,ID_VAR,NDIMS=INVARDIMS)
  if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions number")
  ! Id of dimensions
  ALLOCATE(IVARDIMSID(INVARDIMS))
  IERROR=NF90_INQUIRE_VARIABLE(ID_FILE,ID_VAR,DIMIDS=IVARDIMSID)
  if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions ids")

  ALLOCATE(ZFIELD(U%NDIM_NATURE))

  SELECT CASE (INVARDIMS)
    CASE (1)
      ! Check dimension length
      IERROR=NF90_INQUIRE_DIMENSION(ID_FILE,IVARDIMSID(1),LEN=ILENDIM)
      if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
    CASE (2)
      IERROR=NF90_INQUIRE_DIMENSION(ID_FILE,IVARDIMSID(1),LEN=ILENDIM1)
      if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
      IERROR=NF90_INQUIRE_DIMENSION(ID_FILE,IVARDIMSID(2),LEN=ILENDIM2)
      if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
  
      ILENDIM=ILENDIM1*ILENDIM2
  
    CASE DEFAULT
      CALL ABOR1_SFX('PREP_ISBA_NETCDF: incorrect number of dimensions for variable '//TRIM(HSURF))
  
  END SELECT
  !
  IF(ILENDIM/=U%NDIM_NATURE) CALL ABOR1_SFX('PREP_ISBA_NETCDF: incorrect number of points '// &
                                  'in netcdf file for variable '//TRIM(HSURF))
  !
  ! Read 1D variable
  IERROR=NF90_GET_VAR(ID_FILE,ID_VAR,ZFIELD)
   CALL HANDLE_ERR_CDF(IERROR,"can't read variable "//TRIM(HSURF))
  !
  ! Close netcdf file
  IERROR=NF90_CLOSE(ID_FILE)
  !
ELSE
  ALLOCATE(ZFIELD(0))
ENDIF
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZNATURE(U%NDIM_FULL))
ELSE
  ALLOCATE(ZNATURE(0))
ENDIF
CALL GATHER_AND_WRITE_MPI(U%XNATURE,ZNATURE)
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZFIELD0(U%NDIM_FULL))
  ZFIELD0(:) = XUNDEF
  ICPT = 0
  DO JI=1,U%NDIM_FULL
    IF (ZNATURE(JI)/=0.) THEN
      ICPT = ICPT + 1
      ZFIELD0(JI) = ZFIELD(ICPT)
    ENDIF
  ENDDO
ELSE
  ALLOCATE(ZFIELD0(0))
ENDIF
!
DEALLOCATE(ZNATURE,ZFIELD)
ALLOCATE(PFIELD(IL,INLAYERS,1)) !will be deallocated later by prep_hor_isba_field
!
CALL READ_AND_SEND_MPI(ZFIELD0,PFIELD(:,1,1),U%NR_NATURE)
!
! For now initial values are identical for all tiles / soil layers.
DO JJ=2,INLAYERS
  PFIELD(:,JJ,1)=PFIELD(:,1,1)
END DO
!
DEALLOCATE(ZFIELD0)
!
!Interpolation method
CINTERP_TYPE='NONE'
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_NETCDF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_NETCDF
