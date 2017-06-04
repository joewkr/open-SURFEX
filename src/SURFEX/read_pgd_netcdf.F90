!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#################################################################################
SUBROUTINE READ_PGD_NETCDF (UG, U, USS, &
                            HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD,PFIELD)
!#################################################################################
!
!!****  *READ_PGD_NETCDF* - read data from NETCDF files during PGD (altitude)
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
!!      Original    11/2012
!!------------------------------------------------------------------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_ABOR1_SFX

USE MODI_READ_AND_SEND_MPI
USE MODE_READ_CDF, ONLY :HANDLE_ERR_CDF
! USE MODD_PGD_GRID,       ONLY : NL ! grid dimension length
USE MODI_PT_BY_PT_TREATMENT
USE MODI_GET_LUOUT

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
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=20),   INTENT(IN)  :: HFIELD     ! name of variable
REAL,DIMENSION(:),INTENT(OUT),OPTIONAL :: PFIELD ! output a variable

REAL,DIMENSION(:),POINTER  :: ZLAT,ZLON
REAL,DIMENSION(:),POINTER  :: ZLAT2D,ZLON2D
REAL,DIMENSION(:),POINTER,SAVE    :: ZFIELD    ! field to read
!
REAL, DIMENSION(:), ALLOCATABLE :: ZFIELD0
!
!*      0.2    declarations of local variables
!
! CHARACTER(LEN=28) :: YNCVAR
!
INTEGER::IERROR !error status
INTEGER::ID_FILE ! id of netcdf file
INTEGER::INFIELD,INLAT,INLON ! dimension lengths
INTEGER::ILUOUT
INTEGER::JPOINT !loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_NETCDF',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
SELECT CASE (TRIM(HFIELD))
  CASE ('ZS','slope')
  CASE DEFAULT
    CALL ABOR1_SFX('READ_PGD_NETCDF: '//TRIM(HFIELD)//" initialization not implemented !")
END SELECT

!------------------------------------------------------------------------------------
!              ---------

!
!*      2.     Reading of field
!              ----------------

! Open netcdf file
IERROR=NF90_OPEN(HFILENAME,NF90_NOWRITE,ID_FILE)
CALL HANDLE_ERR_CDF(IERROR,"can't open file "//TRIM(HFILENAME))

CALL READ_FIELD_NETCDF(ID_FILE,'LAT                 ',ZLAT,INLAT)
CALL READ_FIELD_NETCDF(ID_FILE,'LON                 ',ZLON,INLON)
CALL READ_FIELD_NETCDF(ID_FILE,HFIELD,ZFIELD,INFIELD)

! Close netcdf file
IERROR=NF90_CLOSE(ID_FILE)

IF (PRESENT(PFIELD)) THEN

  ALLOCATE(ZFIELD0(U%NDIM_FULL))
  !
  DO JPOINT=1,INFIELD

! On pourrait faire un controle des coordonnées ?
!    IF ((ABS(ZLAT(JPOINT)-????XLAT???)<0.001)  .AND. (ABS(ZLON(JPOINT)-????XLON???)<0.001)) THEN

    ZFIELD0(JPOINT)=ZFIELD(JPOINT)

!    END IF
  END DO
  !
  CALL READ_AND_SEND_MPI(ZFIELD0,PFIELD)
  !
  DEALLOCATE(ZFIELD0)
  !
ELSE

  ALLOCATE(ZLAT2D(INFIELD))
  ALLOCATE(ZLON2D(INFIELD))

  IF (INLAT*INLON==INFIELD) THEN
    CALL ABOR1_SFX('READ_PGD_NETCDF: 1D LAT and LON not implemented')
  ELSEIF ((INLAT==INFIELD) .AND. (INFIELD==INLON)) THEN
    ZLAT2D(:)=ZLAT(:)
    ZLON2D(:)=ZLON(:)
  ELSE
    CALL ABOR1_SFX('READ_PGD_NETCDF: problem with dimensions lengths between LAT LON and FIELD')
  END IF

  DO JPOINT=1,INFIELD
    !*    5.     Call to the adequate subroutine (point by point treatment)
    !            ----------------------------------------------------------
    !     
    CALL PT_BY_PT_TREATMENT(UG, U, USS, &
                            ILUOUT,  (/ ZLAT2D(JPOINT)/) , (/ZLON2D(JPOINT)/) , (/ ZFIELD(JPOINT)/) , &
      HSUBROUTINE                                       )  

  ENDDO

  DEALLOCATE(ZLAT2D)
  DEALLOCATE(ZLON2D)

END IF

DEALLOCATE(ZLAT)
DEALLOCATE(ZLON)

DEALLOCATE(ZFIELD)

IF (LHOOK) CALL DR_HOOK('READ_PGD_NETCDF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------

CONTAINS

SUBROUTINE READ_FIELD_NETCDF(ID_FILE,HFIELD,PFIELD,ILENDIM)

USE MODE_READ_CDF, ONLY :HANDLE_ERR_CDF

USE NETCDF
!
IMPLICIT NONE


INTEGER,INTENT(IN)::ID_FILE
 CHARACTER(LEN=20),   INTENT(IN)  :: HFIELD     ! name of variable
REAL,DIMENSION(:),POINTER::PFIELD

INTEGER::ID_VAR ! Netcdf IDs for file and variable
INTEGER::INVARDIMS !number of dimensions of netcdf input variable
INTEGER,DIMENSION(:),ALLOCATABLE::IVARDIMSID
INTEGER::ILENDIM1,ILENDIM2
INTEGER,INTENT(OUT)::ILENDIM
INTEGER::IERROR !error status
INTEGER::ITYPE

! Look for variable ID for HFIELD
IERROR=NF90_INQ_VARID(ID_FILE,TRIM(HFIELD),ID_VAR)
CALL HANDLE_ERR_CDF(IERROR,"can't find variable "//TRIM(HFIELD))

! Number of dimensions
IERROR=NF90_INQUIRE_VARIABLE(ID_FILE,ID_VAR,NDIMS=INVARDIMS)
if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions number")

! Id of dimensions
ALLOCATE(IVARDIMSID(INVARDIMS))

IERROR=NF90_INQUIRE_VARIABLE(ID_FILE,ID_VAR,DIMIDS=IVARDIMSID)
if (IERROR/=NF90_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions ids")


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
    CALL ABOR1_SFX('READ_PGD_NETCDF: incorrect number of dimensions for variable '//TRIM(HFIELD))

END SELECT

DEALLOCATE(IVARDIMSID)

! IF(ILENDIM/=NL) CALL ABOR1_SFX('READ_PGD_NETCDF: incorrect number of points &
! &                             in netcdf file for variable '//TRIM(HFIELD))

ALLOCATE(PFIELD(ILENDIM))

IERROR=NF90_INQUIRE_VARIABLE(ID_FILE,ID_VAR,XTYPE=ITYPE)
IF (ITYPE/=NF90_DOUBLE) THEN
  CALL ABOR1_SFX('READ_PGD_NETCDF: incorrect type for variable '//TRIM(HFIELD))
END IF

! Read 1D variable
IERROR=NF90_GET_VAR(ID_FILE,ID_VAR,PFIELD)

CALL HANDLE_ERR_CDF(IERROR,"can't read variable "//TRIM(HFIELD))

END SUBROUTINE READ_FIELD_NETCDF


END SUBROUTINE READ_PGD_NETCDF
