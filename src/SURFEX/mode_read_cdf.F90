!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_CDF
!===================================================================
!
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE HANDLE_ERR_CDF(status,line)
!     ####################
USE NETCDF
!
IMPLICIT NONE
INTEGER, INTENT(IN)           :: status
 CHARACTER(*), INTENT(IN) :: line
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:HANDLE_ERR_CDF',0,ZHOOK_HANDLE)
IF (status /= NF90_NOERR) THEN
  CALL ABOR1_SFX('MODE_READ_NETCDF: HANDLE_ERR_CDF:'//TRIM(line))
END IF
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:HANDLE_ERR_CDF',1,ZHOOK_HANDLE)
END SUBROUTINE HANDLE_ERR_CDF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET1DCDF(KCDF_ID,IDVAR,PMISSVALUE,PVALU1D)
!     ####################
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: KCDF_ID !netcdf file identifiant
INTEGER,INTENT(IN) :: IDVAR   !variable to read identifiant
REAL, INTENT(OUT) ::  PMISSVALUE !undefined value
REAL,DIMENSION(:),INTENT(OUT) :: PVALU1D !value array
!
integer :: status
character(len=80) :: HACTION
integer,save :: NDIMS=1
integer :: KVARTYPE
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
integer :: JLOOP
integer :: NGATTS   
character(len=80),DIMENSION(:),ALLOCATABLE :: HNAME
REAL,DIMENSION(:),ALLOCATABLE :: ZVALU1D !value array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:GET1DCDF',0,ZHOOK_HANDLE)
PMISSVALUE=-9999.9
ALLOCATE(NVARDIMID (NDIMS))
ALLOCATE(NVARDIMLEN(NDIMS))
ALLOCATE(NVARDIMNAM(NDIMS))
NVARDIMID (:)=0
NVARDIMLEN(:)=0
NVARDIMNAM(:)=' '
!
HACTION='get variable type'
status=nf90_inquire_variable(KCDF_ID,IDVAR,XTYPE=KVARTYPE)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'variable type = ',KVARTYPE
!
HACTION='get variable dimensions identifiant'
status=nf90_inquire_variable(KCDF_ID,IDVAR,DIMIDS=NVARDIMID)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'variable dimension ',NDIMS,' identifiant ',NVARDIMID(NDIMS)
!
HACTION='get variable dimensions name'
status=nf90_inquire_dimension(KCDF_ID,NVARDIMID(NDIMS),NAME=NVARDIMNAM(NDIMS))
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!
HACTION='get variable dimensions length'
status=nf90_inquire_dimension(KCDF_ID,NVARDIMID(NDIMS),LEN=NVARDIMLEN(NDIMS))
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'variable dimension ',NDIMS,' named ',NVARDIMNAM(NDIMS),&
!     &'has a length of',NVARDIMLEN(NDIMS)
!
HACTION='get attributs'
!status=nf90_inq_natts(KCDF_ID,NGATTS)
status=nf90_inquire_variable(KCDF_ID,IDVAR,NATTS=NGATTS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'number of attributes = ',NGATTS
allocate(hname(1:NGATTS))
!
DO JLOOP=1,NGATTS
  status=nf90_inq_attname(KCDF_ID,IDVAR,JLOOP,hname(JLOOP))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  !write(0,*) 'attributes names = ', hname(JLOOP)
  if (TRIM(hname(JLOOP))=='missing_value') then
    !write(0,*) 'missing value search '
    HACTION='get missing value'
    status=nf90_get_att(KCDF_ID,IDVAR,"missing_value",PMISSVALUE)
    if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif
ENDDO
!
ALLOCATE(ZVALU1D(1:NVARDIMLEN(NDIMS)))
ZVALU1D=0.
!
IF (KVARTYPE>=5) then
  HACTION='get variable values (1D)'
  status=nf90_get_var(KCDF_ID,IDVAR,ZVALU1D(:))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
ENDIF
PVALU1D(:)=ZVALU1D(:)
IF (ALLOCATED(ZVALU1D  ))  DEALLOCATE(ZVALU1D)
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:GET1DCDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET1DCDF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET2DCDF(KCDF_ID,IDVAR,PDIM1,HDIM1NAME,PDIM2,HDIM2NAME,&
             PMISSVALUE,PVALU2D)  
!     ####################
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: KCDF_ID !netcdf file identifiant
INTEGER,INTENT(IN) :: IDVAR   !variable to read identifiant
REAL,DIMENSION(:),INTENT(OUT) :: PDIM1,PDIM2 !dimensions for PVALU2D array
 CHARACTER(len=80),INTENT(OUT) :: HDIM1NAME,HDIM2NAME     !dimensions names
REAL, INTENT(OUT) :: PMISSVALUE
REAL,DIMENSION(:,:),INTENT(OUT) :: PVALU2D !value array
!
integer :: status
character(len=80) :: HACTION
integer,save :: NDIMS=2
integer :: KVARTYPE
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
integer :: JLOOP
integer :: NGATTS   
character(len=80),DIMENSION(:),ALLOCATABLE :: HNAME
real :: ZMISS1,ZMISS2
REAL,DIMENSION(:,:),ALLOCATABLE :: ZVALU2D !value array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:GET2DCDF',0,ZHOOK_HANDLE)
PMISSVALUE=-9999.9 
ALLOCATE(NVARDIMID (NDIMS))
ALLOCATE(NVARDIMLEN(NDIMS))
ALLOCATE(NVARDIMNAM(NDIMS))
NVARDIMID (:)=0
NVARDIMLEN(:)=0
NVARDIMNAM(:)=' '
!
HACTION='get variable type'
status=nf90_inquire_variable(KCDF_ID,IDVAR,XTYPE=KVARTYPE)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'variable type = ',KVARTYPE
!
HACTION='get variable dimensions identifiant'
status=nf90_inquire_variable(KCDF_ID,IDVAR,DIMIDS=NVARDIMID)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!
HACTION='get attributs'
!status=nf90_inq_natts(KCDF_ID,NGATTS)
status=nf90_inquire_variable(KCDF_ID,IDVAR,NATTS=NGATTS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'number of attributes = ',NGATTS
allocate(hname(1:NGATTS))
!
DO JLOOP=1,NGATTS
  status=nf90_inq_attname(KCDF_ID,IDVAR,JLOOP,hname(JLOOP))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  !write(0,*) 'attributes names = ', hname(JLOOP)
  if (TRIM(hname(JLOOP))=='missing_value') then
    !write(0,*) 'missing value search '
    HACTION='get missing value'
    status=nf90_get_att(KCDF_ID,IDVAR,"missing_value",PMISSVALUE)
    if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
    !write(0,*) 'missing value = ',PMISSVALUE
  endif
ENDDO
!
!
DO JLOOP=1,NDIMS
  HACTION='get variable dimensions name'
  status=nf90_inquire_dimension(KCDF_ID,NVARDIMID(JLOOP),NAME=NVARDIMNAM(JLOOP))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  HACTION='get variable dimensions length'
  status=nf90_inquire_dimension(KCDF_ID,NVARDIMID(JLOOP),LEN=NVARDIMLEN(JLOOP))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  !write(0,*) 'variable dimension ',JLOOP,' named ',NVARDIMNAM(JLOOP),&
  !     &'has a length of',NVARDIMLEN(JLOOP)
ENDDO
! 
ALLOCATE(ZVALU2D(1:NVARDIMLEN(1),1:NVARDIMLEN(2)))
ZVALU2D=0.
IF (KVARTYPE>=5) then
  HACTION='get variable values (2D)'
  status=nf90_get_var(KCDF_ID,IDVAR,ZVALU2D(:,:))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
ENDIF
PVALU2D(:,:)=ZVALU2D(:,:)
!
 CALL GET1DCDF(KCDF_ID,NVARDIMID(1),ZMISS1,PDIM1)
 CALL GET1DCDF(KCDF_ID,NVARDIMID(2),ZMISS2,PDIM2)
HDIM1NAME=NVARDIMNAM(1)
HDIM2NAME=NVARDIMNAM(2)
IF (ALLOCATED(ZVALU2D  ))  DEALLOCATE(ZVALU2D)
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:GET2DCDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET2DCDF
!--------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
       SUBROUTINE READ_LATLONVAL_CDF(HFILENAME,HNCVARNAME,PLON,PLAT,PVAL)
!     ####################
!
USE NETCDF
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
REAL, DIMENSION(:), INTENT(OUT) :: PLON,PLAT ! Longitudes/latitudes innetcdf file 
REAL, DIMENSION(:), INTENT(OUT) :: PVAL      ! value to get
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer ::JLOOP1,JDIM1,JDIM2,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer ::NLEN
integer,dimension(1) :: IDIMID
integer,DIMENSION(1:2) :: NLEN2D,IDIMID2D
integer,DIMENSION(:),ALLOCATABLE :: NVARDIMID,NVARDIMLEN
character(len=80),DIMENSION(:),ALLOCATABLE :: NVARDIMNAM
real,DIMENSION(:),ALLOCATABLE   :: ZVALU
real,DIMENSION(:,:),ALLOCATABLE :: ZVALU2D
real :: ZMISS
real,DIMENSION(:),ALLOCATABLE :: ZDIM1
real,DIMENSION(:),ALLOCATABLE :: ZDIM2
character(len=80) :: YDIM1NAME,YDIM2NAME
integer :: ILONFOUND,ILATFOUND, IARG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    1.      Open the netcdf file 
!             --------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:READ_LATLONVAL_CDF',0,ZHOOK_HANDLE)
status=-9999
kcdf_id=-9999
HACTION='open netcdf'
status=NF90_OPEN(HFILENAME,nf90_nowrite,kcdf_id)
!write(0,*) 'status=',status
!write(0,*) 'identifiant de ',HFILENAME,'=',kcdf_id
if (status/=NF90_NOERR) then 
  CALL HANDLE_ERR_CDF(status,HACTION)
!else
!  write(0,*) 'netcdf file opened: ',HFILENAME
endif
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF90_INQUIRE(kcdf_id,NVARIABLES=NBVARS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF90_INQUIRE_VARIABLE(kcdf_id,JLOOP1,NAME=VARNAME(JLOOP1))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf90_close(kcdf_id)
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  CALL ABOR1_SFX('MODE_READ_NETCDF: READ_LATLONVAL_CDF')
endif
!-----------
!
!*    4.      get the variable in netcdf file 
!             -------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,NDIMS=NVARDIMS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!
!     4.2      get the variable dimensions length and values
!              ----------------------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D
  CASE (1) 
    !write(0,*) 'variable dimensions number = ',NVARDIMS
    HACTION='get variable dimensions length'
    status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,DIMIDS=IDIMID)
    status=nf90_inquire_dimension(kcdf_id,IDIMID(1),LEN=NLEN)
    if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
    ALLOCATE(ZVALU(NLEN))
    !write(0,*) 'call GET1DCDF'
    CALL GET1DCDF(kcdf_id,ID_VARTOGET,ZMISS,ZVALU)
    PVAL(:)=ZVALU(:)
!CAS 2D
  CASE (2)
    !write(0,*) 'variable dimensions number = ',NVARDIMS
    status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,DIMIDS=IDIMID2D)
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf90_inquire_dimension(kcdf_id,IDIMID2D(JLOOP),LEN=NLEN2D(JLOOP))
      if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
    ENDDO
    ALLOCATE(ZVALU2D(NLEN2D(1),NLEN2D(2)))
    ALLOCATE(ZDIM1(NLEN2D(1)))
    ALLOCATE(ZDIM2(NLEN2D(2)))
    !write(0,*) 'call GET2DCDF'
    CALL GET2DCDF(kcdf_id,ID_VARTOGET,ZDIM1,YDIM1NAME,ZDIM2,YDIM2NAME,&
           ZMISS,ZVALU2D)  
    !write(0,*) 'YDIM1NAME: ',YDIM1NAME
    !write(0,*) 'YDIM2NAME: ',YDIM2NAME
    if ((YDIM1NAME=='lon').OR.(YDIM1NAME=='longitude')) ILONFOUND=1
    if ((YDIM2NAME=='lon').OR.(YDIM2NAME=='longitude')) ILONFOUND=2
    if ((YDIM1NAME=='lat').OR.(YDIM1NAME=='latitude'))  ILATFOUND=1
    if ((YDIM2NAME=='lat').OR.(YDIM2NAME=='latitude'))  ILATFOUND=2
    IARG=0
!
!     4.3      complete arrays
!              ---------------
    IF ((ILONFOUND==1).AND.(ILATFOUND==2)) then
      !write(0,*) 'ILONFOUND',ILONFOUND,'ILATFOUND',ILATFOUND
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PVAL(IARG)=ZVALU2D(JDIM1,JDIM2)
          PLON(IARG)=ZDIM1(JDIM1)
          PLAT(IARG)=ZDIM2(JDIM2)
        ENDDO
      ENDDO
    ELSEIF ((ILONFOUND==2).AND.(ILATFOUND==1)) then
      !write(0,*) 'ILONFOUND',ILONFOUND,'ILATFOUND',ILATFOUND
      DO JDIM1=1,SIZE(ZDIM1)
        DO JDIM2=1,SIZE(ZDIM2)
          IARG=IARG+1
          PVAL(IARG)=ZVALU2D(JDIM1,JDIM2)
          PLAT(IARG)=ZDIM1(JDIM1)
          PLON(IARG)=ZDIM2(JDIM2)
        ENDDO
      ENDDO
    ELSE
      write(0,*) '*****WARNING*****: incompatible dimensions to lat/lon/value arrays'
    ENDIF
!
END SELECT
!
!
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
status=nf90_close(kcdf_id)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (ALLOCATED(ZVALU       ))  DEALLOCATE(ZVALU  )
IF (ALLOCATED(ZVALU2D     ))  DEALLOCATE(ZVALU2D)
IF (ALLOCATED(ZDIM1       ))  DEALLOCATE(ZDIM1  )
IF (ALLOCATED(ZDIM2       ))  DEALLOCATE(ZDIM2  )
!
IF (ALLOCATED(NVARDIMID   ))  DEALLOCATE(NVARDIMID )
IF (ALLOCATED(NVARDIMNAM  ))  DEALLOCATE(NVARDIMNAM)
IF (ALLOCATED(NVARDIMLEN  ))  DEALLOCATE(NVARDIMLEN)
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:READ_LATLONVAL_CDF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_LATLONVAL_CDF
!------------------------------------------------------------------------------
!==============================================================================
!     ####################
       SUBROUTINE READ_DIM_CDF(HFILENAME,HNCVARNAME,KDIM)
!     ####################
!
USE NETCDF
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME   ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME  ! Name of variable to read in netcdf file
INTEGER,           INTENT(OUT):: KDIM        ! value of dimension to get
!
integer :: status
integer :: kcdf_id
integer :: NBVARS
character(len=80) :: HACTION
character(len=80),DIMENSION(:),ALLOCATABLE :: VARNAME
integer ::JLOOP1,JLOOP
integer ::ID_VARTOGET,ID_VARTOGET1,ID_VARTOGET2
integer ::NVARDIMS
integer, dimension(1) :: NDIMID
integer,DIMENSION(2) ::NLEN2D, NDIMID2D
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    1.      Open the netcdf file 
!             --------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:READ_DIM_CDF',0,ZHOOK_HANDLE)
HACTION='open netcdf'
status=NF90_OPEN(HFILENAME,nf90_nowrite,kcdf_id)
if (status/=NF90_NOERR) then 
  CALL HANDLE_ERR_CDF(status,HACTION)
!else
endif
!
!-----------
!
!*    2.      get the number of variables in netcdf file 
!             ------------------------------------------
HACTION='get number of variables'
status=NF90_INQUIRE(kcdf_id,NVARIABLES=NBVARS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'nb vars', NBVARS
ALLOCATE(VARNAME(NBVARS))
!
!-----------
!
!*    3.      get the variables names in netcdf file 
!             --------------------------------------
ID_VARTOGET1=0
ID_VARTOGET2=0
DO JLOOP1=1,NBVARS
  HACTION='get variables  names'
  status=NF90_INQUIRE_VARIABLE(kcdf_id,JLOOP1,NAME=VARNAME(JLOOP1))
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  !write(0,*) 'var',JLOOP1,' name: ',VARNAME(JLOOP1)
  if (VARNAME(JLOOP1)==HNCVARNAME) then
    !write(0,*) 'var',JLOOP1,' corresponding to variable required'
    ID_VARTOGET1=JLOOP1
  endif
  if (VARNAME(JLOOP1)/=HNCVARNAME) then
    if((LGT(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))).AND.&
           (SCAN(TRIM(VARNAME(JLOOP1)),TRIM(HNCVARNAME))==1)) then  
      !write(0,*) 'var',JLOOP1,VARNAME(JLOOP1),' could correspond to variable required ?'
      !write(0,*) HNCVARNAME,' is variable required; only ',VARNAME(JLOOP1),' found'
      ID_VARTOGET2=JLOOP1
    endif
  endif
ENDDO
if (ID_VARTOGET1/=0) then
  ID_VARTOGET=ID_VARTOGET1
else
  ID_VARTOGET=ID_VARTOGET2
endif
if (ID_VARTOGET==0) then
  HACTION='close netcdf'
  status=nf90_close(kcdf_id)
  if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
  CALL ABOR1_SFX('MODE_READ_CDF: READ_DIM_CDF')
endif
!-----------
!
!*    4.      get the total dimension of HNCVARNAME 
!             -------------------------------------
!
!     4.1      get the variable dimensions number
!             -----------------------------------
!
HACTION='get variable dimensions number'
status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,ndims=NVARDIMS)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'variable dimensions number = ',NVARDIMS
!
!     4.2      get the variable dimensions length
!              ----------------------------------
SELECT CASE (NVARDIMS)
!CAS 1D
  CASE (1) 
    HACTION='get variable dimensions length'
    status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,dimids=NDIMID)
    status=nf90_inquire_dimension(kcdf_id,NDIMID(1),LEN=KDIM)
    if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!
!CAS 2D
  CASE (2)
    KDIM=1
    status=nf90_inquire_variable(kcdf_id,ID_VARTOGET,dimids=NDIMID2D)
    DO JLOOP=1,NVARDIMS
      HACTION='get variable dimensions length'
      status=nf90_inquire_dimension(kcdf_id,NDIMID2D(JLOOP),LEN=NLEN2D(JLOOP))
      if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
      KDIM=KDIM*NLEN2D(JLOOP)
    ENDDO
END SELECT
!-----------
!*    10.     Close the netcdf file 
!             ---------------------
HACTION='close netcdf'
status=nf90_close(kcdf_id)
if (status/=NF90_NOERR) CALL HANDLE_ERR_CDF(status,HACTION)
!write(0,*) 'OK: netcdf file closed: ',HFILENAME
!
!-----------
!*    11.     Deallocate 
!             ----------
IF (ALLOCATED(VARNAME     ))  DEALLOCATE(VARNAME)
IF (LHOOK) CALL DR_HOOK('MODE_READ_CDF:READ_DIM_CDF',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_DIM_CDF
!------------------------------------------------------------------------------
!==============================================================================
END MODULE MODE_READ_CDF
