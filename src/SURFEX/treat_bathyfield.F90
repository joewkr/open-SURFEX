!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TREAT_BATHYFIELD (UG, U, USS, &
                                   HPROGRAM,HSCHEME,HFILETYPE,    &
                              HSUBROUTINE,HFILENAME,HNCVARNAME,   &
                              HFIELD, PPGDARRAY,HSFTYPE               )  
!     ##############################################################
!
!!**** *TREAT_BATHYFIELD* chooses which treatment subroutine to use to read 
!!                        the bathymetry
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    C. Lebeaupin Brossier        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!    
!!    Original    01/2008
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PGDWORK, ONLY : NSIZE, NSIZE_ALL, XALL, XSUMVAL
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC, NCOMM, NREQ, NINDEX, IDX_R, &
                                NSIZE_TASK,NREQ, NSIZE_max=>NSIZE
!
USE MODI_INI_SSOWORK
USE MODI_GET_LUOUT
USE MODI_READ_DIRECT
USE MODI_READ_BINLLV
USE MODI_READ_BINLLVFAST
USE MODI_READ_ASCLLV
USE MODI_READ_NETCDF
USE MODI_AVERAGE2_MESH
USE MODI_READ_AND_SEND_MPI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_AVERAGE2_COVER
!
USE MODI_AVERAGE2_OROGRAPHY
!
USE MODI_READ_DIRECT_GAUSS
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HFILETYPE     ! Type of the data file
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME    ! Name of the variable in netcdf file
 CHARACTER(LEN=20), INTENT(IN) :: HFIELD        ! Name of the field.
REAL, DIMENSION(:), INTENT(INOUT), OPTIONAL :: PPGDARRAY ! field on MESONH grid
 CHARACTER(LEN=3),   INTENT(IN),    OPTIONAL :: HSFTYPE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZPGDARRAY
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
REAL, DIMENSION(:,:), ALLOCATABLE :: ZVAL
INTEGER, DIMENSION(:), ALLOCATABLE :: ISIZE
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
INTEGER, DIMENSION(MPI_STATUS_SIZE,NPROC-1) :: ISTATUS2
#endif
INTEGER :: ILUOUT, INFOMPI, JP, ICPT, JI, JL, IREQ, IDX,&
                        IDX_SAVE
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_BATHYFIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.     Selection of type of reading (and point by point treatment)
!            -----------------------------------------------------------
!
SELECT CASE (HFILETYPE)

   CASE ('DIRECT')
         IF(UG%G%CGRID=="GAUSS     ")THEN
            CALL READ_DIRECT_GAUSS(UG, U, USS, &
                                   HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)
         ELSE
            CALL READ_DIRECT(UG, U, USS, &
                             HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)
         ENDIF
   CASE ('BINLLV')
       CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_BINLLV(UG, U, USS, &
                        HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('BINLLF')
       CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_BINLLVFAST(UG, U, USS, &
                            HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('ASCLLV')
       CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_ASCLLV(UG, U, USS, &
                        HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('NETCDF')
       CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_NETCDF(UG, U, USS, &
                        HPROGRAM,HSUBROUTINE,HFILENAME,HNCVARNAME)

END SELECT
!
!-------------------------------------------------------------------------------
!
!nsize contains the number of points found for each of the domain, for each task
ALLOCATE(NSIZE(U%NSIZE_FULL,1))
!
IF (NPROC>1) THEN
  !
  ALLOCATE(ISIZE(NSIZE_max))
  !
  IDX_SAVE = IDX_R
  IDX = IDX_SAVE + NRANK
  !each task sends to each other task the part of NSIZE_ALL it got, stored in
  !isize
  CALL READ_AND_SEND_MPI(NSIZE_ALL(:,1),ISIZE(1:NSIZE_TASK(NRANK)),KPIO=NRANK,KDX=IDX)
  !
  NSIZE(:,1) = 0
  !for each task
  DO JP=0,NPROC-1
   !
    IF (JP/=NRANK) THEN
      !
#ifdef SFX_MPI
      !each task receives each ISIZE from each task
      CALL MPI_RECV(ISIZE,NSIZE_max*KIND(ISIZE)/4,MPI_INTEGER,&
                JP,IDX_SAVE+1+JP,NCOMM,ISTATUS,INFOMPI)
#endif
      !
    ELSE
      !
      ICPT = 0
      DO JI = 1,SIZE(NINDEX)
        IF (NINDEX(JI)==JP) THEN
          ICPT = ICPT + 1
          ISIZE(ICPT) = NSIZE_ALL(JI,1)
        ENDIF
      ENDDO
      !
    ENDIF
    !
    !nsize is the sum of all parts isize
    NSIZE(:,1) = NSIZE(:,1) + ISIZE(1:NSIZE_TASK(NRANK))
    !
  ENDDO
  DEALLOCATE(ISIZE)
#ifdef SFX_MPI  
  CALL MPI_WAITALL(NPROC-1,NREQ(1:NPROC-1),ISTATUS2,INFOMPI)
#endif  
ELSE
  NSIZE(:,1) = NSIZE_ALL(:,1)
ENDIF
!
!
DEALLOCATE(NSIZE_ALL)
!
!
SELECT CASE (HSUBROUTINE)

  CASE ('A_MESH')
    !most simple case
    ALLOCATE(XSUMVAL(U%NSIZE_FULL,1))
    IF (NPROC>1) THEN
      XSUMVAL(:,:) = 0.
      ALLOCATE(ZVAL(U%NSIZE_FULL,1))
      DO JP = 0,NPROC-1
        CALL READ_AND_SEND_MPI(XALL(:,:,1),ZVAL,KPIO=JP)
        XSUMVAL(:,:) = XSUMVAL(:,:) + ZVAL(:,:)
      ENDDO
      DEALLOCATE(ZVAL)
    ELSE
      XSUMVAL(:,:) = XALL(:,:,1)
    ENDIF
    DEALLOCATE(XALL)
    !
END SELECT
!
!*    2.     Call to the adequate subroutine (global treatment)
!            --------------------------------------------------
!
SELECT CASE (HSUBROUTINE)

  CASE ('A_MESH')
    IF (.NOT. PRESENT(PPGDARRAY)) THEN
      WRITE(ILUOUT,*) 'You asked to average a PGD field with A_MESH option,'
      WRITE(ILUOUT,*) 'but you did not give the array to store this field'
      CALL ABOR1_SFX('TREAT_BATHYFIELD: PGD ARRAY IS MISSING')
    END IF
    ALLOCATE(ZPGDARRAY(SIZE(PPGDARRAY),1))    
    CALL AVERAGE2_MESH(ZPGDARRAY)
    PPGDARRAY = ZPGDARRAY(:,1)
    DEALLOCATE(ZPGDARRAY)

END SELECT
IF (LHOOK) CALL DR_HOOK('TREAT_BATHYFIELD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_BATHYFIELD
