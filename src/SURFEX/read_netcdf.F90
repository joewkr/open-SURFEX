!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NETCDF (UG, U, USS, &
                              HPROGRAM,HSUBROUTINE,HFILENAME,HNCVARNAME)
!     ##############################################################
!
!!**** *READ_NETCDF* reads a netcdf file and copy lat/lon/val then call treatment 
!!                   subroutine
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    C. Lebeaupin Brossier Meteo-France
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
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGD_GRID,   ONLY : LLATLONMASK
!
USE MODI_PT_BY_PT_TREATMENT
USE MODE_READ_CDF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
IMPLICIT NONE
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
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME    ! Name of the variable in netcdf file
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER      :: JLAT, JLON                 ! indexes of OLATLONMASK array
REAL         :: ZVALUE                     ! values of a data point
REAL         :: ZLAT                       ! latitude of data point
REAL         :: ZLON                       ! longitude of data point
!
REAL, DIMENSION(:),ALLOCATABLE :: ZVALU    ! array of values extract from netcdf file
REAL, DIMENSION(:),ALLOCATABLE :: ZLONG    ! array of values extract from netcdf file
REAL, DIMENSION(:),ALLOCATABLE :: ZLATI    ! array of values extract from netcdf file
!
INTEGER      :: ILUOUT                     ! output listing
INTEGER      :: JLOOP                      ! loop indice
INTEGER      :: JDIMENSION                 ! dimensions of ZVALU,ZLAT, 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
                                           ! and ZLON arrays
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NETCDF',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!
!*    1.      Read the netcdf file and lat/lon/val arrays dimensions
!             ------------------------------------------------------
 CALL READ_DIM_CDF(HFILENAME,HNCVARNAME,JDIMENSION)
ALLOCATE(ZVALU(JDIMENSION))
ALLOCATE(ZLATI(JDIMENSION))
ALLOCATE(ZLONG(JDIMENSION))

!*    1.      Read the netcdf file and extract lat/lon/val
!             --------------------------------------------
 CALL READ_LATLONVAL_CDF(HFILENAME,HNCVARNAME,ZLONG(:),ZLATI(:),ZVALU(:))
!
!----------------------------------------------------------------------------
!
!*    4.     Test if point is in the domain
!            ------------------------------
!
DO JLOOP=1,SIZE(ZVALU)
! 
  ZLON  = ZLONG(JLOOP)
  ZLAT  = ZLATI(JLOOP)
  ZVALUE= ZVALU(JLOOP)
! 
  ZLON=ZLON+NINT((180.-ZLON)/360.)*360.
  !
  JLAT = 1 + INT( ( ZLAT + 90. ) * 2. )
  JLAT = MIN(JLAT,360)
  JLON = 1 + INT( ( ZLON       ) * 2. )
  JLON = MIN(JLON,720)
  !
  IF (.NOT. LLATLONMASK(JLON,JLAT)) CYCLE
!
!-------------------------------------------------------------------------------
!
!*    5.     Call to the adequate subroutine (point by point treatment)
!            ----------------------------------------------------------
! 
  CALL PT_BY_PT_TREATMENT(UG, U, USS, &
                          ILUOUT,  (/ ZLAT /) , (/ ZLON /) , (/ ZVALUE /) , &
                            HSUBROUTINE                                       )  
!
!-------------------------------------------------------------------------------
ENDDO
IF (ALLOCATED(ZVALU       ))  DEALLOCATE(ZVALU  )
IF (ALLOCATED(ZLONG       ))  DEALLOCATE(ZLONG  )
IF (ALLOCATED(ZLATI       ))  DEALLOCATE(ZLATI  )
IF (LHOOK) CALL DR_HOOK('READ_NETCDF',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NETCDF
