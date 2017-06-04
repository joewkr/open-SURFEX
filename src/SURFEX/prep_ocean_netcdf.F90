!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_OCEAN_NETCDF(HPROGRAM,HSURF,HFILE,HFILETYPE,&
                              KLUOUT,HNCVARNAME,PFIELD)  
!     #################################################################################
!
!!****  *PREP_OCEAN_NETCDF* - prepares oceanic fields from Mercator analysis
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
!!     C. Lebeaupin Brossier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!------------------------------------------------------------------
!
USE MODD_PREP,       ONLY : CINTERP_TYPE, CINGRID_TYPE
USE MODD_GRID_LATLONREGUL, ONLY : NILENGTH, NINDEPTH, XILONARRAY
!
USE MODE_READ_NETCDF_MERCATOR
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=28),  INTENT(IN), OPTIONAL   :: HNCVARNAME!var to read 
REAL, POINTER, DIMENSION(:,:,:)   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
REAL,DIMENSION(:), ALLOCATABLE :: ZLATI
REAL,DIMENSION(:), ALLOCATABLE :: ZLONG
REAL,DIMENSION(:), ALLOCATABLE :: ZDEPTH
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------------
!*      1.     Grid type
!              ---------
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_NETCDF',0,ZHOOK_HANDLE)
CINGRID_TYPE='LATLON'
!
!
IF (.NOT. ALLOCATED(XILONARRAY)) CALL PREP_NETCDF_GRID(HFILE,HNCVARNAME)
!
ALLOCATE(ZLATI(NILENGTH) )
ALLOCATE(ZLONG(NILENGTH) )
ALLOCATE(ZDEPTH(NINDEPTH))
!
ALLOCATE(ZFIELD(NILENGTH,NINDEPTH,1))
!
!*      2.     Reading of field
!              ----------------
 CALL READ_LATLONDEPVAL_CDF(HFILE,HNCVARNAME,ZLONG,ZLATI,ZDEPTH,ZFIELD(:,:,1))
ALLOCATE(PFIELD(1:SIZE(ZFIELD,1),1:SIZE(ZFIELD,2),1:SIZE(ZFIELD,3)))
PFIELD=ZFIELD
!
!*      3.     Interpolation method
!              --------------------
!
CINTERP_TYPE='HORIBL'
!
!*      4.     Deallocations
!              -------------
!
IF (ALLOCATED(ZLONG       ))  DEALLOCATE(ZLONG  )
IF (ALLOCATED(ZLATI       ))  DEALLOCATE(ZLATI  )
IF (ALLOCATED(ZDEPTH      ))  DEALLOCATE(ZDEPTH )
IF (ALLOCATED(ZFIELD      ))  DEALLOCATE(ZFIELD )
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_NETCDF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_OCEAN_NETCDF
