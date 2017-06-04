!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_BUFFER
!     #####################
!-------------------------------------------------------------------
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE READ_BUFFER_LAND_MASK(KLUOUT,HINMODEL,PMASK)
!     ####################
!
USE MODD_GRID_BUFFER, ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! originating model
REAL, DIMENSION(:), POINTER       :: PMASK     ! Land mask
!
INTEGER                           :: IRET      ! return code
REAL, DIMENSION(:), POINTER       :: ZFIELD    ! field read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_LAND_MASK',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading land mask'
SELECT CASE (HINMODEL)
CASE ('ALADIN')
ALLOCATE (ZFIELD(NNI))
    CALL READ_BUFFER('LSM   ',ZFIELD,IRET)
END SELECT
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: LAND SEA MASK MISSING')
END IF
!
ALLOCATE (PMASK(NNI))
WHERE (ZFIELD>0.5)
  PMASK = 1.
ELSEWHERE
  PMASK = 0.
END WHERE
DEALLOCATE (ZFIELD)
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_LAND_MASK',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFFER_LAND_MASK
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_BUFFER_ZS_LAND(KLUOUT,HINMODEL,PFIELD)
!     ############################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XG
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read orography
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS_LAND',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading land orography'
SELECT CASE (HINMODEL)
CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('LPHIS ',PFIELD,IRET)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: LAND OROGRAPHY MISSING')
END IF
!
! Datas given in archives are multiplied by the gravity acceleration
PFIELD = PFIELD / XG
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS_LAND',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE READ_BUFFER_ZS_LAND
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_BUFFER_ZS_SEA(KLUOUT,HINMODEL,PFIELD)
!     ############################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XG
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Buffer originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read orography
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS_SEA',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading sea orography in buffer'
SELECT CASE (HINMODEL)
CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('SPHIS ',PFIELD,IRET)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: SEA OROGRAPHY MISSING')
END IF
!
! Datas given in archives are multiplied by the gravity acceleration
PFIELD = PFIELD / XG
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS_SEA',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE READ_BUFFER_ZS_SEA
!
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_BUFFER_ZS(KLUOUT,HINMODEL,PFIELD)
!     ############################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XG
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read orography
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading orography'
SELECT CASE (HINMODEL)
CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('PHIS  ',PFIELD,IRET)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: OROGRAPHY MISSING')
END IF
!
! Datas given in archives are multiplied by the gravity acceleration
PFIELD = PFIELD / XG
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_ZS',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFFER_ZS
!
!     ###########################
      SUBROUTINE READ_BUFFER_TS(KLUOUT,HINMODEL,PFIELD)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read surface temperature
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TS',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading surface temperature'
!
SELECT CASE (HINMODEL)
CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('TG1   ',PFIELD,IRET)
END SELECT
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: SURFACE TEMPERATURE MISSING')
END IF
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TS',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFFER_TS
!
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_BUFFER_SST(KLUOUT,HINMODEL,PFIELD)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read surface temperature
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SST',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading sea surface temperature'
!
SELECT CASE (HINMODEL)
CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('SST   ',PFIELD,IRET)
!
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: SEA SURFACE TEMPERATURE MISSING')
END IF
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SST',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFFER_SST
!
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_BUFFER_T2(KLUOUT,HINMODEL,PFIELD)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_BUFFER,  ONLY : NNI
!
USE MODI_READ_BUFFER
!
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PFIELD    ! 
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!
!* Read surface temperature
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_T2',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' | Reading deep soil temperature'
!
SELECT CASE (HINMODEL)

CASE ('ALADIN')
   ALLOCATE (PFIELD(NNI))
   CALL READ_BUFFER('TG2   ',PFIELD,IRET)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: DEEP SOIL TEMPERATURE MISSING')
END IF
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_T2',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE READ_BUFFER_T2
!
!     #######################
      SUBROUTINE READ_BUFFER_WG(KLUOUT,HINMODEL,PFIELD,PD)
!     #######################
!
! This tasks is divided in the following steps :
!  - computing the MesoNH constants
!  - reading the grib datas according to the type of file (ECMWF/Arpege/Aladin)
!  - converting from specific humidity to relative humidity
!  - interpolation with land mask
!  - converting back from relative humidity to specific humidity with MesoNH constants
! Five different models are supported :
!  - ECMWF with 2 layers (untested)
!  - ECMWF with 3 layers (archive before 1991 - Blondin model)
!  - ECMWF with 4 layers (archive after 1991 - Viterbo model)
!  - Arpege/Aladin before ISBA (I don't know the name of this model)
!  - Arpege/Aladin with ISBA model
! The available model is detect according to the fields which are presents :
!  - ECMWF archive : loads as many layers as possible
!  - Arpege/Aladin archive : ISBA model needs Clay and Sans fraction fields, if they
!    are present, they are used and the model is declared to be ISBA.
! To detect the height of the layers, two methods are used :
!  - if level type is not 112, a default value is assumed and a warning message is
!    displayed
!  - if level type is ID 112, then the position of the top and bottom surface may be
!    given. If they are present, they are used, if not the default value is taken and
!    a warning message is issued.
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!
!* local variables
!  ---------------
!
LOGICAL                           :: GISBA     ! T: surface scheme in file is ISBA
INTEGER                           :: IRET      ! return code
REAL,    DIMENSION(:), POINTER    :: ZFIELD    ! field to read
REAL,  DIMENSION(:,:), ALLOCATABLE:: ZWG       ! profile of soil water contents
REAL,  DIMENSION(:),   ALLOCATABLE:: ZCLAY     ! clay fraction
REAL,  DIMENSION(:),   ALLOCATABLE:: ZSAND     ! sand fraction
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWWILT     ! wilting point
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWFC       ! field capacity
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWSAT      ! saturation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 1.  Search and read clay fraction if available
!     ------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_WG',0,ZHOOK_HANDLE)
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('CLAY  ',ZFIELD,IRET)
!
! if not available, the model is not ISBA
IF (IRET /= 0) THEN
  GISBA = .FALSE.
ELSE
  GISBA = .TRUE.
  WRITE (KLUOUT,'(A)') ' | The soil model is ISBA'
  ALLOCATE (ZCLAY(NNI))
  ZCLAY(:) = ZFIELD(:) / 100. ! this field is given in percent
  DEALLOCATE (ZFIELD)
END IF
!
!-------------------------------------------------------------------------------
!
! 2.  Search and read sand fraction if available
!     ------------------------------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('SAND  ',ZFIELD,IRET)
!
! if not available, the model is not ISBA (IWMODE=1)
IF (GISBA) THEN
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_BUFFER: (WG) SAND FRACTION MISSING')
  ELSE
    ALLOCATE (ZSAND(NNI))
    ZSAND(:) = ZFIELD(:) / 100. ! this field is given in percent
    DEALLOCATE (ZFIELD)
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
! 3.  Read layer 1 moisture
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('WG1   ',ZFIELD,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: SOIL MOISTURE LEVEL 1 MISSING')
END IF
!
ALLOCATE(ZWG(NNI,2))
ZWG(:,1) = ZFIELD(:)
DEALLOCATE(ZFIELD)
!
!-------------------------------------------------------------------------------
!
! 4.  Read layer 2 moisture
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('WG2   ',ZFIELD,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_BUFFER: SOIL MOISTURE LEVEL 2 MISSING')
END IF
!
ZWG(:,2) = ZFIELD(:)
DEALLOCATE(ZFIELD)
!
!-------------------------------------------------------------------------------
!
! 5.  Read layer 2 depth (ISBA only)
!     ------------------
!
ALLOCATE(PD(NNI,3))
!
!* note that soil water reservoir is considered uniform between 0.2m and BUFFER soil depth
IF (GISBA) THEN
  ALLOCATE (ZFIELD(NNI))
  CALL READ_BUFFER('D2    ',ZFIELD,IRET)
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_BUFFER: LEVEL 2 DEPTH MISSING')
  END IF
  PD(:,1) = 0.01
  PD(:,2) = 0.20
  PD(:,3) = ZFIELD(:)
  !
  !* updates Wg in m3/m3
  !
  ZWG(:,1) = ZWG(:,1) / 10.
  ZWG(:,2) = ZWG(:,2) /(1000. * ZFIELD(:))
  DEALLOCATE(ZFIELD)
ELSE
  PD(:,1) = 0.01
  PD(:,2) = 0.2
  PD(:,3) = 2.0
END IF
!
!
!-------------------------------------------------------------------------------
!
! 6.  Compute relative humidity from units kg/m^2
!     -------------------------------------------
!
ALLOCATE(PFIELD(NNI,3))
!
! Compute ISBA model constants (if needed)
!
IF (GISBA) THEN
  ALLOCATE (ZWFC  (NNI))
  ALLOCATE (ZWWILT(NNI))
  ALLOCATE (ZWSAT (NNI))
  !
  ZWSAT (:) = (-1.08*100. * ZSAND(:) + 494.305) * 0.001
  ZWWILT(:) = 37.1342E-3 * SQRT( 100. * ZCLAY(:) )
  ZWFC  (:) = 89.0467E-3 * (100. * ZCLAY(:) )**0.3496
  !
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)

  ZWG(:,1) = MAX(MIN(ZWG(:,1),ZWSAT),0.)
  ZWG(:,2) = MAX(MIN(ZWG(:,2),ZWSAT),0.)
  !
  PFIELD(:,1) = (ZWG(:,1) - ZWWILT) / (ZWFC - ZWWILT)
  PFIELD(:,2) = (ZWG(:,2) - ZWWILT) / (ZWFC - ZWWILT)
  PFIELD(:,3) = PFIELD(:,2)
  DEALLOCATE (ZWSAT)
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
  !
ELSE ! Non ISBA
  PFIELD(:,1) =  ZWG(:,1)           /  20.
  PFIELD(:,2) = (ZWG(:,1)+ZWG(:,2)) / (20. + 100.)
  PFIELD(:,3) = PFIELD(:,2)
END IF
!
DEALLOCATE(ZWG)
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_WG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_WG
!
!-------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_WGI(KLUOUT,HINMODEL,PFIELD,PD)
!     #######################
!
! This tasks is divided in the following steps :
!  - computing the MesoNH constants
!  - reading the grib datas according to the type of file (ECMWF/Arpege/Aladin)
!  - converting from specific humidity to relative humidity
!  - interpolation with land mask
!  - converting back from relative humidity to specific humidity with MesoNH constants
! Five different models are supported :
!  - ECMWF with 2 layers (untested)
!  - ECMWF with 3 layers (archive before 1991 - Blondin model)
!  - ECMWF with 4 layers (archive after 1991 - Viterbo model)
!  - Arpege/Aladin before ISBA (I don't know the name of this model)
!  - Arpege/Aladin with ISBA model
! The available model is detect according to the fields which are presents :
!  - ECMWF archive : loads as many layers as possible
!  - Arpege/Aladin archive : ISBA model needs Clay and Sans fraction fields, if they
!    are present, they are used and the model is declared to be ISBA.
! To detect the height of the layers, two methods are used :
!  - if level type is not 112, a default value is assumed and a warning message is
!    displayed
!  - if level type is ID 112, then the position of the top and bottom surface may be
!    given. If they are present, they are used, if not the default value is taken and
!    a warning message is issued.
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!
!* local variables
!  ---------------
!
LOGICAL                           :: GISBA     ! T: surface scheme in file is ISBA
INTEGER                           :: IRET      ! return code
REAL,    DIMENSION(:), POINTER    :: ZFIELD    ! field to read
REAL,  DIMENSION(:,:), ALLOCATABLE:: ZWGI      ! profile of soil ice contents
REAL,  DIMENSION(:),   ALLOCATABLE:: ZCLAY     ! clay fraction
REAL,  DIMENSION(:),   ALLOCATABLE:: ZSAND     ! sand fraction
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWWILT     ! wilting point
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWFC       ! field capacity
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWSAT      ! saturation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 1.  Search and read clay fraction if available
!     ------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_WGI',0,ZHOOK_HANDLE)
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('CLAY  ',ZFIELD,IRET)
!
! if not available, the model is not ISBA (IWMODE=1)
IF (IRET /= 0) THEN
  GISBA = .FALSE.
ELSE
  GISBA = .TRUE.
  WRITE (KLUOUT,'(A)') ' | The soil model is ISBA'
  ALLOCATE (ZCLAY(NNI))
  ZCLAY(:) = ZFIELD(:) / 100. ! this field is given in percent
  DEALLOCATE (ZFIELD)
END IF
!
!-------------------------------------------------------------------------------
!
! 2.  Search and read sand fraction if available
!     ------------------------------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('SAND  ',ZFIELD,IRET)
!
! if not available, the model is not ISBA (IWMODE=1)
IF (GISBA) THEN
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_BUFFER: (WGI) SAND FRACTION MISSING')
  ELSE
    ALLOCATE (ZSAND(NNI))
    ZSAND(:) = ZFIELD(:) / 100. ! this field is given in percent
    DEALLOCATE (ZFIELD)
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
! 3.  Allocate soil ice reservoir
!     ---------------------------
!
ALLOCATE(ZWGI(NNI,2))
!
!-------------------------------------------------------------------------------
!
! 4.  Read layer 1 soil ice
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('WGI1  ',ZFIELD,IRET)
IF (IRET == 0) THEN
  WRITE (KLUOUT,'(A)') ' -> Soil ice level 1 is present'
  ZWGI(:,1) = ZFIELD(:)
  DEALLOCATE(ZFIELD)
ELSE
  ZWGI(:,1) = 0.
END IF
!
!
!-------------------------------------------------------------------------------
!
! 5.  Read layer 2 soil ice
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER('WGI2  ',ZFIELD,IRET)
IF (IRET == 0) THEN
  WRITE (KLUOUT,'(A)') ' -> Soil ice level 2 is present'
  ZWGI(:,2) = ZFIELD(:)
  DEALLOCATE(ZFIELD)
ELSE
  ZWGI(:,2) = 0.
END IF
!
!
!-------------------------------------------------------------------------------
!
! 5.  Read layer 2 depth (ISBA only)
!     ------------------
!
ALLOCATE(PD(NNI,3))
!
IF (GISBA) THEN
  ALLOCATE (ZFIELD(NNI))
  CALL READ_BUFFER('D2    ',ZFIELD,IRET)
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_BUFFER: LEVEL 2 DEPTH FOR ICE MISSING')
  END IF
  PD(:,1) = 0.01
  PD(:,2) = 0.20
  PD(:,3) = ZFIELD(:)
  !
  !* updates Wgi in m3/m3
  !
  ZWGI(:,1) = ZWGI(:,1) / 10.
  ZWGI(:,2) = ZWGI(:,2) /(1000. * ZFIELD(:))
  DEALLOCATE(ZFIELD)
ELSE
  PD(:,1) = 0.01
  PD(:,2) = 0.20
  PD(:,3) = 2.0
END IF
!
!
!-------------------------------------------------------------------------------
!
! 6.  Compute relative humidity from units kg/m^2
!     -------------------------------------------
!
ALLOCATE(PFIELD(NNI,3))
!
! Compute ISBA model constants (if needed)
!
IF (GISBA) THEN
  ALLOCATE (ZWFC  (NNI))
  ALLOCATE (ZWWILT(NNI))
  ALLOCATE (ZWSAT (NNI))
  !
  ZWSAT (:) = (-1.08*100. * ZSAND(:) + 494.305) * 0.001
  ZWWILT(:) = 37.1342E-3 * SQRT( 100. * ZCLAY(:) )
  ZWFC  (:) = 89.0467E-3 * (100. * ZCLAY(:) )**0.3496
  !
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)

  ZWGI(:,1) = MAX(MIN(ZWGI(:,1),ZWSAT),0.)
  ZWGI(:,2) = MAX(MIN(ZWGI(:,2),ZWSAT),0.)
  !
  PFIELD(:,1) = ZWGI(:,1) / ZWSAT
  PFIELD(:,2) = ZWGI(:,2) / ZWSAT
  PFIELD(:,3) = PFIELD(:,2)
  DEALLOCATE (ZWSAT)
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
  !
ELSE ! Non ISBA
  PFIELD(:,1) = 0.
  PFIELD(:,2) = 0.
  PFIELD(:,3) = 0.
END IF
!
DEALLOCATE(ZWGI)
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_WGI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_WGI
!
!-------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_TG(KLUOUT,HINMODEL,PFIELD,PD)
!     #######################
!
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!
!* local variables
!  ---------------
!
REAL,    DIMENSION(:), POINTER    :: ZFIELD    ! field to read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TG',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') ' | Reading soil temperature'
!
!--------------------------------------------------------------------------------
!
! 1.  Allocate soil temperature profile
!     ---------------------------------
!
ALLOCATE(PFIELD(NNI,3))
ALLOCATE(PD    (NNI,3))
!
!--------------------------------------------------------------------------------
!
! 2.  Search and read level 1 (and its depth)
!     -----------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER_TS(KLUOUT,HINMODEL,ZFIELD)
!
PFIELD(:,1) = ZFIELD(:)
PD    (:,1) = 0.01
DEALLOCATE(ZFIELD)
!
!--------------------------------------------------------------------------------
!
! 3.  Deep soil temperature
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER_T2(KLUOUT,HINMODEL,ZFIELD)
!
PFIELD(:,2) = ZFIELD(:)
PD    (:,2) = 0.4         ! deep temperature depth assumed equal to 0.2m
DEALLOCATE(ZFIELD)
!
!--------------------------------------------------------------------------------
!
! 4.  Assumes uniform temperature profile below
!     -----------------------------------------
!
PFIELD(:,3) = PFIELD(:,2)
PD    (:,3) = 5.          ! temperature profile down to 5m
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TG',1,ZHOOK_HANDLE)
!
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_TG
!-------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_SNOW_VEG_DEPTH(KLUOUT,HINMODEL,PFIELD)
!     #######################
!
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XRHOSMAX
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   POINTER       :: PFIELD    ! field to initialize
!
!
!* local variables
!  ---------------
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SNOW_VEG_DEPTH',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') ' | Reading snow depth'
!
!--------------------------------------------------------------------------------
!
! 1.  Allocate soil temperature profile
!     ---------------------------------
!
ALLOCATE(PFIELD(NNI))
!
!--------------------------------------------------------------------------------
!
! 2.  Search and read level 1 (kg/m2)
!     -----------------------
!
 CALL READ_BUFFER('SNOW  ',PFIELD,IRET)
!
! conversion to snow depth (meters)
!
  PFIELD = PFIELD / XRHOSMAX
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SNOW_VEG_DEPTH',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_SNOW_VEG_DEPTH
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_SNOW_VEG(KLUOUT,HINMODEL,PFIELD)
!     #######################
!
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XRHOSMAX
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   POINTER       :: PFIELD    ! field to initialize
!
!
!* local variables
!  ---------------
!
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SNOW_VEG',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') ' | Reading snow content (kg/m2)'
!
!--------------------------------------------------------------------------------
!
! 1.  Allocate soil temperature profile
!     ---------------------------------
!
ALLOCATE(PFIELD(NNI))
!
!--------------------------------------------------------------------------------
!
! 2.  Search and read level 1 (and its depth)
!     -----------------------
!
 CALL READ_BUFFER('SNOW  ',PFIELD,IRET)
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_SNOW_VEG',1,ZHOOK_HANDLE)
!
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_SNOW_VEG
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_T_TEB(KLUOUT,HINMODEL,PTI,PFIELD,PD)
!     #######################
!
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL,                 INTENT(IN)    :: PTI       ! internal temperature
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! normalized grid
!
!
!* local variables
!  ---------------
!
REAL,    DIMENSION(:), POINTER    :: ZFIELD    ! field to read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_T_TEB',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') ' | Reading temperature for buildings'
!
!--------------------------------------------------------------------------------
!
! 1.  Allocate soil temperature profile
!     ---------------------------------
!
ALLOCATE(PFIELD(NNI,3))
ALLOCATE(PD    (NNI,3))
!
!--------------------------------------------------------------------------------
!
! 2.  Search and read level 1
!     -----------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER_TS(KLUOUT,HINMODEL,ZFIELD)
!
PFIELD(:,1) = ZFIELD(:)
PD    (:,1) = 0.
DEALLOCATE(ZFIELD)
!
!--------------------------------------------------------------------------------
!
! 3.  Deep temperature
!     ----------------
!
PFIELD(:,2) = PTI
PD    (:,2) = 0.5         ! deep temperature depth assumed at half of wall or roof
!
!--------------------------------------------------------------------------------
!
! 4.  Assumes uniform temperature profile below
!     -----------------------------------------
!
PFIELD(:,3) = PTI
PD    (:,3) = 1.          ! temperature at building interior
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_T_TEB',1,ZHOOK_HANDLE)
!
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_T_TEB
!-------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_BUFFER_TF_TEB(KLUOUT,HINMODEL,PTI,PFIELD,PD)
!     #######################
!
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_READ_BUFFER
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL,                 INTENT(IN)    :: PTI       ! internal temperature
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!
!* local variables
!  ---------------
!
REAL,    DIMENSION(:), POINTER    :: ZFIELD    ! field to read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TF_TEB',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') ' | Reading soil temperature'
!
!--------------------------------------------------------------------------------
!
! 1.  Allocate soil temperature profile
!     ---------------------------------
!
ALLOCATE(PFIELD(NNI,3))
ALLOCATE(PD    (NNI,3))
!
!--------------------------------------------------------------------------------
!
! 2.  use building internal temperature as first level
!     -----------------------
!
ALLOCATE (ZFIELD(NNI))
!
PFIELD(:,1) = PTI
PD    (:,1) = 0.
DEALLOCATE(ZFIELD)
!
!--------------------------------------------------------------------------------
!
! 3.  Deep soil temperature
!     ---------------------
!
ALLOCATE (ZFIELD(NNI))
 CALL READ_BUFFER_T2(KLUOUT,HINMODEL,ZFIELD)
!
PFIELD(:,2) = ZFIELD(:)
PD    (:,2) = 0.5         ! deep temperature depth assumed at half of the floor
DEALLOCATE(ZFIELD)
!
!--------------------------------------------------------------------------------
!
! 4.  Assumes uniform temperature profile below
!     -----------------------------------------
!
PFIELD(:,3) = PFIELD(:,2)
PD    (:,3) = 1.          ! temperature profile down to depth of the floor
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_BUFFER:READ_BUFFER_TF_TEB',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE READ_BUFFER_TF_TEB
!-------------------------------------------------------------------
END MODULE MODE_READ_BUFFER
