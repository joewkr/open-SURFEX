!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PREP_GRIB_GRID
CONTAINS
      SUBROUTINE PREP_GRIB_GRID(HGRIB,KLUOUT,HINMODEL,HGRIDTYPE,HINTERP_TYPE,TPTIME_GRIB)
!     ##########################################################################
!
!!****  *PREP_GRIB_GRID* - reads GRIB grid.
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
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson (from read_all_data_grib_case)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!!      S. Faroux 01/2011 : to use library GRIB_API instead of GRIBEX (from
!!                          read_all_data_grib_case)
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODE_READ_GRIB
!
USE MODD_HORIBL, ONLY : LGLOBLON, LGLOBS, LGLOBN, XILO1H, XILO2H, NO, NINLOH, &
                        XLA, XOLA, XOLO, NP, XLOPH
!
USE MODD_PREP,       ONLY : XLAT_OUT, XLON_OUT, LINTERP
!
USE MODD_GRID_ROTLATLON
USE MODD_GRID_GAUSS, ONLY : XILA1, XILO1, XILA2, XILO2, NINLA, NINLO, NILEN, LROTPOLE, XCOEF, XLAP, XLOP, &
                            XLAT, XLON
USE MODD_GRID_AROME, ONLY : XX, XY, NX, NY, XLAT0, XLON0, XLATOR, XLONOR, XRPK, XBETA, XZX, XZY, NIX
USE MODD_GRID_GRIB,  ONLY : NNI, CGRIB_FILE
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,       ONLY : XPI
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODI_HORIBL_SURF_INIT
USE MODI_HORIBL_SURF_COEF
USE MODI_ARPEGE_STRETCH_A
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!* 0.1. Declaration of arguments
!       ------------------------
!
 CHARACTER(LEN=*),  INTENT(IN)   :: HGRIB     ! Grib file name
INTEGER,          INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6), INTENT(OUT)   :: HINMODEL  ! Grib originating model
 CHARACTER(LEN=10), INTENT(OUT)  :: HGRIDTYPE ! Grid type
 CHARACTER(LEN=6),  INTENT(OUT)   :: HINTERP_TYPE ! Grid type
TYPE (DATE_TIME)                :: TPTIME_GRIB    ! current date and time

!
!* 0.2 Declaration of local variables
!      ------------------------------
!
 CHARACTER(LEN=50)                  :: HGRID         ! type of grid
! General purpose variables
INTEGER(KIND=kindOfInt), DIMENSION(:), ALLOCATABLE :: ININLO_GRIB
INTEGER(KIND=kindOfInt)                            :: IMISSING
INTEGER(KIND=kindOfInt)                            :: IUNIT
INTEGER(KIND=kindOfInt)                            :: IGRIB
INTEGER(KIND=kindOfInt)                            :: IRET          ! Return code from subroutines
!
! Variable involved in the task of reading the grib file
INTEGER                            :: ICENTER       ! number of center
INTEGER                            :: ISCAN, JSCAN
INTEGER                            :: ILENX ! nb points in X
INTEGER                            :: ILENY ! nb points in Y
INTEGER                            :: ITIME, IYEAR, IMONTH, IDAY
INTEGER                            :: IUNITTIME,IP1
INTEGER                            :: INO, IINLA     ! output number of points
REAL :: ZTIME
!
! Grib Grid definition variables
INTEGER                            :: JLOOP1        ! Dummy counter
!JUAN
!JUAN
INTEGER :: INFOMPI, J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID_1',0,ZHOOK_HANDLE)
!
IF (NRANK==NPIO) THEN
!
WRITE (KLUOUT,'(A)') ' -- Grib reader started'
!
! open grib file
 CALL GRIB_OPEN_FILE(IUNIT,HGRIB,'R',IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error opening the grib file '//HGRIB)
END IF
!
 CALL GRIB_NEW_FROM_FILE(IUNIT,IGRIB,IRET)
! needed for HIRLAM ROTLATLON (infos in the second record)
 CALL GRIB_NEW_FROM_FILE(IUNIT,IGRIB,IRET)
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading the grib file')
END IF
!
! close the grib file
 CALL GRIB_CLOSE_FILE(IUNIT)
!
!
!---------------------------------------------------------------------------------------
!* 2.  Fix originating center
!---------------------------------------------------------------------------------------
!
 CALL GRIB_GET(IGRIB,'centre',ICENTER,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading center')
END IF
!
 CALL GRIB_GET(IGRIB,'typeOfGrid',HGRID,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading type of grid')
END IF
!
SELECT CASE (ICENTER)

  CASE (96)
    SELECT CASE (HGRID)

      CASE('rotated_ll')
        WRITE (KLUOUT,'(A)') ' | Grib file from HIRLAM - regular latlon grid '
        HINMODEL='HIRLAM'
        HGRIDTYPE='ROTLATLON '

      CASE DEFAULT
        WRITE (KLUOUT,'(A)') ' | Grib file from HARMONY'
        HINMODEL='ALADIN'
        HGRIDTYPE='AROME     '

    END SELECT

  CASE (82)
    WRITE (KLUOUT,'(A)') ' | Grib file from HIRLAM'
    HINMODEL='HIRLAM'
    HGRIDTYPE='ROTLATLON '

  CASE (98)
    WRITE (KLUOUT,'(A)') ' | Grib file from European Center for Medium-range Weather Forecast'
    HINMODEL = 'ECMWF '
    HGRIDTYPE= 'GAUSS     '

  CASE (85)
    SELECT CASE (HGRID)

      CASE('regular_gg')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        WRITE (KLUOUT,'(A)') 'but same grid as ECMWF model (unstretched)'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'GAUSS     '

      CASE('reduced_gg')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        WRITE (KLUOUT,'(A)') 'but reduced grid'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'GAUSS     '

      CASE('regular_ll')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Mocage model'
        HINMODEL = 'MOCAGE'
        HGRIDTYPE= 'LATLON    '

      CASE('unknown_PLPresent')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'ROTGAUSS  '

      CASE('reduced_stretched_rotated_gg')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        WRITE (KLUOUT,'(A)') 'but reduced grid'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'ROTGAUSS  '

      CASE('lambert')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Aladin france model'
        HINMODEL = 'ALADIN'
        HGRIDTYPE= 'AROME     '

      CASE('mercator')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Aladin reunion model'
        HINMODEL = 'ALADIN'
        HGRIDTYPE= 'MERCATOR  '

    END SELECT

  CASE DEFAULT
    CALL ABOR1_SFX('PREP_GRIB_GRID: GRIB FILE FORMAT NOT SUPPORTED')

END SELECT
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(IGRIB,KIND(IGRIB)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(HINMODEL,LEN(HINMODEL),MPI_CHARACTER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(HGRIDTYPE,LEN(HGRIDTYPE),MPI_CHARACTER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
!
!---------------------------------------------------------------------------------------
!* 3. Number of points
!---------------------------------------------------------------------------------------
!
NX    = NUNDEF
NY    = NUNDEF
NINLA = NUNDEF
NILEN = NUNDEF
IF (ALLOCATED(NINLO)) DEALLOCATE(NINLO)
!
SELECT CASE (HGRIDTYPE)

     CASE ('AROME    ','MERCATOR  ')
     ! 3.1 Lambert conformal projection (ALADIN files)
     ! or Mercator projection (ALADIN REUNION files)
       IF (NRANK==NPIO) THEN
         CALL GRIB_GET(IGRIB,'Nj',NY,IRET)
         CALL GRIB_GET(IGRIB,'Ni',NX,IRET)
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(NY,KIND(NY)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(NX,KIND(NX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF
       NNI= NX * NY
     !
     !
     CASE ('GAUSS     ','ROTGAUSS  ','LATLON    ')
     ! 3.2 Usual or Gaussian lat,lon grid (ECMWF files)
     !

       IF (NRANK==NPIO) CALL GRIB_GET(IGRIB,'Nj',NINLA,IRET)
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(NINLA,KIND(NINLA)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF
       ALLOCATE (NINLO(NINLA))
       NILEN = 0
       IF (NRANK==NPIO) THEN
         ALLOCATE (ININLO_GRIB(NINLA))
        CALL GRIB_IS_MISSING(IGRIB,'pl',IMISSING,IRET)
        IF (IRET /= 0 .OR. IMISSING==1)  THEN !  regular
          CALL GRIB_GET(IGRIB,'Ni',ININLO_GRIB(1),IRET)
          ININLO_GRIB(2:NINLA)=ININLO_GRIB(1)
          NILEN=NINLA*ININLO_GRIB(1)
        ELSE !  quasi-regular
          CALL GRIB_GET(IGRIB,'pl',ININLO_GRIB)
          DO JLOOP1=1 ,NINLA
            NILEN = NILEN + ININLO_GRIB(JLOOP1)
          ENDDO
        ENDIF
        NINLO = ININLO_GRIB !JUAN
        DEALLOCATE(ININLO_GRIB)
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(NILEN,KIND(NILEN)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(NINLO,SIZE(NINLO)*KIND(NINLO)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF
       NNI = NILEN
     CASE ('ROTLATLON ')
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'Nj',NRY,IRET)
       CALL GRIB_GET(IGRIB,'Ni',NRX,IRET)
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(NRY,KIND(NRY)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(NRX,KIND(NRX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF
       NNI = NRX * NRY

     CASE DEFAULT
       CALL ABOR1_SFX('PREP_GRIB_GRID: GRID PROJECTION NOT SUPPORTED')
     !
END SELECT
!
!---------------------------------------------------------------------------------------
!* 4.  Updates grid information
!---------------------------------------------------------------------------------------
!
XX    = XUNDEF
XY    = XUNDEF
XILA1 = XUNDEF
XILO1 = XUNDEF
XILA2 = XUNDEF
XILO2 = XUNDEF
LROTPOLE = .FALSE.
XCOEF = XUNDEF
XLAP  = XUNDEF
XLOP  = XUNDEF

SELECT CASE (HGRIDTYPE)

     CASE ('AROME     ')
     ! 4.1 Lambert conformal projection (ALADIN files)
     !
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'xDirectionGridLength',ILENX)
       CALL GRIB_GET(IGRIB,'yDirectionGridLength',ILENY)
       XY = (NY-1)*ILENY
       XX = (NX-1)*ILENX
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XY,KIND(XY)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XX,KIND(XX)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'Latin1InDegrees',XLAT0)
       CALL GRIB_GET(IGRIB,'LoVInDegrees',XLON0)
       IF (XLON0 > 180.) XLON0 = XLON0 - 360.
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XLAT0,KIND(XLAT0)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLON0,KIND(XLON0)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XLATOR)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XLONOR)
       IF (XLONOR > 180.) XLONOR = XLONOR - 360.
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XLATOR,KIND(XLATOR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLONOR,KIND(XLONOR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

       XRPK  = SIN(XLAT0/180.*XPI)
       XBETA = 0.
     !
     CASE ('GAUSS     ','LATLON    ')
       HGRIDTYPE = 'GAUSS     '
     ! 4.2 Usual or Gaussian lat,lon grid (ECMWF files)
     !     No projection - just stores the grid definition
     !
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XILA1)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XILO1)
       CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XILA2)
       CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XILO2)
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XILA1,KIND(XILA1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILO1,KIND(XILO1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILA2,KIND(XILA2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILO2,KIND(XILO2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

       LROTPOLE = .FALSE.
     !
     CASE ('ROTLATLON ')
     !
     ! 4.2.5 Rotated lat/lon grid (HIRLAM)
     !
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'iScansNegatively',ISCAN)
       CALL GRIB_GET(IGRIB,'jScansPositively',JSCAN)

       IF (ISCAN+JSCAN == 0 ) THEN !lon (i) positive, lat (j) negative
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA2)
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO1)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA1)
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO2)
       ELSEIF (ISCAN+JSCAN == 2) THEN ! lon (i) negative, lat (j) positive
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA1)
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO2)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA2)
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO1)
       ELSEIF (ISCAN == 1) THEN ! lon (i) negative, lat (j) negative)
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA2)
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO2)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA1)
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO1)
       ELSEIF (JSCAN == 1) THEN ! lon (i) positive, lat (j) positive
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA1)
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO1)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA2)
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO2)
       ENDIF

       CALL GRIB_GET(IGRIB,'latitudeOfSouthernPoleInDegrees',XRLAP)
       CALL GRIB_GET(IGRIB,'longitudeOfSouthernPoleInDegrees',XRLOP)

       CALL GRIB_GET(IGRIB,'iDirectionIncrementInDegrees',XRDX)
       CALL GRIB_GET(IGRIB,'jDirectionIncrementInDegrees',XRDY)
       WRITE(KLUOUT,*)'XRILA1,XRILO1',XRILA1,XRILO1
       WRITE(KLUOUT,*)'XRILA2,XRILO2',XRILA2,XRILO2
       WRITE(KLUOUT,*)'XRLAP,XRLOP',XRLAP,XRLOP
       WRITE(KLUOUT,*)'XRDX,XRDY',XRDX,XRDY
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XRILA1,KIND(XRILA1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRILO1,KIND(XRILO1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRILA2,KIND(XRILA2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRILO2,KIND(XRILO2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRLAP,KIND(XRLAP)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRLOP,KIND(XRLOP)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRDX,KIND(XRDX)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XRDY,KIND(XRDY)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

     !
     CASE ('ROTGAUSS  ')
     ! 4.3 Stretched lat,lon grid (Arpege files)
     !
       HGRIDTYPE = 'GAUSS     '
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XILA1)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XILO1)
       CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XILA2)
       CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XILO2)

       CALL GRIB_GET(IGRIB,'stretchingFactor',XCOEF)
       CALL GRIB_GET(IGRIB,'latitudeOfStretchingPoleInDegrees',XLAP)
       CALL GRIB_GET(IGRIB,'longitudeOfStretchingPoleInDegrees',XLOP)
      ENDIF
      LROTPOLE = .TRUE.
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XILA1,KIND(XILA1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILO1,KIND(XILO1)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILA2,KIND(XILA2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XILO2,KIND(XILO2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XCOEF,KIND(XCOEF)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLAP,KIND(XLAP)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLOP,KIND(XLOP)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF

     !

     CASE ('MERCATOR  ')
     ! 4.4 Mercator  projection (ALADIN Reunion files)
     !
       HGRIDTYPE = 'AROME     '
       IF (NRANK==NPIO) THEN
       CALL GRIB_GET(IGRIB,'Dj',ILENY)
       CALL GRIB_GET(IGRIB,'Di',ILENX)
       XY = (NY-1)*ILENY
       XX = (NX-1)*ILENX

       CALL GRIB_GET(IGRIB,'LaDInDegrees',XLAT0)

       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XLATOR)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XLONOR)
       IF (XLONOR > 180.) XLONOR = XLONOR - 360.
       ENDIF
       IF (NPROC>1) THEN
#ifdef SFX_MPI
         CALL MPI_BCAST(XY,KIND(XY)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XX,KIND(XX)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLAT0,KIND(XLAT0)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLATOR,KIND(XLATOR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
         CALL MPI_BCAST(XLONOR,KIND(XLONOR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
       ENDIF
       XLON0 = 0.
       XRPK  = 0.
       XBETA = 0.

     CASE DEFAULT
       IF (NRANK==NPIO) WRITE (KLUOUT,'(A)') 'No such projection implemented in prep_grib_grid ',HGRID
       CALL ABOR1_SFX('PREP_GRIB_GRID: UNKNOWN PROJECTION')
     !
END SELECT
!---------------------------------------------------------------------------------------
!* 2.4 Read date
!---------------------------------------------------------------------------------------
!
WRITE (KLUOUT,'(A)') ' | Reading date'
!
IF (NRANK==NPIO) THEN
 CALL GRIB_GET(IGRIB,'year',IYEAR,IRET)
 CALL GRIB_GET(IGRIB,'month',IMONTH,IRET)
 CALL GRIB_GET(IGRIB,'day',IDAY,IRET)
 CALL GRIB_GET(IGRIB,'time',ITIME,IRET)
ZTIME=INT(ITIME/100)*3600+(ITIME-INT(ITIME/100)*100)*60
!
 CALL GRIB_GET(IGRIB,'P1',IP1,IRET)
IF ( IP1>0 ) THEN
  CALL GRIB_GET(IGRIB,'unitOfTimeRange',IUNITTIME,IRET)
  SELECT CASE (IUNITTIME)       ! Time unit indicator
    CASE (1)                    !hour
      ZTIME   = ZTIME + IP1*3600.
    CASE (0)                    !minute
      ZTIME   = ZTIME + IP1*60.
  END SELECT
ENDIF
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(IYEAR,KIND(IYEAR)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(IMONTH,KIND(IMONTH)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(IDAY,KIND(IDAY)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(ZTIME,KIND(ZTIME)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
#endif
ENDIF

TPTIME_GRIB%TDATE%YEAR = IYEAR
TPTIME_GRIB%TDATE%MONTH = IMONTH
TPTIME_GRIB%TDATE%DAY = IDAY
TPTIME_GRIB%TIME = ZTIME
!
!---------------------------------------------------------------------------------------
!
IF (NRANK==NPIO) THEN
CALL GRIB_RELEASE(IGRIB,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in releasing the grib message memory')
END IF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID_2',0,ZHOOK_HANDLE)
!
IF (ALLOCATED(XLAT_OUT)) THEN

   INO = SIZE(XLAT_OUT)

  IF (HGRIDTYPE=='GAUSS     ') THEN
    IF (ALLOCATED(XLAT)) DEALLOCATE(XLAT)
    IF (ALLOCATED(XLON)) DEALLOCATE(XLON)
    ALLOCATE(XLAT    (INO))
    ALLOCATE(XLON    (INO))
    IF (LROTPOLE) THEN
!* transformation of output latitudes, longitudes into rotated coordinates
      CALL ARPEGE_STRETCH_A(INO,XLAP,XLOP,XCOEF,XLAT_OUT,XLON_OUT,XLAT,XLON)
    ELSE
      XLAT = XLAT_OUT
      XLON = XLON_OUT
    END IF
  ELSEIF (HGRIDTYPE=='AROME     ') THEN
    IF (ALLOCATED(XZX)) DEALLOCATE(XZX)
    IF (ALLOCATED(XZY)) DEALLOCATE(XZY)
    IF (ALLOCATED(NIX)) DEALLOCATE(NIX)
    ALLOCATE(XZX      (INO))
    ALLOCATE(XZY      (INO))
    ALLOCATE(NIX(NY))
    NIX=NX
    CALL XY_CONF_PROJ(XLAT0,XLON0,XRPK,XBETA,XLATOR,XLONOR,XZX,XZY,XLAT_OUT,XLON_OUT)
  ELSEIF (HGRIDTYPE=='ROTLATLON ') THEN
  ENDIF
!
  IF (ALLOCATED(NO)) DEALLOCATE(NO)
  IF (ALLOCATED(XLA)) DEALLOCATE(XLA)
  IF (ALLOCATED(XOLA)) DEALLOCATE(XOLA)
  IF (ALLOCATED(XOLO)) DEALLOCATE(XOLO)
  IF (ALLOCATED(NINLOH)) DEALLOCATE(NINLOH)

  ALLOCATE(NO(INO,4))
  ALLOCATE(XOLA(INO),XOLO(INO))
  ALLOCATE(XLA(INO,4))
!

  !
  IF (HGRIDTYPE=='GAUSS     ') THEN
    IINLA = NINLA
    ALLOCATE(NINLOH(IINLA+4))
    CALL HORIBL_SURF_INIT(XILA1,XILO1,XILA2,XILO2,NINLA,NINLO,INO,XLON,XLAT, &
                          LINTERP,LGLOBLON,LGLOBN,LGLOBS,NO, &
                          NINLOH,XOLA,XOLO,XILO1H,XILO2H,XLA)
  ELSEIF (HGRIDTYPE=='AROME     ') THEN
    IINLA = NY
    ALLOCATE(NINLOH(IINLA+4))
    CALL HORIBL_SURF_INIT(0.,0.,XY,XX,NY,NIX,INO,XZX,XZY, &
                          LINTERP,LGLOBLON,LGLOBN,LGLOBS,NO, &
                          NINLOH,XOLA,XOLO,XILO1H,XILO2H,XLA)
  ENDIF
!
  IF (ALLOCATED(NP)) DEALLOCATE(NP)
  IF (ALLOCATED(XLOPH)) DEALLOCATE(XLOPH)
  ALLOCATE(NP(INO,12))
  ALLOCATE(XLOPH(INO,12))

  IF (LGLOBS) IINLA = IINLA + 2
  IF (LGLOBN) IINLA = IINLA + 2

  IF (HGRIDTYPE=='GAUSS     '.OR.HGRIDTYPE=='AROME     ') THEN
    CALL HORIBL_SURF_COEF(INO,LINTERP,LGLOBLON,XILO1H,XILO2H,XOLO,&
                          NO,NINLOH(1:IINLA),NP,XLOPH)
  ENDIF
!
ENDIF
!
HINTERP_TYPE = "HORIBL"
!
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID_2',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_GRIB_GRID
END MODULE MODI_PREP_GRIB_GRID
