!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
        MODULE MODD_DATA_LAKE
!     #####################
!
 CHARACTER(LEN=80), PARAMETER :: CLAKELTA = 'LAKE_LTA_NEW.nc'            ! The dataset file name
 CHARACTER(LEN=80), PARAMETER :: CLAKELDB = 'GlobalLakeDepth'    ! The file name of the map for global lake depth
 CHARACTER(LEN=80), PARAMETER :: CSTATUSLDB = 'GlobalLakeStatus' ! The file name of the map for global lake depth
!
INTEGER, PARAMETER :: NLONG=360, & ! Number of grid boxes of the "lake grid" in longitude
                      NLATG=150    ! Number of grid boxes of the "lake grid" in latitude
REAL, PARAMETER :: XFIRSTLAT=-60.   ! The first latitude of the "lake grid", deg. 
!
INTEGER, PARAMETER :: NGRADDEPTH_LTA = 12 ! Number of gradations for Depth
INTEGER, PARAMETER :: NGRADSTATUS_LDB = 5 ! Number of gradations for Status
INTEGER, PARAMETER :: NGRADDEPTH_LDB = 21 ! Number of gradations for Depth
!
REAL, DIMENSION(NGRADDEPTH_LTA), PARAMETER :: XCENTRGRADDEPTH_LTA = &  ! Central values for the gradations in depth, m
      (/1., 3., 5., 7., 10., 14., 18., 22., 27., 33., 39., 50./)
!                                             
REAL, DIMENSION(NGRADDEPTH_LDB), PARAMETER :: XCENTRGRADDEPTH_LDB = &  ! Central values for the gradations for status
      (/0., 1., 3., 5., 7., 10., 14., 18., 22., 27., 33., 39., 50., 70., 100., 150., 250., 400., 600., 1000., 1600./)
! Central values for the gradations for depth
INTEGER, DIMENSION(NGRADSTATUS_LDB), PARAMETER :: NCENTRGRADSTATUS_LDB = (/0, 1, 2, 3, 4/)
!
REAL, DIMENSION(NGRADDEPTH_LDB+1), PARAMETER :: XBOUNDGRADDEPTH_LDB = & ! Boundaries of gradations for depth
      (/-99999., 0., 2., 4., 6., 8., 12., 16., 20., 24., 30., 36., 42., 58., 82., 118., 182., 318., 482., 718., 1282., 99999.0/)
! Boundaries of gradations for status
REAL, DIMENSION(NGRADSTATUS_LDB+1), PARAMETER :: XBOUNDGRADSTATUS_LDB = (/-99999.0, 0.5, 1.5, 2.5, 3.5, 99999.0/)
!
REAL, PARAMETER :: XSMALL_DUMMY = -99999.0 ! Small value
!
REAL, PARAMETER :: XC_SMALL=0.01 ! Small value for the lake depth
!
REAL :: XAUXT_SNOW = 273.15
REAL :: XAUXT_ICE = 273.15
REAL :: XAUXT_MNW = 273.15
REAL :: XAUXT_WML = 273.15
REAL :: XAUXT_BOT = 273.15
REAL :: XAUXT_B1 = 273.15
REAL :: XAUXCT = 0.0
REAL :: XAUXH_SNOW = 0.0
REAL :: XAUXH_ICE = 0.0
REAL :: XAUXH_ML = 0.0
REAL :: XAUXH_B1 = 0.0
REAL :: XAUXT_SFC = 273.15
!
!REAL, PARAMETER :: XT_DUMMY=273.15, & ! Dummy value for temperature
!                   XC_DUMMY=0.0,    & ! Dummy value for the shape-factor
!                   XH_DUMMY=0.0       ! Dummy value for depth
!
END MODULE MODD_DATA_LAKE
