!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.

!     ######################
      MODULE MODD_OL_FILEID
!     ######################
!
!!****  *MODD_OL_FILEID* Keep in memory the netcdf ID of the output files
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!------------------------------------------------------------------------------
!
!* variables for each patch
!
 CHARACTER(LEN=200), DIMENSION(19) :: XNETCDF_FILENAME_IN= &
                                       (/'PARAMS.nc                  ',&
                                         'FORCING.nc                 ',&
                                         'Forc_TA.nc                 ',&
                                         'Forc_QA.nc                 ',&
                                         'Forc_PS.nc                 ',&
                                         'Forc_DIR_SW.nc             ',&
                                         'Forc_SCA_SW.nc             ',&
                                         'Forc_LW.nc                 ',&
                                         'Forc_RAIN.nc               ',&
                                         'Forc_SNOW.nc               ',&
                                         'Forc_WIND.nc               ',&
                                         'Forc_DIR.nc                ',& 
                                         'Forc_CO2.nc                ',&
                                         'SXPOST.nc                  ',&
                                         'LAND_USE.nc                ',&
                                         'ISBA_VEG_EVOLUTION_P.OUT.nc',&
                                         'ISBA_VEG_EVOLUTION_A.OUT.nc',&
                                         'ISBA_PROGNOSTIC.OUT.nc     ',&
                                         'ISBA_DIAGNOSTICS.OUT.nc    '/)
 CHARACTER(LEN=200), DIMENSION(25) :: XNETCDF_FILENAME_OUT= &
                                       (/'ISBA_VEG_EVOLUTION.OUT.nc  ',&
                                         'ISBA_VEG_EVOLUTION_P.OUT.nc', &
                                         'ISBA_VEG_EVOLUTION_A.OUT.nc', &
                                         'ISBA_PROGNOSTIC.OUT.nc     ',&
                                         'ISBA_DIAGNOSTICS.OUT.nc    ',&
                                         'ISBA_DIAG_CUMUL.OUT.nc     ',&
                                         'ISBA_ANALYSIS.OUT.nc       ',&
                                         'SEAFLUX_PROGNOSTIC.OUT.nc  ',&
                                         'SEAFLUX_DIAGNOSTICS.OUT.nc ',&
                                         'SEAFLUX_DIAG_CUMUL.OUT.nc  ',&
                                         'WATFLUX_PROGNOSTIC.OUT.nc  ',&
                                         'WATFLUX_DIAGNOSTICS.OUT.nc ',&
                                         'WATFLUX_DIAG_CUMUL.OUT.nc  ',&
                                         'FLAKE_PROGNOSTIC.OUT.nc    ',&
                                         'FLAKE_DIAGNOSTICS.OUT.nc   ',&
                                         'FLAKE_DIAG_CUMUL.OUT.nc    ',&
                                         'TEB_PROGNOSTIC.OUT.nc      ',&
                                         'GARDEN_PROGNOSTIC.OUT.nc   ',&
                                         'GREENROOF_PROGNOSTIC.OUT.nc',&
                                         'TEB_DIAGNOSTICS.OUT.nc     ',&
                                         'TEB_PGD.OUT.nc             ',&
                                         'TEB_CANOPY.OUT.nc          ',&
                                         'TEB_DIAG_CUMUL.OUT.nc      ',&
                                         'SURF_ATM.OUT.nc            ',&
                                         'SURF_ATM_DIAGNOSTICS.OUT.nc'/)  
INTEGER, DIMENSION(25) :: XNETCDF_FILEID_OUT
!
 CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: XVAR_TO_FILEIN !contains names
                                                                !of variables to write
INTEGER*4, DIMENSION(:), ALLOCATABLE :: XID_IN, XID_VARIN  !contains ids of
                                                           !opened files for each 
                                                           !variable to write
INTEGER :: XIN
INTEGER :: XCOUNT
!------------------------------------------------------------------------------
!
END MODULE MODD_OL_FILEID

