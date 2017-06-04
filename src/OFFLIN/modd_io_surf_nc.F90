!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_IO_SURF_NC
!     ######################
!
!!****  *MODD_IO_SURF_NC* Keep in memory the netcdf ID of the output files
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
!!      S. Faroux   *Meteo France*
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
 CHARACTER(LEN=28),SAVE :: CFILE_NC
 CHARACTER(LEN=28),SAVE :: CFILEIN_NC 
 CHARACTER(LEN=28),SAVE :: CFILEIN_NC_SAVE            ! Name of the input
 CHARACTER(LEN=28),SAVE :: CFILEOUT_NC                ! Name of the output
 CHARACTER(LEN=28),SAVE :: CFILEOUT_NC_SAVE = ''      ! Name of the output
 CHARACTER(LEN=28),SAVE :: CFILEPGD_NC                ! Name of the pgd file
 CHARACTER(LEN=28),SAVE :: CLUOUT_NC
!
INTEGER :: NID_NC
INTEGER :: NLUOUT         ! logical unit of output file
LOGICAL, SAVE :: LCREATED = .FALSE.
LOGICAL :: LDEF = .FALSE.
!
INTEGER, DIMENSION(:),POINTER :: NMASK=>NULL()
 CHARACTER(LEN=6)               :: CMASK ! surface mask type
INTEGER :: NFULL
INTEGER :: NFULL_AUX ! total number fo points of surface (Auxilarry file for prep)
!
LOGICAL, SAVE                     :: LMASK = .FALSE.
!
INTEGER, DIMENSION(:),ALLOCATABLE :: NMASK_IGN
!
!------------------------------------------------------------------------------
!
END MODULE MODD_IO_SURF_NC

