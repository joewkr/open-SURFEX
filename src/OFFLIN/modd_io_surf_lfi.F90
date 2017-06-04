!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_SURF_LFI
!     ##################
!
!!****  *MODD_IO_SURF_LFI - 
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    
USE MODD_SURF_PAR, ONLY : NUNDEF
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
 CHARACTER(LEN=28),SAVE :: CLUOUT_LFI                  ! Name of the listing
 CHARACTER(LEN=28),SAVE :: CFILE_LFI                   ! Name of the current file
 CHARACTER(LEN=28),SAVE :: CFILEIN_LFI                 ! Name of the input
 CHARACTER(LEN=28),SAVE :: CFILEIN_LFI_SAVE            ! Name of the input
 CHARACTER(LEN=28),SAVE :: CFILEOUT_LFI                ! Name of the output
 CHARACTER(LEN=28),SAVE :: CFILEDIAG_LFI               ! Name of the output
 CHARACTER(LEN=28),SAVE :: CFILEPGD_LFI                ! Name of the pgd file
INTEGER                :: NUNIT_LFI      ! logical unit of surface file (LFI part)
INTEGER                :: NLUOUT         ! logical unit of output file
!
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL() ! 1D mask to read only interesting
 CHARACTER(LEN=6)               :: CMASK ! surface mask type
INTEGER                        :: NFULL     = NUNDEF ! total number fo points of surface
INTEGER                        :: NFULL_AUX = NUNDEF ! total number fo points of surface (Auxilarry file for prep)
INTEGER                        :: NFULL_SURF= NUNDEF ! total number fo points of surface (PGD, PREP or SURFace prognostic file)
!
!* variables to insure compatibility with MesoNH and AROME files
!
LOGICAL :: LMNH_COMPATIBLE = .FALSE.    ! true if grid is compatible with MesoNH
LOGICAL :: LCARTESIAN                   ! flag for cartesian grid
INTEGER :: NIU = 0                      ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NIB = NUNDEF                 ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NIE = NUNDEF                 ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NJU = 0                      ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NJB = NUNDEF                 ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NJE = NUNDEF                 ! horizontal indexes of the grid (any file, including auxilliary file in PREP)
INTEGER :: NIU_SURF = NUNDEF
INTEGER :: NIB_SURF = NUNDEF
INTEGER :: NIE_SURF = NUNDEF 
INTEGER :: NJU_SURF = NUNDEF
INTEGER :: NJB_SURF = NUNDEF
INTEGER :: NJE_SURF = NUNDEF
                                        ! horizontal indexes of the grid (PGD, PREP or SURFACE file)
!
END MODULE MODD_IO_SURF_LFI
