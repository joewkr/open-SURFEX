!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_SURF_BIN
!     ##################
!
!!****  *MODD_IO_SURF_BIN - 
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
!!      A. LEMONSU   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
INTEGER, DIMENSION(:), POINTER :: NMASK                   ! 1D mask to read only interesting
 CHARACTER(LEN=6)               :: CMASK                   ! surface mask type
 CHARACTER(LEN=28),SAVE         :: CFILEIN  ='SURFIN.txt'  ! Name of the input
 CHARACTER(LEN=28),SAVE         :: CFILEOUT ='SURFOUT.txt' ! Name of the input
INTEGER                        :: NFULL                   ! total number for points of surface
INTEGER                        :: NUNIT                   ! logical unit of surface file
INTEGER                        :: NLUOUT                  ! logical unit of output file
!
END MODULE MODD_IO_SURF_BIN
