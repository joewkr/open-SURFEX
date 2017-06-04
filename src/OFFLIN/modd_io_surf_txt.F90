!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_SURF_TXT
!     ##################
!
!!****  *MODD_IO_SURF_TXT - 
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
!!     P. LeMoigne 04/2004 : distinguish in and out file name
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
INTEGER, DIMENSION(:), POINTER :: NMASK ! 1D mask to read only interesting
 CHARACTER(LEN=6)               :: CMASK ! surface mask type
INTEGER                        :: NFULL ! total number for points of surface
!
END MODULE MODD_IO_SURF_TXT
