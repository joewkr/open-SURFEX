!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_BUFF
!     ##################
!
!!****  *MODD_IO_IO_BUFF - 
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
!
!*       0.   DECLARATIONS
!
!
IMPLICIT NONE

 CHARACTER(LEN=12), DIMENSION(50000) :: CREC   ! list of records already read/written
INTEGER                            :: NREC   ! number of records read/written

!
!
END MODULE MODD_IO_BUFF
