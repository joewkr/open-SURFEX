!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_WRITE_TXT
!     ##################
!
!!****  *MODD_WRITE_TXT - 
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
!!      P. LE MOIGNE    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
!      
INTEGER, PARAMETER                  :: JPVAR = 700             ! maximum number of fields to write      
 CHARACTER(LEN=12), DIMENSION(JPVAR) :: CVAR='                ' ! names of fields to write
 CHARACTER(LEN=12), DIMENSION(JPVAR) :: CVARN='                ' ! names of fields to write
INTEGER, DIMENSION(JPVAR)           :: NVAR                    ! unit number associated to CVAR elements
INTEGER                             :: NIND                    ! current unit number
INTEGER                             :: NUNIT0=33
!
END MODULE MODD_WRITE_TXT
