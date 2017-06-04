!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_WRITE_BIN
!     ##################
!
!!****  *MODD_WRITE_BIN - 
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
!!      A. LEMONSU      *Meteo France*
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
INTEGER, DIMENSION(JPVAR)           :: NVAR                    ! unit number associated to CVAR elements
INTEGER                             :: NIND                    ! current unit number
INTEGER                             :: NUNIT0=33
INTEGER                             :: NWRITE                  ! counter for writing
!
!
END MODULE MODD_WRITE_BIN
