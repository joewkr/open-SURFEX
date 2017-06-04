!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_WRITE_COVER_TEX
!     ######################
!
!!****  *MODD_WRITE_COVER_TEX* -
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
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER           :: NTEX       ! logical unit of file 'class.tex'
INTEGER, PARAMETER:: NLINES = 50! number of lines per page
!
 CHARACTER(LEN=2)                               :: CLANG = 'EN' ! language used
 CHARACTER(LEN=60), DIMENSION(:), ALLOCATABLE   :: CNAME        ! names of ecosystems (1 language)
!-------------------------------------------------------------------------------
!
END MODULE MODD_WRITE_COVER_TEX
