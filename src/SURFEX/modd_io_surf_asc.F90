!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_SURF_ASC
!     ##################
!
!!****  *MODD_IO_SURF_ASC - 
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
 CHARACTER(LEN=28),SAVE :: CFILE = 'SURFIN.txt'
 CHARACTER(LEN=28),SAVE :: CFILEIN ='SURFIN.txt' ! Name of the input
 CHARACTER(LEN=28),SAVE :: CFILEIN_SAVE ='SURFIN.txt' ! Name of the input
 CHARACTER(LEN=28),SAVE :: CFILEOUT='SURFOUT.txt'! Name of the output
 CHARACTER(LEN=28),SAVE :: CFILEPGD     ='PGD.txt'    ! Name of the pgd file
INTEGER                :: NUNIT       ! logical unit of surface file
INTEGER                :: NLUOUT      ! logical unit of output file
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL() ! 1D mask to read only interesting
 CHARACTER(LEN=6)               :: CMASK ! surface mask type
INTEGER                        :: NFULL ! total number fo points of surface
LOGICAL, SAVE          :: LCREATED=.FALSE.   ! flag to know if the output file was created
INTEGER                :: NNI_FORC      ! number of points in forcing files.
!
END MODULE MODD_IO_SURF_ASC
