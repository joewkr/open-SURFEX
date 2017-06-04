!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_IO_SURF_FA
!     ##################
!
!!****  *MODD_IO_SURF_FA - 
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
IMPLICIT NONE
!
CHARACTER(LEN=6), SAVE  :: CDNOMC ='header'     ! Name of the header
CHARACTER(LEN=28),SAVE  :: CFILE_FA       ='SURFIN.fa'  ! Name of the input
CHARACTER(LEN=28),SAVE  :: CFILEIN_FA       ='SURFIN.fa'  ! Name of the input
CHARACTER(LEN=28),SAVE  :: CFILEIN_FA_SAVE  ='SURFIN.fa'  ! Name of the input
CHARACTER(LEN=28),SAVE  :: CFILEOUT_FA      ='SURFOUT.fa' ! Name of the input
CHARACTER(LEN=28),SAVE  :: CFILEPGD_FA      ='PGD.fa'     ! Name of the pgd file
CHARACTER(LEN=4), SAVE  :: CPREFIX1D        ='SFX.'       ! Prefix name in fa file
CHARACTER(LEN=1), SAVE  :: CPREFIX2D        ='X'          ! Prefix name in fa file
!
CHARACTER(LEN=6)        :: CMASK     ! surface mask type
!
INTEGER                 :: NUNIT_FA         =19           ! logical unit of surface file (FA part)
INTEGER                 :: NLUOUT         ! logical unit of output file
INTEGER                 :: IVERBFA=0      ! amount of message from FA lib 
!
LOGICAL, SAVE           :: LFANOCOMPACT=.FALSE. 
LOGICAL, SAVE           :: LPREP       =.FALSE.
LOGICAL                 :: LOPEN   ! flag to know if the file has been openned during the surface call
!      
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL()     ! 1D mask to read only interesting
INTEGER                        :: NFULL     ! total number of points of surface
INTEGER                        :: NFULL_EXT ! total number of points including the extension zone (ALADIN)
INTEGER                        :: NDGL,NDLON,NDLUX,NDGUX        ! dimensions of ALADIN domain
!
REAL                           :: PERPK,PEBETA,PELON0,PELAT0,  &! grid projection parameters
                                  PEDELX,PEDELY,PELON1,PELAT1   ! for the ALADIN domain  
!
END MODULE MODD_IO_SURF_FA
