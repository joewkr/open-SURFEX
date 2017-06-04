!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_WRITE_SURF_ATM
!     ####################
!
!!****  *MODD_WRITE_SURF_ATM - declaration of writing surface ATM
!!
!!    PURPOSE
!!    -------
!     Declaration of flags to write out fields in historical fields
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
!!      P. Le Moigne *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2008
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
LOGICAL    :: LNOWRITE_CANOPY  ! flag used to avoid writing of canopy fields in OUTPUT file
LOGICAL    :: LNOWRITE_TEXFILE ! flag used to avoid writing of tex file describing parameters
!
LOGICAL :: LNAM_ATM_WRITTEN = .TRUE.
LOGICAL :: LNAM_FLAKE_WRITTEN = .TRUE.
LOGICAL :: LNAM_ISBA_WRITTEN = .TRUE.
LOGICAL :: LNAM_SEAFLUX_WRITTEN = .TRUE.
LOGICAL :: LNAM_TEB_WRITTEN = .TRUE.
LOGICAL :: LNAM_WATFLUX_WRITTEN = .TRUE.
!
LOGICAL :: LFIRST_WRITE = .TRUE.
LOGICAL, DIMENSION(50000) :: LNOWRITE
INTEGER :: NCPT_WRITE = 0
!
LOGICAL :: LSPLIT_PATCH = .TRUE.
!
!-----------------------------------------------------------------------------------------------------
!
END MODULE MODD_WRITE_SURF_ATM
