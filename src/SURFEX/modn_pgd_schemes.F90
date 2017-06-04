!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PGD_SCHEMES
!     ##################
!
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003                    
!!
!!      A. Lemonsu      05/2009         Key for vegetation in TEB (TEB-Veg)
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
 CHARACTER(LEN=6) :: CNATURE  ! type of scheme for natural covers: NONE, ISBA
 CHARACTER(LEN=6) :: CSEA     ! type of scheme for oceans        : NONE, CSEAFLX
 CHARACTER(LEN=6) :: CTOWN    ! type of scheme for towns         : NONE, CTEB
 CHARACTER(LEN=6) :: CWATER   ! type of scheme for inland waters : NONE, WATFLX, FLAKE
LOGICAL          :: LGARDEN  ! T : urban green areas treated inside TOWN by CTOWN scheme
!                            ! F : urban  greenareas treated inside NATURE aggregated
!                            !     with other vegetated surfaces
!
NAMELIST/NAM_PGD_SCHEMES/CNATURE, CSEA, CTOWN, CWATER, LGARDEN
!
END MODULE MODN_PGD_SCHEMES
