!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_SURF_ATM
!     ##################
!
!!****  *MODN_PREP_SURF_ATM* - declaration of namelist NAM_PREP_SURF_ATM
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
!!      Original    01/2004                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
 CHARACTER(LEN=28) :: CFILE        ! file name
 CHARACTER(LEN=6)  :: CFILETYPE    ! file type
 CHARACTER(LEN=28) :: CFILEPGD        ! file name
 CHARACTER(LEN=6)  :: CFILEPGDTYPE    ! file type
INTEGER           :: NHALO_PREP   ! HALO for nearest point extrapolation
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface
LOGICAL           :: LWRITE_EXTERN
!
NAMELIST/NAM_PREP_SURF_ATM/CFILE, CFILETYPE, CFILEPGD, CFILEPGDTYPE, NHALO_PREP, &
         NYEAR, NMONTH, NDAY, XTIME, LWRITE_EXTERN
!
!-------------------------------------------------------------------------------
!
END MODULE MODN_PREP_SURF_ATM
