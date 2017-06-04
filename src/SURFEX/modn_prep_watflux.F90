!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_WATFLUX
!     ##################
!
!!****  *MODN_PREP_WATFLUX* - declaration of namelist NAM_PREP_WATFLUX
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_WATFLUX
!     which concern the surface configuration.
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
!!      S.Malardel    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_WATFLUX, ONLY : CFILE_WATFLX, CTYPE, CFILEPGD_WATFLX, CTYPEPGD, XTS_WATER_UNIF

!
IMPLICIT NONE
!
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface
LOGICAL           :: LWAT_SBL     ! flag to use air layers inside the Surface Boundary Layer

!
NAMELIST/NAM_PREP_WATFLUX/CFILE_WATFLX, CTYPE, CFILEPGD_WATFLX, CTYPEPGD, XTS_WATER_UNIF,   &
                          NYEAR, NMONTH, NDAY, XTIME, LWAT_SBL  
!
END MODULE MODN_PREP_WATFLUX
