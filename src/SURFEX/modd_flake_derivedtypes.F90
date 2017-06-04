!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!------------------------------------------------------------------------------

MODULE modd_flake_derivedtypes  

!------------------------------------------------------------------------------
!
! Description:
!
!  Derived type(s) is(are) defined.
!
!
! Current Code Owner: DWD, Dmitrii Mironov
!  Phone:  +49-69-8062 2705
!  Fax:    +49-69-8062 3721
!  E-mail: dmitrii.mironov@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.00       2005/11/17 Dmitrii Mironov 
!  Initial release 
! !VERSION!  !DATE!     <Your name>
!  <Modification comments>
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

!USE modd_data_parameters , ONLY : &
!    ireals                   ,  &! KIND-type parameter for real variables 
!    iintegers                    ! KIND-type parameter for "normal" integer variables  

!==============================================================================

IMPLICIT NONE

!==============================================================================
!
! Declarations

!  Maximum value of the wave-length bands 
!  in the exponential decay law for the radiation flux.
!  A storage for a ten-band approximation is allocated,
!  although a smaller number of bands is actually used.
INTEGER , PARAMETER ::  &
    nband_optic_max = 10  

!  Define TYPE "opticpar_medium"
TYPE opticpar_medium
  INTEGER                         ::    &
      nband_optic                                            ! Number of wave-length bands  
  REAL , DIMENSION (nband_optic_max) ::    &
      frac_optic                                         ,  &! Fractions of total radiation flux 
      extincoef_optic                                        ! Extinction coefficients   
END TYPE opticpar_medium

!==============================================================================

END MODULE modd_flake_derivedtypes  

