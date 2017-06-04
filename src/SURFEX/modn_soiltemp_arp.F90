!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_SOILTEMP_ARP
!!    #####################
!!
!!*** *MODN_SOITEMP_ARP*
!!
!!    PURPOSE
!!    -------
!       Namelist for  
!!
!!**  AUTHOR
!!    ------
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
!
INTEGER, PARAMETER          :: NMAX_LAYER=10
LOGICAL                     :: LTEMP_ARP
INTEGER                     :: NTEMPLAYER_ARP
REAL, DIMENSION(NMAX_LAYER) :: SODELX
!
NAMELIST /NAM_SOILTEMP_ARP/LTEMP_ARP, NTEMPLAYER_ARP, SODELX
!
END MODULE MODN_SOILTEMP_ARP
