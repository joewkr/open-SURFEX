!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_BVOC_PAR
!     ####################
!
!!
!! Declaration of parameters for biogenic emissions 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!     
!*       0.   DECLARATIONS
!             ----------
!
IMPLICIT NONE
!
!
! Parameters for effect of canopy light attenuation when ISBA_STD is used
REAL, PARAMETER  :: XCANFAC=0.8
!
!Parametres Guenther's formula;
!
!isoprene
REAL, PARAMETER  :: XISO_ALF=0.0027
REAL, PARAMETER  :: XISO_CL =1.066
REAL, PARAMETER  :: XISO_CT1=95000.
REAL, PARAMETER  :: XISO_CT2 =230000.
REAL, PARAMETER  :: XISO_BTS =303.    !temperature of normalization
REAL, PARAMETER  :: XISO_BTM =314.
!
!monoterpenes
REAL, PARAMETER :: XMONO_BETA=0.09
REAL, PARAMETER :: XMONO_T3=303.      !temperature of normalization
!
!
!--------------------------------------------------
!Default emission potential for other types of vegetation than ligneous(france) 
!
!Grassland (Simpson et al, 1999)
!microg.m-2.hr-1)
REAL, PARAMETER  :: XISOPOT_GRASS= 40.
REAL, PARAMETER  :: XMONOPOT_GRASS= 40.
!
!Crops
REAL, PARAMETER  :: XISOPOT_CROP= 0.
REAL, PARAMETER  :: XMONOPOT_CROP= 100.
!
!
END MODULE MODD_BVOC_PAR
