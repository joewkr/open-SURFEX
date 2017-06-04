!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_SGH_PAR
!     ######################
!
!!****  *MODD_SGH_PAR* - declaration of SGH parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the SGH scheme (Decharme and Douville,
!     Climate Dyn. 2006a). 
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
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
!------------------------------------------------------------------------------     
!
INTEGER,PARAMETER     :: NDIMTAB = 100     
!      
REAL, PARAMETER       :: X2 = 2.
!
REAL, PARAMETER       :: X4 = 4.
!
REAL, PARAMETER       :: XF_DECAY = 2.
!
REAL, PARAMETER       :: XICE_DEPH_MAX = 0.2
!
REAL, PARAMETER       :: XREGP = 0.961
!
REAL, PARAMETER       :: XREGA = 1.957
!
REAL, PARAMETER       :: XMTOKM = 1000.
!
REAL, PARAMETER       :: XSTOHR = 3600.
!
REAL, PARAMETER       :: X001 = 0.01
!
REAL, PARAMETER       :: XMUREGP = 0.5
!
REAL, PARAMETER       :: XMUREGA = 0.2
!
REAL, PARAMETER       :: XHORT_DEPTH = 0.1
!
!--------------------------------------------------------------------------------
!
END MODULE MODD_SGH_PAR












