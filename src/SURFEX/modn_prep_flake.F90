!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
MODULE MODN_PREP_FLAKE
!     ##################
!
!!****  *MODN_PREP_FLAKE* - declaration of namelist NAM_PREP_FLAKE
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_FLAKE
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
USE MODD_PREP_FLAKE, ONLY : CFILE_FLAKE, CTYPE, CFILEPGD_FLAKE, CTYPEPGD, &
                              XTS_UNIF, &
                              XUNIF_T_SNOW, &
                              XUNIF_T_ICE, &
                              XUNIF_T_WML, &
                              XUNIF_T_BOT, &
                              XUNIF_T_B1, &
                              XUNIF_CT, &
                              XUNIF_H_SNOW, &
                              XUNIF_H_ICE, &
                              XUNIF_H_ML, &
                              XUNIF_H_B1, &
                              LCLIM_LAKE
!
IMPLICIT NONE
!
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface
LOGICAL           :: LWAT_SBL     ! flag to use air layers inside the Surface Boundary Layer
!
NAMELIST/NAM_PREP_FLAKE/CFILE_FLAKE, CTYPE, CFILEPGD_FLAKE, CTYPEPGD, XTS_UNIF,   &
                         XUNIF_T_SNOW, XUNIF_T_ICE, &
                         XUNIF_T_WML, &                         
                         XUNIF_T_BOT, XUNIF_T_B1,   &
                         XUNIF_CT,    &                         
                         XUNIF_H_SNOW, XUNIF_H_ICE,    &
                         XUNIF_H_ML, XUNIF_H_B1, &
                         LCLIM_LAKE, &                         
                         NYEAR, NMONTH, NDAY, XTIME, LWAT_SBL  
! Only one file is alowed, which name will be stored in CFILE_FLAKE.
! Initial conditions for all FLake variables may be present in the file.
!
!
END MODULE MODN_PREP_FLAKE
