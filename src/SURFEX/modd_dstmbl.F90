!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
MODULE MODD_DSTMBL
!
IMPLICIT NONE
!
REAL, PARAMETER :: XDMT_SLT_OPT = 75.0e-6 ! [m] Optimal diameter for saltation, IvW82 p. 117 Fgr. 8, Pye87 p. 31, MBA97 p. 4388, SRL96 (2)
REAL, PARAMETER :: XDMT_ERO_OPT = 6.7e-3  ! [mm]
REAL, PARAMETER :: XGAMA = 2.5            ! dimensionless speed factor
REAL, PARAMETER :: XDNS_SLT = 2650.0      ! [kg m-3] Density of optimal saltation particles, MBA97 p. 4388 
REAL, PARAMETER :: XCST_SLT = 2.61        ! [frc] Saltation constant Whi79 p. 4648, MaB97 p. 16422 
!
! fxm: rgh_mmn_smt set to 33.3e-6 um, MaB95 p. 16426 recommend 10.0e-6
REAL,PARAMETER:: XRGH_MMN_SMT = 33.3e-6   ! [m] Smooth roughness length MaB95 p. 16426, MaB97 p. 4392, GMB98 p. 6207
!
REAL, PARAMETER :: XFLX_MSS_FDG_FCTM = 1.08d0  ! [frc] Global mass flux tuning factor (a posteriori)
INTEGER, PARAMETER :: NTEX = 12                ! number of texture
INTEGER, PARAMETER :: NMODE=3                  ! number of mode
INTEGER, PARAMETER :: NDP=100                  ! number of paticle
INTEGER, PARAMETER :: NBIN=4                   ! number of bin
!
END MODULE MODD_DSTMBL
