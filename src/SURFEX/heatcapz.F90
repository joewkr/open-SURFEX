!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE HEATCAPZ(PSANDZ,PHCAPSOIL)
!   ###############################################################
!!****  *HEATCAPZ*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates soil thermal conductivity components
!     using sand fraction and model constants in
!     order to calculate the thermal conductivity
!     following the method of Johansen (1975) as recommended
!     by Farouki (1986) parameterized for SVAT schemes
!     following Peters-Lidard et al. 1998 (JAS). This is
!     used in explicit calculation of CG (soil thermal
!     inertia): it is an option. DEFAULT is method of
!     Noilhan and Planton (1989) (see SOIL.F90).
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!    Peters-Lidard et al. 1998 (JAS)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!                  18/02/00    2D for veritcal profiles
!!                  2008/03     P. LeMoigne, ###it thrmconz subroutine
!!                  2014/10     B. Decharme, bug in soil solid heat capacity
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,   ONLY : XDRYWGHT, XSPHSOIL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:), INTENT(IN) :: PSANDZ     ! soil sand fraction (-)
!
REAL,   DIMENSION(:,:), INTENT(OUT):: PHCAPSOIL  ! soil solid heat capacity (J K-1 m-3) 
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HEATCAPZ',0,ZHOOK_HANDLE)
!
PHCAPSOIL(:,:) = XUNDEF
!
WHERE(PSANDZ(:,:)/=XUNDEF)
!   
!  Soil solid heat capacity from Peters-Lidard et al. 1998
!
   PHCAPSOIL(:,:) = XSPHSOIL*XDRYWGHT
!
END WHERE
!
IF (LHOOK) CALL DR_HOOK('HEATCAPZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE HEATCAPZ
