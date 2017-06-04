!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############
      MODULE MODD_IGN      
!     ###############
!
!!****  *MODD_IGN* - declaration of constants for the Lambert projections 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     Constants for the Lambert projections
!  KLAMB    PROJECTION          (Namelist code)
!              
!    1 :    Lambert I                 'L1'
!    2 :    Lambert II                'L2'
!    3 :    Lambert III               'L3'
!    4 :    Lambert IV                'L4'
!    5 :    Extended Lambert II       'L2E'
!    6 :    Lambert 93                'L93'
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!     NOTE TECHNIQUE IGN NT/G 71 : 
!!        PROJECTION CARTOGRAPHIQUE CONIQUE COMFORME DE LAMBERT
!!        (www.ign.fr)
!!    AUTHOR
!!    ------
!!      E. Martin    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007   
!       02/2011     Correction de la longitude d'origine pour L93 (A. Lemonsu)
!       01/2016     Correction de la valeur de l'excentricite pour L93 (V. Masson)
!       01/2016     Correction de la valeur du rayon terrestre pour Lamberts 1 a 4 (V. Masson)
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
REAL, DIMENSION(6) :: XN = (/ 0.7604059656,0.7289686274,0.6959127966, &
                                0.6712679322,0.7289686274,0.7256077650 /)  
!                            ! exposant de projection (n) 
!
REAL, DIMENSION(6) :: XC = (/ 11603796.98,11745793.39,11947992.52,    &
                                12136281.99,11745793.39,11754255.426   /)  
                              ! constante de projection (c) (m)
!
REAL, DIMENSION(6) :: XXS= (/ 600000.,600000.,600000.,                &
                                234.358,600000.,700000. /)  
                              ! coordonnée X en projection du Pôle (Xs) (m)
!
REAL, DIMENSION(6) :: XYS= (/ 5657616.674, 6199695.768,6791905.085,   &
                                7239161.542, 8199695.768,12655612.050 /)  
                              ! coordonnée Y en projection du Pôle (Ys) (m)
!
REAL, DIMENSION(6) :: XLONP= (/ 2.33722917, 2.33722917, 2.33722917, &
                                2.33722917, 2.33722917, 3.         /)
                              ! longitude de référence (deg)
                              ! Méridien de Paris pour L1,L2,L3,L4,L2E
                              ! 3° Est Greenwitch pour L93
!
REAL, DIMENSION(6) :: XECC= (/ 0.08248325676, 0.08248325676, 0.08248325676, &
                               0.08248325676, 0.08248325676, 0.08181919112  /)
                              ! premiere excentricité de l'ellispoide terrestre
!
REAL, DIMENSION(6) :: XA= (/ 6378249.2, 6378249.2, 6378249.2, &
                             6378249.2, 6378249.2, 6378137.0  /)
                              ! 1/2 grand axe de l'ellipsoide terrestre (m)
!---------------------------------------------------------------------
! Parameters for calculations and approximations
!----------------------------------------------------------------------
REAL               :: XCVGLAT = 1.E-11  ! Small number for convergence
!                                         tests for latitude retrieval
REAL               :: XDELTY = 1000. ! Increment for calculation of 
!                                     angle with the North    
!
REAL               :: XEXPAND = 200000. ! distance for expansion of
!                                         the grid for latlonmask_ign
!                                         to account for projection
!                                         deformation (m)
!
END MODULE MODD_IGN

