!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     ##############################################
MODULE MODI_VER_INTERP_LIN3D_SURF
CONTAINS
      FUNCTION VER_INTERP_LIN3D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2),SIZE(KKLIN,3))                   &
                                       :: PVAR2 ! variable values on target
!                                             ! grid
END FUNCTION VER_INTERP_LIN3D_SURF
END MODULE MODI_VER_INTERP_LIN3D_SURF
