!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########################################################################
SUBROUTINE SURF_SOLAR_SLOPES (PCOSZEN,PSINZEN,PAZIMSOL,PSLOPANG, PSLOPAZI, &
                              PSURF_TRIANGLE,PDIRSRFSWD,PDIRSWDT           )
!#########################################################################
!
!!****  * SURF_SOLAR_SLOPES * - computes the modifications to the downwards
!!                           direct solar flux at the surface, due to
!!                           orientation and shape of this surface.
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson      * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!!      V. Masson   01/03/03 add multiple wavelengths
!!
!!                03/14  : M Lafaysse, modifs for optimization and parallelization
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SLOPE_EFFECT, ONLY:NNXLOC,NNYLOC
!
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!

REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN ! COS(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PSINZEN ! SIN(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PAZIMSOL! azimuthal solar angle

REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPAZI   ! azimuthal slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPANG   ! vertical slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSURF_TRIANGLE ! surface of triangles
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIRSRFSWD!Downward SuRF. DIRect SW Flux
REAL, DIMENSION(:,:,:,:), INTENT(OUT):: PDIRSWDT ! shortwave flux received by 
!                                                ! each subgrid triangle
!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!
REAL,    PARAMETER :: XPI=4.*ATAN(1.)  ! Pi
INTEGER, PARAMETER :: JPHEXT = 1 ! number of points around the physical domain
!
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ, JB
INTEGER :: JT
!
REAL                   :: ZDZSDX   ! slope in X and Y direction
REAL                   :: ZDZSDY   ! of a triangle surface
REAL                   :: ZSLOPAZI ! azimuthal slope angle
REAL                   :: ZSLOPANG ! vertical slope angle
!
!-------------------------------------------------------------------------------
!
IIB= 1+JPHEXT
IIE=NNXLOC-JPHEXT
IJB=1+JPHEXT
IJE=NNYLOC-JPHEXT
!
PDIRSWDT(:,:,:,:)=0.
!
!-------------------------------------------------------------------------------
!
!*       1.    LOOP ON GRID MESHES
!              -------------------
!
!* discretization of the grid mesh in four triangles
!
DO JT=1,4
!
  DO JJ=IJB,IJE
    DO JI=IIB,IIE
!
!* If zenithal angle greater than Pi/2, sun is down.
!
      IF (PCOSZEN(JI,JJ)<0.) CYCLE
!
!-------------------------------------------------------------------------------
!
!*       2.    MODIFICATION OF RADIATION DUE TO LOCAL SLOPE
!              --------------------------------------------
!* modification of radiation received by 1 square meter of surface 
! (of the triangle) because of its orientation relative to the sun
!

! Modif Matthieu Lafaysse :
! threshold 0.001 on zenithal angle cosinus to avoid numerical problems at sunset and sunrise

      PDIRSWDT(JI,JJ,JT,:) = MAX( 0.0 , PDIRSRFSWD(JI,JJ,:) * ( &
         COS(PSLOPANG(JI,JJ,JT))                                          &
       + SIN(PSLOPANG(JI,JJ,JT)) * PSINZEN(JI,JJ) / MAX(PCOSZEN(JI,JJ),0.001)     &
         *  COS(PAZIMSOL(JI,JJ)-PSLOPAZI(JI,JJ,JT))))
!
!* normalizes received radiation by the surface of the triangle to obtain
!  radiation representative of an horizontal surface.
!
        PDIRSWDT(JI,JJ,JT,:) = PDIRSWDT(JI,JJ,JT,:) * PSURF_TRIANGLE(JI,JJ,JT)
!
!         DO JB=1,SIZE(PDIRSWDT(JI,JJ,JT,:))
!           IF (PDIRSWDT(JI,JJ,JT,JB)>15000) THEN
!             PRINT*,"warning >10000"
!             PRINT*,ASIN(PSINZEN(JI,JJ)),ACOS(PCOSZEN(JI,JJ)),ZSLOPANG,ZSLOPAZI,PAZIMSOL(JI,JJ),&
!             PDIRSRFSWD(JI,JJ,JB),PDIRSWDT(JI,JJ,JT,JB)
!           END IF
!         END DO
    END DO
  END DO
END DO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURF_SOLAR_SLOPES
