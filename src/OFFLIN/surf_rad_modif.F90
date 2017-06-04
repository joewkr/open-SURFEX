!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SURF_RAD_MODIF (PMAP,PXHAT,PYHAT,PCOSZEN,PSINZEN,PAZIMSOL,  &
                           PZS,PZS_XY,PSLOPANG,PSLOPAZI,PSURF_TRIANGLE,&
                           ZXHAT_ll,ZYHAT_ll,IIOR_ll,IJOR_ll,ZZS_ll,   &
                           ZZS_XY_ll,PDIRFLASWD, PDIRSRFSWD            )
!###################################################################
!
!!****  * SURF_RAD_MODIF * - computes the modifications to the downwards
!!                           radiative fluxes at the surface, due to
!!                           orientation and shape of this surface.
!!
!!    PURPOSE
!!    -------
!!
!!    1) defines a continuous shape of the orography using triangles
!!       (SURF_SOLAR_GEOM)
!!
!!    2) modification of direct SW downwards flux due to the
!!       slope and orientation of the surface (SURF_SOLAR_SLOPES).
!!       The surface characteristics are compared to the azimuthal 
!!       and zenithal solar angles.
!!
!!    3) modification of direct SW by shadowing from other grid points orography.
!!
!!    4) A procedure is added to insure energy conservation after these modifications.
!!
!!       Only the RESOLVED orography is taken into account for these (4) effects.
!!       Therefore, these modifications will have an impact only for fine
!!       resolutions (large resolved slopes).
!!
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
!!      V. Masson        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/02/00
!!      V. Masson   28/02/00 extract the surface modifications of the
!!                           RADIATIONS routine, and add the subgrid solar
!!                           computations and the resolved shadows.
!!      V. Masson   18/02/02 rewrites the routine to add shadows from
!!                           one grid point to another
!!      V. Masson   03/03/03 moves local computations to surface schemes
!!                           and add multiple SW wavelengths
!!
!!                03/14  : M Lafaysse, modifs for optimization and parallelization
!!                         + comment spatial energy conservation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!USE MODI_SURF_SOLAR_SUM
USE MODI_SURF_SOLAR_SLOPES
USE MODI_SURF_SOLAR_SHADOWS
!
!
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PMAP       ! map factor
REAL, DIMENSION(:),       INTENT(IN) :: PXHAT      ! X coordinate
REAL, DIMENSION(:),       INTENT(IN) :: PYHAT      ! Y coordinate
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN    ! COS(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PSINZEN    ! SIN(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PAZIMSOL   ! azimuthal solar angle
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS        ! (resolved) model orography
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS_XY     ! orography at vort. points
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPAZI   ! azimuthal slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPANG   ! vertical slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSURF_TRIANGLE ! surface of triangles
REAL, DIMENSION(:),       INTENT(IN) :: ZXHAT_ll   ! X coordinate (all processors)
REAL, DIMENSION(:),       INTENT(IN) :: ZYHAT_ll   ! Y coordinate (all processors)
INTEGER,                  INTENT(IN) :: IIOR_ll    ! position of SW corner of current processor domain
!                                                  ! in the entire domain (I index along X coordinate) 
!                                                  ! (both including the 1 point border)
INTEGER,                  INTENT(IN) :: IJOR_ll    ! position of SW corner of current processor domain
!                                                  ! in the entire domain (J index along Y coordinate)
!                                                  ! (both including the 1 point border)
REAL, DIMENSION(:,:),     INTENT(IN) :: ZZS_ll     ! orography at center of grid meshes
!                                                  ! (all processors)
REAL, DIMENSION(:,:),     INTENT(IN) :: ZZS_XY_ll  ! orography at SW corner of grid meshes
                                                   ! (all processors)
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIRFLASWD ! Downward DIR SW Flux on flat surf
REAL, DIMENSION(:,:,:),   INTENT(OUT):: PDIRSRFSWD ! Downward SuRF. DIRect    SW Flux

!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!
REAL, DIMENSION(SIZE(PZS,1),SIZE(PZS,2),SIZE(PDIRFLASWD,3)) :: ZDIRSWD 
                                                      ! down SW on grid mesh
REAL, DIMENSION(SIZE(PZS,1),SIZE(PZS,2),4,SIZE(PDIRFLASWD,3)) :: ZDIRSWDT
!                                                     ! down SW on triangles
!                                                     ! (4 per grid mesh)
!
!REAL, DIMENSION(SIZE(PDIRFLASWD,3)) :: ZENERGY1 
! energy received by the surface by direct solar radiation
!REAL, DIMENSION(SIZE(PDIRFLASWD,3)) :: ZENERGY2
! before and after modification of radiation by terrain slopes
!REAL, DIMENSION(SIZE(PDIRFLASWD,3)) :: ZENERGYP
! idem except taking into account only positive variations of energy
!
INTEGER :: ISWB  ! number of SW spectral bands
INTEGER :: JSWB  ! loop on SW spectral bands
!-------------------------------------------------------------------------------
!
!* initializations
!
ISWB = SIZE(PDIRFLASWD,3)
!
!-------------------------------------------------------------------------------
!
! DO JSWB = 1, ISWB
!   CALL SURF_SOLAR_SUM     (PXHAT, PYHAT, PMAP, PDIRFLASWD(:,:,JSWB), ZENERGY1(JSWB) )
! END DO
!
!
!*       2.    Slope direction direct SW effects
!              ---------------------------------
!

CALL SURF_SOLAR_SLOPES  ( PCOSZEN, PSINZEN, PAZIMSOL,                    &
                          PSLOPANG,PSLOPAZI,PSURF_TRIANGLE,&
                          PDIRFLASWD, ZDIRSWDT   )


!
!*       3.    RESOLVED shadows for direct solar radiation
!              -------------------------------------------

CALL SURF_SOLAR_SHADOWS (PMAP, PXHAT, PYHAT, PCOSZEN, PSINZEN, PAZIMSOL, PZS, PZS_XY,&
                     ZXHAT_ll,ZYHAT_ll,IIOR_ll,IJOR_ll,ZZS_ll,ZZS_XY_ll,         &
                     ZDIRSWDT, ZDIRSWD                                           )

!
!
!*       4.    Energy conservation
!              -------------------
!
!! M Lafaysse : comment the spatial energy conservation
PDIRSRFSWD(:,:,:) = ZDIRSWD(:,:,:)



! DO JSWB = 1, ISWB
!   CALL SURF_SOLAR_SUM(PXHAT, PYHAT, PMAP,  &
!                       ZDIRSWD(:,:,JSWB),   &
!                       ZENERGY2(JSWB)       )
!   !
!   CALL SURF_SOLAR_SUM(PXHAT, PYHAT, PMAP,                             &
!                       MAX(ZDIRSWD(:,:,JSWB)-PDIRFLASWD(:,:,JSWB),0.), &
!                       ZENERGYP(JSWB)                                  )
!   !
!   IF (ZENERGYP(JSWB)>0.) THEN
!     PDIRSRFSWD(:,:,JSWB) = ZDIRSWD(:,:,JSWB)                              &
!                          + (ZENERGY1(JSWB)-ZENERGY2(JSWB))/ZENERGYP(JSWB) &
!                           * MAX(ZDIRSWD(:,:,JSWB)-PDIRFLASWD(:,:,JSWB),0.)
!   ELSE
!     PDIRSRFSWD(:,:,JSWB) = PDIRFLASWD(:,:,JSWB)
!   END IF
! END DO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURF_RAD_MODIF
