!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SLOPE_RADIATIVE_EFFECT(PTSTEP,PZENITH,PAZIM,PPS,PTA,PRAIN,PDIR_SW,PLW, &
                                  PZS,PZS_XY,PSLOPANG,PSLOPAZI,PSURF_TRIANGLE     )
!##############################################################
!
!!**** *SLOPE_RADIATIVE_EFFECT* compute direct short-wave radiation modified by slopes and shadows, 
!                               BUT renormalized on the horizontal surface of the grid mesh  
!                               to serve as input for ISBA
!
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!     Based on original package to correct shortwave radiation included in
!!     MESO-NH (developed by V. Masson)
!!
!!    AUTHOR
!!    ------
!!
!!    V. Vionnet        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    04/11
!!             29/04/11  : VV implementation of adjustment of incoming longwave 
!!                         radiation as a function of the slope (routine
!!                         originally implemented in operational chain SCM) 
!!                         (routine meteo.f90 of Crocus) 
!!                03/14  : M Lafaysse, modifs for optimization and parallelization
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODI_SURF_RAD_MODIF
!
USE MODD_CSTS,       ONLY : XPI,XSTEFAN
USE MODD_SLOPE_EFFECT
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL,                 INTENT(IN) :: PTSTEP   ! surface time step (s)
REAL, DIMENSION(:),   INTENT(IN) :: PZENITH  ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(:),   INTENT(IN) :: PAZIM    ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(:),   INTENT(IN) :: PPS      ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(:),   INTENT(IN) :: PTA      ! air temperature forcing               (K)
REAL, DIMENSION(:),   INTENT(IN) :: PRAIN    ! liquid precipitation                  (kg/m2/s)

! OROGRAPHY OF THE SUBDOMAIN (THIS PROCESSOR)
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS        ! (resolved) model orography
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS_XY     ! orography at vort. points
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPAZI   ! azimuthal slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSLOPANG   ! vertical slope angle of triangles
REAL, DIMENSION(:,:,:),     INTENT(IN) :: PSURF_TRIANGLE ! surface of triangles

REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDIR_SW  !   IN : input down (direct) short-wave radiation
!                                                   OUT : down (direct) short-wave radiation modified by slopes and shadows, 
!                                                        BUT renormalized on the horizontal surface of the grid mesh
REAL, DIMENSION(:),   INTENT(INOUT) :: PLW      !   IN : longwave radiation (on horizontal surf.)
!                                                   OUT : longwave radiation modified by slopee and valley effects, 
!                                                        BUT renormalized on the horizontal surface of the grid mesh
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JX       ! loop counter
INTEGER :: JY       ! loop counter
INTEGER :: JB       ! loop counter
INTEGER :: INBANDS   ! number of radiative bands
INTEGER :: IINDY

REAL, DIMENSION(:,:), ALLOCATABLE :: ZMAP   ! map factor
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOSZEN ! cosine of solar zenithal angle (=1 at zenith)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSINZEN ! sinus of solar zenithal angle (=0 at zenith)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZAZIMSOL ! solar azimuthal angle
!                                 ! 0     ==> Sun from the South
!                                 ! Pi/2  ==> Sun from the East
!                                 ! Pi    ==> Sun from the North
!                                 ! 3Pi/2 ==> Sun from the West
! REAL, DIMENSION(:,:), ALLOCATABLE :: ZSSO_SURF    ! ratio between sloping orography and horizontal surface
! REAL, DIMENSION(:), ALLOCATABLE :: ZSSO_SURF_1D   ! ratio between sloping orography and horizontal surface (1D vector)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDIRFLASWD ! input down (direct) short-wave radiation
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDIRSRFSWD ! down (direct) short-wave radiation modified by slopes and shadows, 
!                                      ! BUT renormalized on the horizontal surface of the grid mesh
!                                      ! For both variables, 3rd dimension is wavelength
REAL, DIMENSION(:), ALLOCATABLE :: ZALPHA ! intermediate variable 
!
REAL, PARAMETER :: VPRES1 = 87000.
!

INBANDS = SIZE(PDIR_SW,2)

!-------------------------------------------------------------------------------
!
!*    1.    Downwards solar radiation and solar angles
!     ------------------------------------------
!
! PRINT*,"SIZE(PTA)=",SIZE(PTA)

!
! solar zenithal angle    (cosine=1 : zenith)
ALLOCATE(ZCOSZEN(NNXLOC,NNYLOC))
ALLOCATE(ZSINZEN(NNXLOC,NNYLOC))
ALLOCATE(ZAZIMSOL(NNXLOC,NNYLOC))
ALLOCATE(ZMAP(NNXLOC,NNYLOC))
! ALLOCATE(ZSSO_SURF(NNX,NNY))
ALLOCATE(ZDIRFLASWD(NNXLOC,NNYLOC,INBANDS))
ALLOCATE(ZDIRSRFSWD(NNXLOC,NNYLOC,INBANDS))
! ALLOCATE(ZSSO_SURF_1D(IX*IY))
ALLOCATE(ZALPHA(NIX*NIY))

! PRINT*,SIZE(PZENITH)
! PRINT*,NNX,NNY,IX,IY

!PRINT*,NIYLOC,NIXLOC

IF (LREVERTGRID) THEN
  DO JY=1,NIYLOC
    IINDY=NIYLOC-JY+1
    DO JX=1,NIXLOC
      ZCOSZEN (JX+1,IINDY+1) = COS(PZENITH ( JX + (JY-1)*NIXLOC )) 
      ZSINZEN (JX+1,IINDY+1) = SIN(PZENITH ( JX + (JY-1)*NIXLOC )) 
      ZAZIMSOL (JX+1,IINDY+1) = PAZIM ( JX + (JY-1)*NIXLOC )      
    END DO
  END DO
ELSE
  DO JY=1,NIYLOC
    DO JX=1,NIXLOC
      ZCOSZEN (JX+1,JY+1) = COS(PZENITH ( JX + (JY-1)*NIXLOC )) 
      ZSINZEN (JX+1,JY+1) = SIN(PZENITH ( JX + (JY-1)*NIXLOC ))    
      ZAZIMSOL (JX+1,JY+1) = PAZIM ( JX + (JY-1)*NIXLOC )
    END DO
  END DO
ENDIF


ZCOSZEN(1,:) = ZCOSZEN(2,:)
ZCOSZEN(NNXLOC,:) = ZCOSZEN(NNXLOC-1,:)
ZCOSZEN(:,1) = ZCOSZEN(:,2)
ZCOSZEN(:,NNYLOC) = ZCOSZEN(:,NNYLOC-1)
ZSINZEN(1,:) = ZSINZEN(2,:)
ZSINZEN(NNXLOC,:) = ZSINZEN(NNXLOC-1,:)
ZSINZEN(:,1) = ZSINZEN(:,2)
ZSINZEN(:,NNYLOC) = ZSINZEN(:,NNYLOC-1)
ZAZIMSOL(1,:) = ZAZIMSOL(2,:)
ZAZIMSOL(NNXLOC,:) = ZAZIMSOL(NNXLOC-1,:)
ZAZIMSOL(:,1) = ZAZIMSOL(:,2)
ZAZIMSOL(:,NNYLOC) = ZAZIMSOL(:,NNYLOC-1)
!
! Downwards solar radiation
IF (LREVERTGRID) THEN
  DO JB = 1,INBANDS
    DO JY=1,NIYLOC
      IINDY=NIYLOC-JY+1
      DO JX=1,NIXLOC
        ZDIRFLASWD (JX+1,IINDY+1,JB) = PDIR_SW ( JX + (JY-1)*NIXLOC,JB)
      END DO
    END DO
  END DO
ELSE
  DO JB = 1,INBANDS
    DO JY=1,NIYLOC
      DO JX=1,NIXLOC
        ZDIRFLASWD (JX+1,JY+1,JB) = PDIR_SW ( JX + (JY-1)*NIXLOC,JB)
      END DO
    END DO
  END DO
ENDIF


ZDIRFLASWD(1,:,:) = ZDIRFLASWD(2,:,:)
ZDIRFLASWD(NNXLOC,:,:) = ZDIRFLASWD(NNXLOC-1,:,:)
ZDIRFLASWD(:,1,:) = ZDIRFLASWD(:,2,:)
ZDIRFLASWD(:,NNYLOC,:) = ZDIRFLASWD(:,NNYLOC-1,:)

!
!  Map factor
!
ZMAP(:,:) = 1.

!------------------------------------------------------------------------------------------
!
!*        2. Calls radiative computations
!     ----------------------------
!
! PRINT*,XZSL
! PRINT*,"in slope radiative effect",SIZE(XZSL,1),SIZE(XZSL,2)

CALL  SURF_RAD_MODIF ( ZMAP, XXHAT_THREAD, XYHAT_THREAD,                    &
                       ZCOSZEN, ZSINZEN, ZAZIMSOL,PZS,PZS_XY,               &
                       PSLOPANG,PSLOPAZI,PSURF_TRIANGLE,                    &
                       XXHAT,XYHAT,NINDX1_X,NINDX1_Y,XZSL,XZS_XY,           &
                       ZDIRFLASWD, ZDIRSRFSWD                    )
!
!
!-------------------------------------------------------------------------------
!
!*    3.     Output field comes back into 1D vector
!            --------------------------------------
!

IF (LREVERTGRID) THEN
  DO JB=1,INBANDS
    DO JY=1,NIYLOC
      IINDY=NIYLOC-JY+1
      DO JX=1,NIXLOC
        PDIR_SW ( JX + (JY-1)*NIXLOC,JB ) = ZDIRSRFSWD(JX+1,IINDY+1,JB)
!     ZSSO_SURF_1D(JX + (JY-1)*IX)  = ZSSO_SURF(JX+1,JY+1)
      END DO
    END DO
  END DO
ELSE
  DO JB=1,INBANDS
    DO JY=1,NIYLOC
      DO JX=1,NIXLOC
        PDIR_SW ( JX + (JY-1)*NIXLOC,JB ) = ZDIRSRFSWD(JX+1,JY+1,JB)
!     ZSSO_SURF_1D(JX + (JY-1)*IX)  = ZSSO_SURF(JX+1,JY+1)
      END DO
    END DO
  END DO
ENDIF
!
!
!
!-------------------------------------------------------------------------------
!
! !*    4.     Modify longwave incoming radiation due to account for the influence
! !            of opposite slope and correction in case of rain
! !            --------------------------------------------------------
! ZALPHA(:) = MAX(0.25*(ZSSO_SURF_1D(:)/SQRT(1+ZSSO_SURF_1D(:)**2.))**2., 0.1*PPS(:)/VPRES1)
! 
! PLW(:) = (1-ZALPHA(:))*PLW(:) +ZALPHA(:)*XSTEFAN*PTA(:)**4.
! 
! WHERE(PRAIN(:)*PTSTEP>0.001)
!         PLW(:) = MAX(PLW(:),0.95*XSTEFAN*PTA(:)**4.)
! END WHERE


DEALLOCATE(ZCOSZEN)
DEALLOCATE(ZSINZEN)
DEALLOCATE(ZAZIMSOL)
DEALLOCATE(ZMAP)
! DEALLOCATE(ZSSO_SURF)
DEALLOCATE(ZDIRFLASWD)
DEALLOCATE(ZDIRSRFSWD)
! DEALLOCATE(ZSSO_SURF_1D)
DEALLOCATE(ZALPHA)
END SUBROUTINE SLOPE_RADIATIVE_EFFECT
