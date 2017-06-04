!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE BILIN_GRIDIN (PFIELD1,PFIELD_X,PFIELD_Y,PFIELD_XY)
!     #########################################################################
!
!!****  *BILIN_GRIDINEAR * - subroutine to interpolate surface FIELD
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!     Interpolation is bilinear, and uses 9 grid points, located in the
!!   center of model 1 grid mesh, and at the boundaries of this grid
!!   mesh (2 X limits, 2 Y limits and 4 corners).
!!     This implies that the grid mesh values located around the model 1
!!   grid mesh are not used directly. The values at the boundaries of the
!!   grid mesh are defined by the average between the middle point
!!   (this grid mesh value), and the one in the considered direction.
!!   So the eight grid meshes around the considered grid mesh are used
!!   equally.
!!     This is important to note that these average values are erased
!!   and replaced by zero if they are at the limit of any grid
!!   mesh with the zero value. This allows to insure zero value in model 2
!!   grid meshes where there was not the considered class in corresponding
!!   model 1 grid mesh, and to insure continuity of the FIELD type
!!   at such boundaries.
!!
!!
!!    The arrays and array index are defined on the following (model1) grid:
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j+1                    i,j+1                    i+1,j+1
!!
!!
!!
!!                   PFIELD_XY     PFIELD_Y    PFIELD_XY
!!                       *            *            *
!!                     i,j+1        i,j+1       i+1,j+1
!!
!!
!!
!!        XFIELD      PFIELD_X      XFIELD      PFIELD_X      XFIELD
!!          *            *            *            *            *
!!        i-1,j         i,j          i,j         i+1,j        i+1,j
!!
!!
!!
!!                    PFIELD_XY     PFIELD_Y    PFIELD_XY
!!                       *            *            *
!!                      i,j          i,j         i+1,j
!!
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j-1                    i,j-1                    i+1,j-1
!!
!!
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     01/2004
! TD&DD: added OpenMP directives

!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PFIELD1 ! FIELD on regular input grid
!
!*       0.2    Declarations of local variables for print on FM file
!
REAL, DIMENSION (:,:), INTENT(OUT) :: PFIELD_X ! FIELD at mesh interface
REAL, DIMENSION (:,:), INTENT(OUT) :: PFIELD_Y ! FIELD at mesh interface
REAL, DIMENSION (:,:), INTENT(OUT) :: PFIELD_XY! FIELD at mesh corner
!
REAL, DIMENSION(:,:),ALLOCATABLE :: ZW
INTEGER                              :: IIU       ! model 1 X size
INTEGER                              :: IJU       ! model 1 Y size
!
INTEGER                              :: JI        ! grid 1 x index
!
INTEGER                              :: JL        ! grid 2 index
!
REAL                                 :: ZEPS=1.E-3
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_1',0,ZHOOK_HANDLE)

IIU=SIZE(PFIELD1,1)
IJU=SIZE(PFIELD1,2)
!
ALLOCATE(ZW(IIU,IJU))
WHERE (PFIELD1/=XUNDEF)
  ZW=1.
ELSEWHERE
  ZW=0.
END WHERE
!
IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_1',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.     FIELD type at grid mesh interfaces (in X directions)
!               ----------------------------------
!
PFIELD_X(:,:) = 0.
!
PFIELD_X(1,:) = ZW(1,:) * PFIELD1(1,:)
PFIELD_X(IIU+1,:) = ZW(IIU,:) * PFIELD1(IIU,:)
!
IF (IIU>1) THEN
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JL)
  DO JL = 1,IJU
    DO JI = 2,IIU
      IF (PFIELD1(JI-1,JL)/=0. .AND. PFIELD1(JI,JL)/=0.) THEN
        PFIELD_X(JI,JL) = (ZW(JI-1,JL)*PFIELD1(JI-1,JL) + ZW(JI,JL)*PFIELD1(JI,JL)) / &
                  MAX(1.,(ZW(JI-1,JL)                  + ZW(JI,JL))              )
      ENDIF
    ENDDO
  ENDDO  
!$OMP END DO
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     FIELD type at grid mesh interfaces (in X directions)
!               ----------------------------------
!
PFIELD_Y(:,:) = 0.
!
PFIELD_Y(:,1) = ZW(:,1) * PFIELD1(:,1)
PFIELD_Y(:,IJU+1) = ZW(:,IJU) * PFIELD1(:,IJU)
!
IF (IJU>1) THEN
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JL)
  DO JL = 2,IJU
    DO JI = 1,IIU
      IF (PFIELD1(JI,JL-1)/=0. .AND. PFIELD1(JI,JL)/=0.) THEN
        PFIELD_Y(JI,JL) = (ZW(JI,JL-1)*PFIELD1(JI,JL-1) + ZW(JI,JL)*PFIELD1(JI,JL)) / &
                   MAX(1.,(ZW(JI,JL-1)                  + ZW(JI,JL))              )
      ENDIF
    ENDDO
  ENDDO
!$OMP END DO
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_3',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     FIELD type at grid mesh corners
!               -------------------------------
!
PFIELD_XY(:,:) = 0.
!
IF (IIU>1 .AND. IJU>1) THEN
  !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_4',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JL)
  DO JL = 2,IJU
    DO JI = 2,IIU
      IF (PFIELD1(JI-1,JL-1)/=0. .AND. PFIELD1(JI-1,JL)/=0. .AND. &
          PFIELD1(JI  ,JL-1)/=0. .AND. PFIELD1(JI  ,JL)/=0. ) THEN
        PFIELD_XY(JI,JL) = (ZW(JI-1,JL-1)*PFIELD1(JI-1,JL-1) + ZW(JI-1,JL)*PFIELD1(JI-1,JL) + &
                            ZW(JI,  JL-1)*PFIELD1(JI,  JL-1) + ZW(JI,  JL)*PFIELD1(JI,  JL))/&
                     MAX(1.,ZW(JI-1,JL-1) + ZW(JI-1,JL) + ZW(JI,JL-1) + ZW(JI,JL))
      ENDIF
    ENDDO
  ENDDO
!$OMP END DO
  IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_4',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_5',0,ZHOOK_HANDLE)
!
IF (IJU>1) THEN
  DO JL = 2,IJU
    IF (PFIELD1(1,  JL-1)/=0. .AND. PFIELD1(1,  JL)/=0.) THEN
      PFIELD_XY(1    ,JL) = (ZW(1,JL-1)*PFIELD1(1,JL-1) + ZW(1,JL)*PFIELD1(1,JL)) / &
                             MAX(1.,ZW(1,JL-1) + ZW(1,JL))
    ENDIF
    IF (PFIELD1(IIU,JL-1)/=0. .AND. PFIELD1(IIU,JL)/=0.) THEN
      PFIELD_XY(IIU+1,JL) = (ZW(IIU,JL-1)*PFIELD1(IIU,JL-1) + ZW(IIU,JL)*PFIELD1(IIU,JL)) / &
                             MAX(1.,ZW(IIU,JL-1) + ZW(IIU,JL))
    ENDIF 
  ENDDO
ENDIF 
!
IF (IIU>1) THEN
  DO JI = 2,IIU
    IF (PFIELD1(JI-1,1  )/=0. .AND. PFIELD1(JI,1   )/=0.) THEN
      PFIELD_XY(JI,1    ) = (ZW(JI-1,1)*PFIELD1(JI-1,1) + ZW(JI,1)*PFIELD1(JI,1)) / &
                             MAX(1.,ZW(JI-1,1) + ZW(JI,1))
    ENDIF
    IF (PFIELD1(JI-1,IJU)/=0. .AND. PFIELD1(JI,IJU)/=0.) THEN
      PFIELD_XY(JI,IJU+1) = (ZW(JI-1,IJU)*PFIELD1(JI-1,IJU) + ZW(JI,IJU)*PFIELD1(JI,IJU)) / &
                             MAX(1.,ZW(JI-1,IJU) + ZW(JI,IJU))
    ENDIF
  ENDDO
ENDIF 
!
PFIELD_XY(1    ,1    ) = PFIELD1(1  ,1  )
PFIELD_XY(IIU+1,1    ) = PFIELD1(IIU,1  )
PFIELD_XY(1    ,IJU+1) = PFIELD1(1  ,IJU)
PFIELD_XY(IIU+1,IJU+1) = PFIELD1(IIU,IJU)
!
IF (LHOOK) CALL DR_HOOK('BILIN_GRIDIN_5',1,ZHOOK_HANDLE)
!
DEALLOCATE(ZW)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE BILIN_GRIDIN
