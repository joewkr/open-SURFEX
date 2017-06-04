!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SSO (U, UG, USS, OSSO, OSSO_ANIS)
!     #########################
!
!!*SSO  computes the SSO anisotropy, direction and slope
!!
!!
!!    METHOD
!!    ------
!!    See Lott and Miller, 1997, QJRMS 101-127
!!   
!!    AUTHOR
!!    ------
!!
!!    V.Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    23/05/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODI_GET_MESH_DIM
USE MODI_GET_ADJACENT_MESHES
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_READ_AND_SEND_MPI
!
USE MODD_CSTS,           ONLY : XPI
USE MODD_PGDWORK,        ONLY : NSSO, XSSQO, LSSQO
USE MODD_PGD_GRID,       ONLY : NL, CGRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SSO_t), INTENT(INOUT) :: USS
!
LOGICAL, DIMENSION(:), INTENT(OUT) :: OSSO   ! .T. : the SSO coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
LOGICAL, DIMENSION(:), INTENT(OUT) :: OSSO_ANIS ! .T. : the SSO anisotropy
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
!
!*    0.2    Declaration of indexes
!            ----------------------
!
INTEGER :: JS1, JS2
INTEGER :: JL          ! loop index on grid meshs
INTEGER :: IL          ! grid mesh index of second subgrid point used
INTEGER :: JISS, JJSS  ! loop indexes for subsquares arrays
INTEGER :: JNEXT       ! loop index on subgrid meshes
INTEGER :: JPREV       ! loop index on subgrid meshes
INTEGER :: INEXT       ! index to add to JISS or JJSS to obtain the following
!                      ! point containing a data in a segment
INTEGER :: IPREV       ! index to remove from JISS or JJSS to obtain the
!                      ! previous point containing a data in a segment
!
INTEGER :: IMAXI       ! index of the next subsquare with data along I axis,
                       ! or last subsquare inside grid mesh along I axis
INTEGER :: IMAXJ       ! index of the next subsquare with data along J axis,
                       ! or last subsquare inside grid mesh along J axis
INTEGER, DIMENSION(:), ALLOCATABLE :: ILEFT, IRIGHT, ITOP, IBOTTOM
INTEGER :: ICOUNT
!
!*    0.3    Declaration of working arrays inside a MESONH grid (JI,JJ)
!            -----------------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISSO, ISSOT
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ISSQOT, ISSQO
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZSSQO
LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: GSSQO
LOGICAL, DIMENSION(:), ALLOCATABLE :: GSSO, GSSO_ANIS
!
REAL,    DIMENSION(NSSO,NSSO) :: ZDZSDX  ! topographic gradient along x
REAL,    DIMENSION(NSSO,NSSO) :: ZDZSDY  ! topographic gradient along y
LOGICAL, DIMENSION(NSSO,NSSO) :: GDZSDX  ! occurence of gradient along x
LOGICAL, DIMENSION(NSSO,NSSO) :: GDZSDY  ! occurence of gradient along y
REAL :: ZDXEFF    ! width of a subsquare along I axis
REAL :: ZDYEFF    ! width of a subsquare along J axis
LOGICAL :: GBOUND ! .T.: first left (for dzs/dx) or first bottom (for dzs/dy)
                  ! sub-square gradients is being computed
!
!*    0.4    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZDX, ZDY, ZSEA, ZMESH_SIZE
REAL, DIMENSION(:), ALLOCATABLE :: ZHXX0, ZHXY0, ZHYY0
REAL, DIMENSION(NL) :: ZHXX       ! topographic gradient correlation tensor: x,x
REAL, DIMENSION(NL) :: ZHXY       ! topographic gradient correlation tensor: x,y
REAL, DIMENSION(NL) :: ZHYY       ! topographic gradient correlation tensor: y,y
REAL, DIMENSION(NL) :: ZK, ZL, ZM ! diagonalised terms of the tensor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
IF (LHOOK) CALL DR_HOOK('SSO',0,ZHOOK_HANDLE)
!
!*    1.1    Occurence of computation of the coefficients
!            --------------------------------------------
!
OSSO     (:)=.FALSE.
OSSO_ANIS(:)=.FALSE.
!
ZHYY(:) = 0.
ZHXX(:) = 0.
ZHXY(:) = 0.
!
!*    1.2    Grid dimension (meters)
!            -----------------------
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZSEA      (U%NDIM_FULL))
  ALLOCATE(ZMESH_SIZE(U%NDIM_FULL))
  ALLOCATE(ZSSQO (U%NDIM_FULL,NSSO,NSSO))
  ALLOCATE(ISSQOT(U%NDIM_FULL,NSSO,NSSO))
ELSE
  ALLOCATE(ZSEA      (0))
  ALLOCATE(ZSSQO (0,0,0))
  ALLOCATE(ISSQOT(0,0,0))
ENDIF
!
 CALL GATHER_AND_WRITE_MPI(U%XSEA,ZSEA)
 CALL GATHER_AND_WRITE_MPI(UG%G%XMESH_SIZE,ZMESH_SIZE) 
 CALL GATHER_AND_WRITE_MPI(XSSQO,ZSSQO)
!
ALLOCATE(ISSQO(SIZE(XSSQO,1),NSSO,NSSO))
IF (SIZE(ISSQO)/=0) THEN
  ISSQO(:,:,:) = 0
  WHERE (LSSQO(:,:,:)) ISSQO(:,:,:) = 1
ENDIF
 CALL GATHER_AND_WRITE_MPI(ISSQO,ISSQOT)
DEALLOCATE(ISSQO)
!
IF (NRANK==NPIO) THEN
  !
  ALLOCATE(ZDX(U%NDIM_FULL),ZDY(U%NDIM_FULL))
  CALL GET_MESH_DIM(CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,ZDX,ZDY,ZMESH_SIZE)
  DEALLOCATE(ZMESH_SIZE)

  !
  !
  !*    1.3    Left, top, right and bottom adjacent gris meshes
  !            ------------------------------------------------
  !
  ALLOCATE(ILEFT(U%NDIM_FULL),IRIGHT(U%NDIM_FULL),ITOP(U%NDIM_FULL),IBOTTOM(U%NDIM_FULL))
  CALL GET_ADJACENT_MESHES(CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,&
                           ILEFT,IRIGHT,ITOP,IBOTTOM)
  !
  ALLOCATE(GSSO(U%NDIM_FULL),GSSO_ANIS(U%NDIM_FULL))
  ALLOCATE(ZHXX0(U%NDIM_FULL),ZHXY0(U%NDIM_FULL),ZHYY0(U%NDIM_FULL))
  GSSO     (:) = .FALSE.
  GSSO_ANIS(:) = .FALSE.
  ZHYY0(:) = 0.
  ZHXX0(:) = 0.
  ZHXY0(:) = 0.
  !
  !
  !*    2.     Loop on MESONH grid points
  !            --------------------------
  !
  !
  !done on the whole grid (needed because of use of near points)
  DO JL=1,U%NDIM_FULL
    !
    !
    !*    2.1    No land in grid mesh
    !            --------------------
    !
    IF (ZSEA(JL)==1.) CYCLE
    !
    ZDXEFF=ZDX(JL)/FLOAT(NSSO)
    ZDYEFF=ZDY(JL)/FLOAT(NSSO)
    !
    !*    2.3    Not enough data for computation
    !            -------------------------------
    !
    ! 1st step: removes points where orography data is not present at all.
    !
    IF ( COUNT( ISSQOT(JL,:,:)==1) == 0  ) CYCLE
    !
    !----------------------------------------------------------------------------
    !
    !*    3.     Computations of the gradients along x
    !            -------------------------------------
    !
    GDZSDX (:,:)=.FALSE.
    ZDZSDX (:,:)= 0.
    !
    !*    3.1    loop on jss index (there is no specific computation along j)
    !            -----------------
    !
    DO JJSS=1,NSSO
      !
      !* left point mark initialization
      !
      GBOUND=.TRUE.
      !
      !*    3.2    loop on iss index
      !            -----------------
      !
      DO JISS=1,NSSO
        !
        !
        !*    3.3    search for two consecutive grid points
        !            --------------------------------------
        !
        !*    3.3.1 first one
        !
        IF ( ISSQOT(JL,JISS,JJSS)==0 ) CYCLE
        !
        !*    3.3.2  second one (up to one grid mesh further)
        !
        DO JNEXT=1,NSSO
          IF (JISS+JNEXT>NSSO) THEN
            IL    = IRIGHT(JL)
            INEXT = JISS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JISS+JNEXT
          END IF
          ! no right point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (ISSQOT(IL,INEXT,JJSS)==1) EXIT
        END DO
        !
        !*    3.3.3  none found: end of loop along jss
        !                        ---------------------
        !
        IF (JNEXT>=NSSO+1) EXIT
        !
        !*    3.3.4  second point outside of the domain: end of loop along iss
        !                                                ---------------------
        !
        IF (IL==0) EXIT
        !
        !*    3.4    dzs/dx term
        !            -----------
        !
        IMAXI = MIN(JISS+JNEXT-1,NSSO)
        !
        ZDZSDX(JISS:IMAXI,JJSS) = ( ZSSQO(IL,INEXT,JJSS) - ZSSQO(JL,JISS,JJSS)) &
                                    / FLOAT(JNEXT) / ZDXEFF
        !
        GDZSDX(JISS:IMAXI,JJSS) = .TRUE.
        !
        !
        !*    3.5    left data point not on the left of the grid mesh (one more computation)
        !            ------------------------------------------------
        !
        IF (GBOUND .AND. JISS/=1) THEN
          DO JPREV=1,NSSO
            IF (JISS-JPREV<1) THEN
              IL    = ILEFT(JL)
              IPREV = JISS-JPREV+NSSO
            ELSE
              IL    = JL
              IPREV = JISS-JPREV
            END IF
            ! no left point
            IF (IL==0) EXIT
            ! subgrid data found
            IF (ISSQOT(IL,IPREV,JJSS)==1) EXIT
          END DO

          IF (.NOT. (JPREV>=NSSO+1 .OR. IL==0)) THEN
            ZDZSDX(1:JISS,JJSS) = ( ZSSQO(JL,JISS,JJSS) - ZSSQO(IL,IPREV,JJSS)) &
                                    / FLOAT(JPREV) / ZDXEFF  
            !
            GDZSDX(1:JISS,JJSS) = .TRUE.
          END IF
        END IF
        !
        !
        GBOUND=.FALSE.
        !
        !
        !*    3.6    end of loop on iss index
        !            ------------------------
        !
      END DO
      !
      !*    3.7    end of loop on jss index
      !            ------------------------
      !
    END DO
    !
    !----------------------------------------------------------------------------
    !        
    !*    4.     Computations of the gradients along y
    !            -------------------------------------
    !
    GDZSDY (:,:)=.FALSE.
    ZDZSDY (:,:)= 0.
    !
    !*    4.1    loop on iss index (there is no specific computation along i)
    !            -----------------
    !
    DO JISS=1,NSSO
!
!* bottom point mark initialization
!
      GBOUND=.TRUE.
!
!*    4.2    loop on jss index
!            -----------------
!
      DO JJSS=1,NSSO
!
!
!*    4.3    search for two consecutive grid points
!            --------------------------------------
!
!*    4.3.1 first one
!
        IF (ISSQOT(JL,JISS,JJSS)==0 ) CYCLE
!
!*    4.3.2  second one (up to one grid mesh further)
!
        DO JNEXT=1,NSSO
          IF (JJSS+JNEXT>NSSO) THEN
            IL    = ITOP(JL)
            INEXT = JJSS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JJSS+JNEXT
          END IF
          ! no top point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (ISSQOT(IL,JISS,INEXT)==1) EXIT
        END DO
!
!*    4.3.3  none found: end of loop along iss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    3.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!*    4.4    dzs/dy term
!            -----------
!
        IMAXJ = MIN(JJSS+JNEXT-1,NSSO)
!
        ZDZSDY(JISS,JJSS:IMAXJ) = ( ZSSQO(IL,JISS,INEXT) - ZSSQO(JL,JISS,JJSS)) &
                                    / FLOAT(JNEXT) / ZDYEFF 
       !
        GDZSDY(JISS,JJSS:IMAXJ) = .TRUE.
!
!
!*    4.5    bottom data point not on the bottom of the grid mesh (one more computation)
!            ----------------------------------------------------
!
        IF (GBOUND .AND. JJSS/=1) THEN
          DO JPREV=1,NSSO
            IF (JJSS-JPREV<1) THEN
              IL    = IBOTTOM(JL)
              IPREV = JJSS-JPREV+NSSO
            ELSE
              IL    = JL
              IPREV = JJSS-JPREV
            END IF
            ! no left point
             IF (IL==0) EXIT
            ! subgrid data found
            IF (ISSQOT(IL,JISS,IPREV)==1) EXIT
          END DO

          IF (.NOT. (JPREV>=NSSO+1 .OR. IL==0)) THEN
            ZDZSDY(JISS,1:JJSS) = ( ZSSQO(JL,JISS,JJSS) - ZSSQO(IL,JISS,IPREV)) &
                                    / FLOAT(JPREV) / ZDYEFF 
!
            GDZSDY(JISS,1:JJSS) = .TRUE.
          END IF
        END IF
!
!
        GBOUND=.FALSE.
!
        !
        !
        !*    4.6   end of loop on jss index
        !            ------------------------
        !
      END DO
      !
      !*    4.7    end of loop on iss index
      !            ------------------------
      !
    END DO
    !----------------------------------------------------------------------------
    !
    !*    5.     Computations of tensor terms
    !            ----------------------------
    !
    !
    !*    5.1    test to know if term Hxy is computable
    !            --------------------------------------
    !
    ICOUNT =  COUNT(GDZSDX(:,:).AND.GDZSDY(:,:))
    !* 2 values are necessary in the grid point to compute anisotropy
    !
    IF ( ICOUNT==0 ) CYCLE
    !
    !
    !*    5.2    SSO quantities are computable
    !            -----------------------------
    !
    GSSO(JL)=.TRUE.
    GSSO_ANIS(JL)=ICOUNT>1
    !
    !
    !*    5.3    term Hxx
    !            --------
    !
    ZHXX0(JL) = SUM(ZDZSDX(:,:)*ZDZSDX(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))/ICOUNT  
    !
    !*    5.4    term Hyy
    !            --------
    !
    ZHYY0(JL) = SUM(ZDZSDY(:,:)*ZDZSDY(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))/ICOUNT 
    !
    !*    5.5    term Hxy
    !            --------
    !
    ZHXY0(JL) = SUM(ZDZSDX(:,:)*ZDZSDY(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))/ICOUNT  
    !
    !-------------------------------------------------------------------------------
    !
    !*    6.     Next MESONH grid point
    !            ----------------------
    !
  END DO    
  !
ELSE
  !
  ALLOCATE(ZHXX0(0),ZHYY0(0),ZHXY0(0))
  !
ENDIF    
!
DEALLOCATE(ZSSQO,ISSQOT)
DEALLOCATE(ZSEA)
!
!each contrib sent to each task
 CALL READ_AND_SEND_MPI(ZHXX0,ZHXX)
 CALL READ_AND_SEND_MPI(ZHYY0,ZHYY)
 CALL READ_AND_SEND_MPI(ZHXY0,ZHXY)
DEALLOCATE(ZHXX0,ZHYY0,ZHXY0)
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ISSOT(U%NDIM_FULL))
  ISSOT(:) = 0
  WHERE (GSSO(:)) ISSOT(:) = 1
  DEALLOCATE(GSSO)
ELSE
  ALLOCATE(ISSOT(0))
ENDIF
ALLOCATE(ISSO(NL))
 CALL READ_AND_SEND_MPI(ISSOT,ISSO)
WHERE(ISSO(:)==1) OSSO(:) = .TRUE.
!
IF (NRANK==NPIO) THEN
  ISSOT(:) = 0
  WHERE (GSSO_ANIS(:)) ISSOT(:) = 1
  DEALLOCATE(GSSO_ANIS)
ENDIF
 CALL READ_AND_SEND_MPI(ISSOT,ISSO)
WHERE(ISSO(:)==1) OSSO_ANIS(:) = .TRUE.
DEALLOCATE(ISSO)
!
DEALLOCATE(ISSOT)
!
!----------------------------------------------------------------------------
!
!*    7.     Diagonalization of the tensor
!     -----------------------------
!
ZK=0.
ZL=0.
ZM=0.
!
WHERE (OSSO(:))
  ZK(:)=0.5*(ZHXX(:)+ZHYY(:))
  ZL(:)=0.5*(ZHXX(:)-ZHYY(:))
  ZM(:)=     ZHXY(:)
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    8.     S.S.O. characteristics
!            ----------------------
!
!*    8.1    S.S.O. direction of main axis
!            -----------------------------
!
WHERE (OSSO(:) .AND. ZL(:)>1.E-30 )
  USS%XSSO_DIR(:) = 0.5* ATAN(ZM/ZL) * (180./XPI)
END WHERE
!
WHERE (OSSO(:) .AND. ZL(:)<-1.E-30 )
  USS%XSSO_DIR(:) = 0.5* ATAN(ZM/ZL) * (180./XPI) + 90.
END WHERE
!
WHERE (OSSO(:) .AND. ABS(ZL(:))<=1.E-30 )
  USS%XSSO_DIR(:) = 45.
END WHERE
!
WHERE (OSSO(:) .AND. USS%XSSO_DIR(:)>90. )
  USS%XSSO_DIR(:) = USS%XSSO_DIR(:) - 180.
END WHERE
!
!*    8.2    S.S.O. slope
!            ------------
!
WHERE (OSSO(:))
  USS%XSSO_SLOPE(:) = SQRT( ZK+SQRT(ZL*ZL+ZM*ZM) )
END WHERE
!
!*    8.3    S.S.O. anisotropy
!            -----------------
!
WHERE (OSSO_ANIS(:) .AND. (ZK+SQRT(ZL*ZL+ZM*ZM)) >0. )
  USS%XSSO_ANIS(:)=SQRT( MAX(ZK-SQRT(ZL*ZL+ZM*ZM),0.) / (ZK+SQRT(ZL*ZL+ZM*ZM)))
END WHERE
!
WHERE (OSSO_ANIS(:) .AND. (ZK+SQRT(ZL*ZL+ZM*ZM))==0. )
  USS%XSSO_ANIS(:)=1.
END WHERE
IF (LHOOK) CALL DR_HOOK('SSO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SSO
