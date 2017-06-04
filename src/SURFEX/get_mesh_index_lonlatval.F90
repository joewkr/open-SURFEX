!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLATVAL(KSSO,PGRID_PAR,PLAT,PLON,KINDEX,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_LONLATVAL* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    E. Martin         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2007  
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_LONLATVAL, ONLY : XXLIM, XYLIM, XX_MIN, XX_MAX, XY_MIN, &
                                      XY_MAX, XDX, XDY, XXLIMS, NXIDS, XDX_MAX,&
                                      NFRACD  
USE MODE_GRIDTYPE_LONLATVAL
!
USE MODD_POINT_OVERLAY
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
REAL,    DIMENSION(:),         INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(:),         INTENT(IN)    :: PLAT      ! latitude of the point
REAL,    DIMENSION(:),         INTENT(IN)    :: PLON      ! longitude of the point
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KINDEX    ! index of the grid mesh where the point is
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL  :: XLON0
REAL  :: ZVALX
!
REAL, DIMENSION(SIZE(PLON)) :: ZLON
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X Lambert   coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y Lambert   coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZXLIM       ! X Lambert   coordinate
!
INTEGER :: ISIZE, IFACT
INTEGER                           :: IL, ICPT       ! Grid dimension
INTEGER                           :: JL       ! loop counter in lambert grid
INTEGER                           :: JI, JJ       ! loop counter on input points
INTEGER, DIMENSION(SIZE(PLAT),2)  :: ICI
INTEGER, DIMENSION(1)             :: IDX0
!
LOGICAL, DIMENSION(SIZE(PLAT)) :: GMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!----------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_1',0,ZHOOK_HANDLE)
IF (.NOT. ALLOCATED(XXLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,KL=IL)
!
  IFACT = FLOOR(SQRT(FLOAT(IL)))
  ISIZE = FLOOR(FLOAT(IL) / IFACT)
  ALLOCATE(NFRACD(IFACT+1))
  NFRACD(1) = 1
  NFRACD(IFACT+1) = IL
  DO JJ=2,IFACT
    NFRACD(JJ) = 1 + (JJ-1)*ISIZE
  ENDDO  
!
  ALLOCATE(ZX (IL))
  ALLOCATE(ZY (IL))
  ALLOCATE(XDX(IL))
  ALLOCATE(XDY(IL))
  ALLOCATE(XXLIM(IL))
  ALLOCATE(XYLIM(IL))

  ALLOCATE(XXLIMS(0:IL))
  ALLOCATE(NXIDS(IL))

  ALLOCATE(ZXLIM(IL))   
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,PX=ZX,PY=ZY,PDX=XDX,PDY=XDY)
!
!*    2.     Limits of grid meshes in x and y
!            --------------------------------
!
  XXLIM(:)=ZX(:)-XDX(:)/2.
  XYLIM(:)=ZY(:)-XDY(:)/2.

  XX_MIN = MINVAL(XXLIM)
  XX_MAX = MAXVAL(XXLIM+XDX)
  XY_MIN = MINVAL(XYLIM)
  XY_MAX = MAXVAL(XYLIM+XDY)

  XDX_MAX = MINVAL(XDX)

  ZXLIM(:) = XXLIM(:)

  ZVALX = MAXVAL(ZXLIM) + 1.
  DO JI=1,IL
    IDX0 = MINLOC(ZXLIM) 
    XXLIMS(JI) = ZXLIM(IDX0(1))
    NXIDS(JI) = IDX0(1)
    ZXLIM(IDX0(1)) = ZVALX
  ENDDO
  XXLIMS(0) = XXLIMS(1) - XDX_MAX -1.

  DEALLOCATE(ZXLIM)  
  DEALLOCATE(ZX )
  DEALLOCATE(ZY )
  
END IF
!
XLON0 = 0.5*(XX_MIN+XX_MAX)
!
!*    3.     Projection
!            ----------
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR)
!
  ZLON(:) = PLON(:)+NINT((XLON0-PLON(:))/360.)*360.
!
GMASK(:) = .FALSE.
DO JL=1,SIZE(PLAT)
  IF (     ZLON(JL)<XX_MIN .OR. ZLON(JL)>XX_MAX  &
      .OR. PLAT(JL)<XY_MIN .OR. PLAT(JL)>XY_MAX ) GMASK(JL) = .TRUE.
ENDDO

!*    5.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
IFACT = SIZE(NFRACD) - 1
!
KINDEX(:,:)=0
!
KISSOX(:,:) = 0
KISSOY(:,:) = 0
!
ICI(:,:) = 0
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JL,JI,JJ)
DO JL=1,SIZE(PLAT)
  !
  IF (GMASK(JL)) CYCLE
  !
  frac: &
  DO JJ=IFACT,1,-1
    !
    IF (ZLON(JL)>XXLIMS(NFRACD(JJ))) THEN
      !
      DO JI = NFRACD(JJ+1),NFRACD(JJ),-1
        IF (ZLON(JL)>XXLIMS(JI)) THEN
          ICI(JL,2) = JI
          EXIT
        ENDIF
      ENDDO
      !
      DO JI = ICI(JL,2),0,-1
        IF (ZLON(JL)>=XXLIMS(JI)+XDX_MAX) THEN
          ICI(JL,1) = JI+1
          EXIT
        ENDIF
      ENDDO
      !
      EXIT frac
      !
    ENDIF 
    !
  ENDDO frac
  !
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_3',0,ZHOOK_HANDLE)
!
DO JL=1,SIZE(PLAT)
  !
  IF (GMASK(JL)) CYCLE
  !
  ICPT = 0
  DO JI=ICI(JL,1),ICI(JL,2)
    !
    IF (PLAT(JL)>XYLIM(NXIDS(JI)) .AND. PLAT(JL)<XYLIM(NXIDS(JI))+XDY(NXIDS(JI)) &
    .AND. ZLON(JL)<XXLIMS(JI)+XDX(NXIDS(JI))) THEN
      !
      ICPT = ICPT + 1
      !
      KINDEX(ICPT,JL) = NXIDS(JI)
      !
      IF (KSSO/=0) THEN
        KISSOX(ICPT,JL) = 1 + INT( FLOAT(KSSO) * (ZLON(JL)-XXLIM(NXIDS(JI)))/XDX(NXIDS(JI)) )   
        KISSOY(ICPT,JL) = 1 + INT( FLOAT(KSSO) * (PLAT(JL)-XYLIM(NXIDS(JI)))/XDY(NXIDS(JI)) ) 
      ENDIF     
      !
      IF (ICPT==NOVMX) EXIT
      !
    ENDIF 
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL_3',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLATVAL
