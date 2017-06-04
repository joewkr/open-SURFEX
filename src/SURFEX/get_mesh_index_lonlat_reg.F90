!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLAT_REG(KSSO,PGRID_PAR,PLAT,PLON,&
                        KINDEX,KISSOX,KISSOY,PVALUE,PNODATA)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_LONLAT_REG* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_GET_MESH_INDEX_LONLAT_REG, ONLY : XLONLIM, XLATLIM, NLAT, NLON, XLON0,&
                                           NFRACDLAT, NFRACDLON
USE MODE_GRIDTYPE_LONLAT_REG
!
USE MODD_POINT_OVERLAY, ONLY : NOVMX
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
REAL, DIMENSION(:), OPTIONAL, INTENT(IN)    :: PVALUE  ! value of the point to add
REAL, OPTIONAL, INTENT(IN) :: PNODATA
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                           :: JI       ! loop counter in x
INTEGER                           :: JJ       ! loop counter in y
INTEGER                           :: JL       ! loop counter on input points
!
REAL    :: ZLONMIN ! minimum longitude (degrees)
REAL    :: ZLONMAX ! maximum longitude (degrees)
REAL    :: ZLATMIN ! minimum latitude  (degrees)
REAL    :: ZLATMAX ! maximum latitude  (degrees)
REAL    :: ZDLON   ! longitude grid size
REAL    :: ZDLAT   ! latitude  grid size
!
REAL :: ZNODATA
!
REAL, DIMENSION(SIZE(PLAT))       :: ZVALUE
!
REAL, DIMENSION(SIZE(PLON)) :: ZLON
!
INTEGER, DIMENSION(SIZE(PLAT))    :: ICI, ICJ
!
INTEGER :: IFACTLON, ISIZELON, IFACTLAT, ISIZELAT, ISIZE_OMP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_1',0,ZHOOK_HANDLE)
!
IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
  ZVALUE(:) = PVALUE(:)
  ZNODATA = PNODATA
ELSE
  ZVALUE(:) = 1
  ZNODATA = 0
ENDIF
!
IF (.NOT. ALLOCATED(XLATLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX, &
                                 ZLATMIN,ZLATMAX,NLON,NLAT  ) 
!
!----------------------------------------------------------------------------
!
!*    2.     Limits of grid meshes
!            ---------------------
!
  ZDLON = (ZLONMAX-ZLONMIN) / FLOAT(NLON)
  ZDLAT = (ZLATMAX-ZLATMIN) / FLOAT(NLAT)
!
  ALLOCATE(XLONLIM(NLON+1))
  DO JI=1,NLON+1
    XLONLIM(JI) = ZLONMIN + FLOAT(JI-1)*ZDLON
  END DO

  ALLOCATE(XLATLIM(NLAT+1))
  DO JI=1,NLAT+1
    XLATLIM(JI) = ZLATMIN + FLOAT(JI-1)*ZDLAT
  END DO
  !
  XLON0 = 0.5*(ZLONMIN+ZLONMAX)
  !
  IFACTLON = FLOOR(SQRT(FLOAT(NLON+1))) + 1
  ISIZELON = FLOOR(FLOAT(NLON+1) / IFACTLON)
  ALLOCATE(NFRACDLON(IFACTLON+1))
  DO JJ=1,IFACTLON
    NFRACDLON(JJ) = 1 + (JJ-1) * ISIZELON
  ENDDO
  NFRACDLON(IFACTLON+1) = NLON+1
  !
  IFACTLAT = FLOOR(SQRT(FLOAT(NLAT+1))) + 1
  ISIZELAT = FLOOR(FLOAT(NLAT+1) / IFACTLAT)
  ALLOCATE(NFRACDLAT(IFACTLAT+1))
  DO JJ=1,IFACTLAT
    NFRACDLAT(JJ) = 1 + (JJ-1) * ISIZELAT
  ENDDO
  NFRACDLAT(IFACTLAT+1) = NLAT+1
  !
END IF
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_1',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
!
!*    3.     Reshifts the longitudes with respect to projection reference point
!            ------------------------------------------------------------------
!
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JL)
DO JL = 1,SIZE(PLON)
  IF (PLON(JL)>=XLON0+360.) THEN
    ZLON(JL) = PLON(JL) - 360.
  ELSEIF (PLON(JL)<=XLON0-360.) THEN
    ZLON(JL) = PLON(JL) + 360.
  ELSE
    ZLON(JL) = PLON(JL)
  ENDIF
ENDDO
!$OMP END DO 
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!ZLON(JL) = PLON(JL)+NINT((XLON0-PLON(JL))/360.)*360.
!
!----------------------------------------------------------------------------
!
!*    4.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2b',0,ZHOOK_HANDLE)
!
!IF (SIZE(PLAT)/=NLON*NLAT) THEN
!  KINDEX = 0
!  KISSOX = 0
!  KISSOY = 0
!END IF
!
IFACTLAT = SIZE(NFRACDLAT)-1
IFACTLON = SIZE(NFRACDLON)-1
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2b',1,ZHOOK_HANDLE)
!
ISIZE_OMP = MAX(1,SIZE(PLAT)/NBLOCKTOT)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(JL,JJ,JI)
DO JL=1,SIZE(PLAT)
  !
  ICI(JL) = 0
  ICJ(JL) = 0
  !
  IF (ZVALUE(JL)==ZNODATA) CYCLE
  !
  IF (  ZLON(JL)<XLONLIM(1) .OR. ZLON(JL)>=XLONLIM(NLON+1) &
   .OR. PLAT(JL)<XLATLIM(1) .OR. PLAT(JL)>=XLATLIM(NLAT+1) ) CYCLE
  !
  fraclat: &
  DO JJ = IFACTLAT,1,-1
    IF (PLAT(JL)>XLATLIM(NFRACDLAT(JJ))) THEN
      DO JI = NFRACDLAT(JJ+1),NFRACDLAT(JJ)+1,-1
        IF (PLAT(JL)>=XLATLIM(JI)) THEN
          ICJ(JL) = JI
          EXIT fraclat
        ENDIF
      ENDDO
      ICJ(JL) = NFRACDLAT(JJ)
      EXIT fraclat
    ENDIF
  ENDDO fraclat
  !
  fraclon: &
  DO JJ = IFACTLON,1,-1
    IF (ZLON(JL)>XLONLIM(NFRACDLON(JJ))) THEN
      DO JI = NFRACDLON(JJ+1),NFRACDLON(JJ)+1,-1
        IF (ZLON(JL)>=XLONLIM(JI)) THEN
          ICI(JL) = JI
          EXIT fraclon
        ENDIF
      ENDDO
      ICI(JL) = NFRACDLON(JJ)
      EXIT fraclon
    ENDIF
  ENDDO fraclon
  !  
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_3',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_4',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JL,JI,JJ)
DO JL=1,SIZE(PLAT)
  !
  IF (NOVMX>1) KINDEX(2:NOVMX,JL) = 0
  !
  IF (ZVALUE(JL)==ZNODATA) THEN
    !
    KINDEX(1,JL) = 0
    ! 
  ELSEIF ( ICI(JL)==0 .OR. ICJ(JL)==0 ) THEN    
    !
    KINDEX(1,JL) = 0
    !
    IF (KSSO/=0) THEN
      KISSOX(1,JL) = 0
      KISSOY(1,JL) = 0
    ENDIF
    !
  ELSE

    JI = ICI(JL)
    JJ = ICJ(JL)
    KINDEX(1,JL) = (JJ-1) * NLON + JI
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
    IF (KSSO/=0) THEN
      KISSOX(1,JL) = 1 + INT( FLOAT(KSSO) * (ZLON(JL)-XLONLIM(JI))/(XLONLIM(JI+1)-XLONLIM(JI)) )
      KISSOY(1,JL) = 1 + INT( FLOAT(KSSO) * (PLAT(JL)-XLATLIM(JJ))/(XLATLIM(JJ+1)-XLATLIM(JJ)) )
    END IF

  ENDIF

END DO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_4',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLAT_REG
