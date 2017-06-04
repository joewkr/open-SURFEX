!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_CONF_PROJ(KSSO,PGRID_PAR,PLAT,PLON,KINDEX,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_CONF_PROJ* get the grid mesh where point (lat,lon) is located
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
!!    J.Escobar  22/10/2011 : reintroduce optimisation for JI/JJ number of lines computation
!!     J. Escobar  06/2013  : modif for REAL*4
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_CONF_PROJ, ONLY : XLAT0, XLON0, XRPK, XBETA,    &
                                            XLATOR, XLONOR, NIMAX, NJMAX, &
                                            XXLIM, XYLIM  
USE MODE_GRIDTYPE_CONF_PROJ
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODD_CSTS ,ONLY : XSURF_EPSILON
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
!
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
REAL, DIMENSION(SIZE(PLON))       :: ZLON     ! longitude
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X conformal coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y conformal coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX      ! X grid mesh size
REAL, DIMENSION(:), ALLOCATABLE   :: ZDY      ! Y grid mesh size
REAL                              :: ZDXLIM   ! X grid mesh size
REAL                              :: ZDYLIM   ! Y grid mesh size
!
INTEGER                           :: JI       ! loop counter in x
INTEGER                           :: JJ       ! loop counter in y
INTEGER                           :: JL       ! loop counter on input points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_CONF_PROJ',0,ZHOOK_HANDLE)
IF (.NOT. ALLOCATED(XXLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,XLAT0,XLON0,XRPK,XBETA,&
                                XLATOR,XLONOR,NIMAX,NJMAX        )  
!
  ALLOCATE(ZX (NIMAX*NJMAX))
  ALLOCATE(ZY (NIMAX*NJMAX))
  ALLOCATE(ZDX(NIMAX*NJMAX))
  ALLOCATE(ZDY(NIMAX*NJMAX))
!
  CALL GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,                       &
                                PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY      )  
!
!*    2.     Limits of grid meshes in x and y
!            --------------------------------
!
  ALLOCATE(XXLIM(NIMAX+1))
  XXLIM(1) = ZX(1) - ZDX(1)/2.
  DO JI=2,NIMAX
    JL = JI
    XXLIM(JI) = ZX(JL) - (ZDX(JL-1)+ZDX(JL))/4.
  END DO
  XXLIM(NIMAX+1) = ZX(NIMAX) + ZDX(NIMAX)/2.

  ALLOCATE(XYLIM(NJMAX+1))
  XYLIM(1) = ZY(1) - ZDY(1)/2.
  DO JJ=2,NJMAX
    JL = 1 + (JJ-1) * NIMAX
    XYLIM(JJ) = ZY(JL) - (ZDY(JL-NIMAX)+ZDY(JL))/4.
  END DO
  XYLIM(NJMAX+1) = ZY(1+(NJMAX-1)*NIMAX) + ZDY(1+(NJMAX-1)*NIMAX)/2.
!
!
  DEALLOCATE(ZX )
  DEALLOCATE(ZY )
  DEALLOCATE(ZDX)
  DEALLOCATE(ZDY)
END IF
!
ZDXLIM = XXLIM(2) - XXLIM(1)
ZDYLIM = XYLIM(2) - XYLIM(1)
!
!*    2.     Reshifts the longitudes with respect to projection reference point
!            ------------------------------------------------------------------
!
ZLON(:) = PLON(:)+NINT((XLON0-PLON(:)+180.0*XSURF_EPSILON)/360.)*360.
!
!*    3.     Projection
!            ----------
!
ALLOCATE(ZX (SIZE(PLAT)))
ALLOCATE(ZY (SIZE(PLAT)))
!
 CALL XY_CONF_PROJ(XLAT0,XLON0,XRPK,XBETA,XLATOR,XLONOR, &
                    ZX,ZY,PLAT,ZLON                       )  
!
!
!
!*    5.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
KINDEX(:,:) = 0
!
DO JL=1,SIZE(PLON)
  IF (     ZX(JL)<XXLIM(1) .OR. ZX(JL)>=XXLIM(NIMAX+1) &
        .OR. ZY(JL)<XYLIM(1) .OR. ZY(JL)>=XYLIM(NJMAX+1) ) THEN  
    KINDEX(1,JL) = 0
    IF (KSSO/=0) THEN
      KISSOX(1,JL) = 0
      KISSOY(1,JL) = 0
    END IF
    CYCLE
  END IF
  JI = MIN(INT( (ZX(JL) - XXLIM(1))/ZDXLIM+1),NIMAX)
  JJ = MIN(INT( (ZY(JL) - XYLIM(1))/ZDYLIM+1),NJMAX)

  KINDEX(1,JL) = (JJ-1) * NIMAX + JI
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
  IF (KSSO/=0) THEN
    KISSOX(1,JL) = 1 + INT( FLOAT(KSSO) * (ZX(JL)-XXLIM(JI))/(XXLIM(JI+1)-XXLIM(JI)) )
    KISSOY(1,JL) = 1 + INT( FLOAT(KSSO) * (ZY(JL)-XYLIM(JJ))/(XYLIM(JJ+1)-XYLIM(JJ)) )
  END IF
END DO
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZX )
DEALLOCATE(ZY )
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_CONF_PROJ',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_CONF_PROJ
