!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.

!     ###################################################################
      SUBROUTINE OI_HOR_EXTRAPOL_SURF(NDIM,PLAT_IN,PLON_IN,PFIELD_IN, &
                                         PLAT,PLON,PFIELD,OINTERP,PZS,NDIM2)  
!     ###################################################################
!
!!**** *OI_HOR_EXTRAPOL_SURF* extrapolate a surface field
!!
!!    PURPOSE
!!    -------
!!
!!
!!    METHOD
!!    ------
!!
!!    For each point to interpolate, the nearest valid point value is set.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/12/98
!!   V. Masson     01/2004 extrapolation in latitude and longitude
!!   J.-F. Mahfouf 03/2010 adaptation for OI soil analysis 
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,               INTENT(IN)     :: NDIM  ! dimension of arrays
REAL,   DIMENSION(NDIM),  INTENT(IN)     :: PLAT_IN  ! input lat. of each grid mesh.
REAL,   DIMENSION(NDIM),  INTENT(IN)     :: PLON_IN  ! input lon. of each grid mesh.
REAL,   DIMENSION(NDIM),  INTENT(IN)     :: PFIELD_IN! input field on grid mesh
REAL,   DIMENSION(NDIM),  INTENT(IN)     :: PLAT     ! latitude of each grid mesh.
REAL,   DIMENSION(NDIM),  INTENT(IN)     :: PLON     ! longitude of each grid mesh.
REAL,   DIMENSION(NDIM),  INTENT(INOUT)  :: PFIELD   ! field on grid mesh
LOGICAL,DIMENSION(NDIM),  INTENT(IN)     :: OINTERP  ! .true. where physical value is needed
REAL,   DIMENSION(NDIM), OPTIONAL, INTENT(IN) :: PZS      ! surface height
INTEGER,                 OPTIONAL, INTENT(IN) :: NDIM2 ! Optional subdomain to search in
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL     :: ZLAT    ! latitude of point to define
REAL     :: ZLON    ! longitude of point to define
REAL     :: ZDIST   ! current distance to valid point (in lat/lon grid)
REAL     :: ZFIELD  ! current found field value
REAL     :: ZNDIST  ! smallest distance to valid point
REAL     :: ZCOSLA  ! cosine of latitude
REAL     :: ZZS_OUT ! altitude of nearest grid point
REAL,PARAMETER :: ZLIMMAX = 1.  ! Maximum distance allowed (in degrees)
!
INTEGER  :: JI    ! loop index on points
INTEGER  :: JISC  ! loop index on valid points
INTEGER  :: JISC1,JISC2,JZLIMCNT
!
REAL     :: ZLONSC, ZDLAT, ZDLON, ZCONV, ZR_EARTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
LOGICAL :: LNDIM2
!
! Earth radius
!
IF (LHOOK) CALL DR_HOOK('OI_HOR_EXTRAPOL_SURF',0,ZHOOK_HANDLE)
ZR_EARTH = 6371598.0 
!
! Angle conversion factor
!  
ZCONV = XPI/180.0
!-------------------------------------------------------------------------------
!
!*    3.     No data point
!            -------------
!
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0 .AND. LHOOK) CALL DR_HOOK('OI_HOR_EXTRAPOL_SURF',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*      4.   Loop on points to define
!            ------------------------
!
JZLIMCNT = 0
!
DO JI=1,NDIM
  IF (PFIELD(JI)/=XUNDEF) CYCLE
  IF (.NOT. OINTERP(JI))  CYCLE
!
!*      4.1  initialisation
!            --------------
!
  ZNDIST=XUNDEF
  ZLAT=PLAT(JI)
  ZLON=PLON(JI)
  ZFIELD=PFIELD(JI)
  ZCOSLA=COS(ZLAT*ZCONV)
  IF (PRESENT(PZS)) ZZS_OUT=PZS(JI)
  IF ( PRESENT (NDIM2)) THEN
    JISC1=MAX((JI-NDIM2),1)
    JISC2=MIN((JI+NDIM2),NDIM)
    LNDIM2=.TRUE.
  ELSE
    JISC1=1
    JISC2=NDIM
    LNDIM2=.FALSE.
  ENDIF
!
!*      4.2  extrapolation with nearest valid point
!            --------------------------------------
!
  DO JISC=JISC1,JISC2
    IF (PFIELD_IN(JISC)/=XUNDEF) THEN
      ZLONSC = PLON_IN(JISC)
      IF (ZLONSC-ZLON> 180.) ZLONSC = ZLONSC - 360.0
      IF (ZLONSC-ZLON<-180.) ZLONSC = ZLONSC + 360.0
      ZDLAT = (PLAT_IN(JISC)-ZLAT)*ZCONV
      ZDLON = (ZLONSC-ZLON)*ZCONV
      ZDIST = ZDLAT*ZDLAT + ZDLON*ZDLON*ZCOSLA*ZCOSLA
      IF (ZDIST<=ZNDIST) THEN
        ZFIELD=PFIELD_IN(JISC)
        IF (PRESENT(PZS)) ZZS_OUT=PZS(JISC)
        ZNDIST=ZDIST
      END IF
    END IF
  END DO

  ! Check if we got values
  IF ( ZNDIST  == XUNDEF ) THEN
    CALL ABOR1_SFX("Extrapolated point is undefined! No nearby point found.")      
  ELSEIF ( ZNDIST > (ZLIMMAX*ZCONV) ) THEN
    IF ( LNDIM2 ) &
    & CALL ABOR1_SFX("Distance to extrapolated point is to large. Increase ZLIMMAX or NDIM2")     
    JZLIMCNT = JZLIMCNT + 1
  ENDIF
  IF (PRESENT(PZS)) THEN
    PFIELD(JI) = ZFIELD + (ZZS_OUT - PZS(JI))*0.0065
  ELSE
    PFIELD(JI) = ZFIELD
  ENDIF  

END DO

IF ( JZLIMCNT > 0 ) THEN
  PRINT *,'Points with extrapolation distance > ',ZLIMMAX,' degrees are ',JZLIMCNT
ENDIF

IF (LHOOK) CALL DR_HOOK('OI_HOR_EXTRAPOL_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OI_HOR_EXTRAPOL_SURF
