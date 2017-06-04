!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_MESH_INDEX(UG,KLUOUT,KNBLINES,PLAT,PLON,KINDEX,PVALUE,PNODATA,KSSO,KISSOX,KISSOY)
!     ##############################################################
!
!!**** *GET_MESH_INDEX* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
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
!!    P. Samuelsson  SMHI  10/2014   Rotated lonlat
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_POINT_OVERLAY
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_MESH_INDEX_CONF_PROJ
!
USE MODI_GET_MESH_INDEX_GAUSS
!
USE MODI_GET_MESH_INDEX_IGN
!
USE MODI_GET_MESH_INDEX_LONLAT_REG
!
USE MODI_GET_MESH_INDEX_LONLATVAL
!
USE MODI_GET_MESH_INDEX_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER,                         INTENT(IN)    :: KLUOUT  ! output listing
INTEGER,                         INTENT(IN)    :: KNBLINES
REAL,    DIMENSION(:),           INTENT(IN)    :: PLAT    ! latitude of the point
REAL,    DIMENSION(:),           INTENT(IN)    :: PLON    ! longitude of the point
INTEGER, DIMENSION(:,:),         INTENT(OUT)   :: KINDEX  ! index of the grid mesh where the point is
!
REAL, DIMENSION(:), OPTIONAL,     INTENT(IN)   :: PVALUE  ! value of the point to add
REAL, OPTIONAL,                   INTENT(IN)   :: PNODATA
!
INTEGER,               OPTIONAL, INTENT(IN)    :: KSSO    ! number of subgrid mesh in each direction
INTEGER, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: KISSOX  ! X index of the subgrid mesh where the point is
INTEGER, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: KISSOY  ! Y index of the subgrid mesh where the point is
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                        :: ISSO
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IISSOX
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IISSOY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX',0,ZHOOK_HANDLE)
SELECT CASE (UG%G%CGRID)
!     
  CASE("CONF PROJ ","LONLAT REG","GAUSS     ","IGN      ","LONLATVAL ","LONLAT ROT")
    IF (PRESENT(KSSO) .AND. PRESENT(KISSOX) .AND. PRESENT(KISSOY)) THEN
      ISSO = KSSO
    ELSE
      ISSO = 0
    ENDIF
    !
    IF (UG%G%CGRID=="CONF PROJ ") THEN
      CALL GET_MESH_INDEX_CONF_PROJ(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY)  
    ENDIF
    IF (UG%G%CGRID=="LONLAT REG") THEN
      IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
        CALL GET_MESH_INDEX_LONLAT_REG(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY, &
                                     PVALUE,PNODATA)
      ELSE            
        CALL GET_MESH_INDEX_LONLAT_REG(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY)  
      ENDIF
    ENDIF
    IF (UG%G%CGRID=="GAUSS     ") THEN
      IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
        CALL GET_MESH_INDEX_GAUSS(KNBLINES,ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY, &
                                     PVALUE,PNODATA)
      ELSE
        CALL GET_MESH_INDEX_GAUSS(KNBLINES,ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY)
      ENDIF              
    ENDIF
    IF (UG%G%CGRID=="IGN       ") THEN
      IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
        CALL GET_MESH_INDEX_IGN(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY, &
                                     PVALUE,PNODATA)
      ELSE       
        CALL GET_MESH_INDEX_IGN(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY)
      ENDIF  
    ENDIF
    IF (UG%G%CGRID=="LONLATVAL ") &
      CALL GET_MESH_INDEX_LONLATVAL(ISSO,UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,IISSOX,IISSOY)  
    IF (UG%G%CGRID=="LONLAT ROT") THEN
      CALL GET_MESH_INDEX_LONLAT_ROT(SIZE(PLAT),UG%XGRID_FULL_PAR,PLAT,PLON,KINDEX,ISSO,IISSOX,IISSOY)  
    ENDIF
    !
    IF (PRESENT(KSSO) .AND. PRESENT(KISSOX) .AND. PRESENT(KISSOY)) THEN
      KISSOX = IISSOX
      KISSOY = IISSOY
    ENDIF

  CASE DEFAULT
    WRITE(KLUOUT,*) 'error in physiographic fields computations (routine GET_MESH_INDEX)'
    WRITE(KLUOUT,*) 'It is impossible to retrieve geographical coordinates (latitude, longitude)'
    WRITE(KLUOUT,*) 'for the following grid type: CGRID = ', UG%G%CGRID
    CALL ABOR1_SFX('GET_MESH_INDEX: IMPOSSIBLE TO RETRIEVE GEOGRAPHICAL COORDINATES')
END SELECT
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX
