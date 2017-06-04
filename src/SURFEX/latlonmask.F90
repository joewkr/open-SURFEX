!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE LATLONMASK(HGRID,KGRID_PAR,PGRID_PAR,OLATLONMASK)
!     #####################
!
!!**** *LATLONMASK* builds the latiude and longitude mask including the grid
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!      
!!      Original        03/2004
!!                      10/2007  E. Martin  IGN Grids
!!                      12/2012  P. Samuelsson SMHI Rotated lonlat
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_LATLONMASK_CONF_PROJ
!
USE MODI_LATLONMASK_IGN
!
USE MODI_LATLONMASK_LONLAT_REG
!
USE MODI_LATLONMASK_LONLATVAL
!
USE MODI_LATLONMASK_LONLAT_ROT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=10),           INTENT(IN)  :: HGRID       ! type of grid
INTEGER                                  :: KGRID_PAR   ! size of PGRID_PAR
REAL,    DIMENSION(:),       POINTER     :: PGRID_PAR   ! parameters defining this grid
LOGICAL, DIMENSION(720,360), INTENT(OUT) :: OLATLONMASK ! mask where domain is
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LATLONMASK',0,ZHOOK_HANDLE)
SELECT CASE (HGRID)
  CASE('CONF PROJ ')
    CALL LATLONMASK_CONF_PROJ(KGRID_PAR,PGRID_PAR,OLATLONMASK)

  CASE('LONLAT REG')
    CALL LATLONMASK_LONLAT_REG(KGRID_PAR,PGRID_PAR,OLATLONMASK)

  CASE('IGN       ')
    CALL LATLONMASK_IGN(KGRID_PAR,PGRID_PAR,OLATLONMASK)

  CASE('LONLATVAL ')
    CALL LATLONMASK_LONLATVAL(KGRID_PAR,PGRID_PAR,OLATLONMASK)

  CASE('LONLAT ROT')
    CALL LATLONMASK_LONLAT_ROT(KGRID_PAR,PGRID_PAR,OLATLONMASK)

  CASE DEFAULT
    OLATLONMASK(:,:) = .TRUE.
END SELECT
IF (LHOOK) CALL DR_HOOK('LATLONMASK',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
END SUBROUTINE LATLONMASK
