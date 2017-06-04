!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_NEAR_MESHES_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)
!     ##############################################################
!
!!**** *GET_NEAR_MESHES_LONLAT_REG* get the near grid mesh indices
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
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NINDEX, NRANK, NNUM
!
USE MODE_GRIDTYPE_LONLAT_REG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
INTEGER,                         INTENT(IN)    :: KNEAR_NBR ! number of nearest points wanted
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(:,:),POINTER   :: KNEAR     ! near mesh indices
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: ILON    ! number of points in longitude
INTEGER :: ILAT    ! number of points in latitude
INTEGER :: JLAT, JLON
INTEGER :: JL
INTEGER :: JX, JY
INTEGER :: IDIST
INTEGER :: ICOUNT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_LONLAT_REG',0,ZHOOK_HANDLE)
 CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,KLON=ILON,KLAT=ILAT)
!
KNEAR  (:,:) = 0
!
IDIST = INT(SQRT(FLOAT(KNEAR_NBR)))
!
IF (ILON*ILAT==KL) THEN
  DO JLAT=1,ILAT
    DO JLON=1,ILON
      ICOUNT = 0
      JL = JLON + ILON * (JLAT-1)
      IF (NINDEX(JL)==NRANK) THEN
        KNEAR(NNUM(JL),:) = 0      
        DO JX=-(IDIST-1)/2,IDIST/2
          DO JY=-(IDIST-1)/2,IDIST/2
            IF (JLON+JX>0 .AND. JLON+JX<ILON+1 .AND. JLAT+JY>0 .AND. JLAT+JY<ILAT+1) THEN
              ICOUNT = ICOUNT + 1
              KNEAR(NNUM(JL),ICOUNT) = (JLON+JX) + ILON * (JLAT+JY-1)
            END IF
          END DO
        END DO
      ENDIF
    END DO
  END DO
END IF
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_NEAR_MESHES_LONLAT_REG
