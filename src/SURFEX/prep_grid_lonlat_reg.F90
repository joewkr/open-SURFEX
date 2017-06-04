!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_GRID_LONLAT_REG (&
                                       HFILETYPE,HINTERP_TYPE,KNI)
!     ##########################################################################
!
!!****  *PREP_GRID_LATLON* - reads EXTERNALIZED Surface grid.
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
!
!
!
USE MODI_READ_SURF
!
USE MODD_GRID_LATLONREGUL, ONLY : XILAT1,XILON1,XILAT2,XILON2,NINLAT,NINLON,NILENGTH,XILATARRAY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
!
!
 CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE    ! file type
 CHARACTER(LEN=6),  INTENT(OUT)   :: HINTERP_TYPE ! Grid type
INTEGER,           INTENT(OUT)   :: KNI          ! number of points
!
!* 0.2 Declaration of local variables
!      ------------------------------
!
 CHARACTER(LEN=12) :: YRECFM    ! Name of the article to be read
INTEGER           :: IRESP
!
INTEGER :: JL        ! loop counter
INTEGER :: ILON
REAL :: ZDLAT, ZDLON
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
!
!*   1 Projection
!      ----------
!
IF (LHOOK) CALL DR_HOOK('PREP_GRID_LONLAT_REG',0,ZHOOK_HANDLE)
YRECFM = 'LONMIN'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,XILON1,IRESP)
YRECFM = 'LONMAX'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,XILON2,IRESP)
YRECFM = 'LATMIN'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,XILAT1,IRESP)
YRECFM = 'LATMAX'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,XILAT2,IRESP)
YRECFM = 'NLAT'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,NINLAT,IRESP)
!
IF (ALLOCATED(NINLON)) DEALLOCATE(NINLON)
ALLOCATE(NINLON(NINLAT))
YRECFM = 'NLON'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,NINLON(1),IRESP)
IF (NINLAT.GT.1) NINLON(2:NINLAT) = NINLON(1)
!
!-----------------------------------------------------------------------
!
!*   3 Computes additional quantities used in interpolation
!      ----------------------------------------------------
!
NILENGTH = NINLAT*NINLON(1)
KNI = NILENGTH
!
ZDLAT = (XILAT2-XILAT1)/NINLAT
ZDLON = (XILON2-XILON1)/NINLON(1)
!
XILON1 = XILON1 + ZDLON/2.
XILON2 = XILON2 - ZDLON/2.
XILAT1 = XILAT1 + ZDLAT/2.
XILAT2 = XILAT2 - ZDLAT/2.
!
IF (ALLOCATED(XILATARRAY)) DEALLOCATE(XILATARRAY)
ALLOCATE(XILATARRAY(NINLAT))
!
XILATARRAY(1)=XILAT1
DO JL = 2,NINLAT
  XILATARRAY(JL) = XILATARRAY(JL-1) + ZDLAT
ENDDO
!
!-----------------------------------------------------------------------
IF(KNI==1)THEN
  HINTERP_TYPE = 'UNIF'
ELSE
  HINTERP_TYPE = 'HORIBL'
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_GRID_LONLAT_REG',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------
!
END SUBROUTINE PREP_GRID_LONLAT_REG
