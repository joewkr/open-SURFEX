!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL (DTCO, U, GCP, KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL * - Call the interpolation of a surface field
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODI_HOR_INTERPOL_GAUSS
USE MODI_HOR_INTERPOL_ROTLATLON
USE MODI_HOR_INTERPOL_AROME
USE MODI_HOR_INTERPOL_CONF_PROJ
USE MODI_HOR_INTERPOL_CARTESIAN
USE MODI_HOR_INTERPOL_LATLON
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_HOR_INTERPOL_BUFFER
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)   :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT)  :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
INTEGER :: JL ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL',0,ZHOOK_HANDLE)
SELECT CASE (CINTERP_TYPE)
!
!*      1.     Interpolation with horibl (from gaussian, Legendre or regular grid)
!              -------------------------------------------------------------------
!
  CASE('HORIBL')
    SELECT CASE(CINGRID_TYPE)
!
!*      1.1    Interpolation from gaussian or Legendre
!
      CASE ('GAUSS     ')
        CALL HOR_INTERPOL_GAUSS(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.2    Interpolation from regular grid
!
      CASE ('AROME     ')
        CALL HOR_INTERPOL_AROME(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.3    Interpolation from regular lat/lon coord
!
      CASE ('LATLON    ')
        CALL HOR_INTERPOL_LATLON(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.4    Interpolation from rotated lat/lon coord
!
      CASE ('ROTLATLON ')
        CALL HOR_INTERPOL_ROTLATLON(KLUOUT,PFIELDIN,PFIELDOUT)        

      CASE DEFAULT
        CALL ABOR1_SFX('HOR_INTERPOL: WRONG GRID TYPE'//CINGRID_TYPE)

    END SELECT
!
!*      2.     Prescribed uniform field
!              ------------------------
!
  CASE('UNIF  ')
    DO JL=1,SIZE(PFIELDIN,2)
      PFIELDOUT(:,JL) = PFIELDIN(1,JL)
    END DO
!
!*      3.     Bilinear interpolation
!              ----------------------
!
  CASE('BILIN ')
    SELECT CASE(CINGRID_TYPE)
      CASE ('CONF PROJ ')
        CALL HOR_INTERPOL_CONF_PROJ(GCP,KLUOUT,PFIELDIN,PFIELDOUT)
      CASE ('CARTESIAN ')
        CALL HOR_INTERPOL_CARTESIAN(KLUOUT,PFIELDIN,PFIELDOUT)
    END SELECT
!
!*      4.     no interpolation, only packing
!              ------------------------------
!
  CASE('BUFFER')
    CALL HOR_INTERPOL_BUFFER(DTCO, U, KLUOUT,PFIELDIN,PFIELDOUT)

!
!*      4.     no interpolation
!              ----------------
!
  CASE('NONE  ')
    DO JL=1,SIZE(PFIELDIN,2)
      PFIELDOUT(:,JL) = PFIELDIN(:,JL)
    END DO

  CASE DEFAULT 
    CALL ABOR1_SFX('HOR_INTERPOL: WRONG INTERPOLATION TYPE'//CINTERP_TYPE)

END SELECT
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL
