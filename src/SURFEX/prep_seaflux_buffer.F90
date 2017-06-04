!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_BUFFER(HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_SEAFLUX_BUFFER* - prepares SEAFLUX fields from BUFFER
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!

!
USE MODE_READ_BUFFER
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
!
USE MODD_PREP,       ONLY : CINTERP_TYPE
USE MODD_GRID_BUFFER,  ONLY : NNI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to prepare
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_BUF    ! current date and time
 CHARACTER(LEN=6)              :: YINMODEL ! model from which BUFFER data originate
REAL, DIMENSION(:),       POINTER :: ZFIELD   ! field read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_BUFFER',0,ZHOOK_HANDLE)
 CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUF)

!
!*      2.     Reading of field
!              ----------------
!-----------------
SELECT CASE(HSURF)
!-----------------
!
!* 1.  Orography
!      ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(NNI,1))
    PFIELD = 0.0
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        CALL READ_BUFFER_ZS(KLUOUT,YINMODEL,ZFIELD)
        PFIELD(:,1) = ZFIELD(:)
        DEALLOCATE(ZFIELD)
    END SELECT

!
!* 3.  Temperature profiles
!      --------------------
!
  CASE('SST    ')
    ALLOCATE(PFIELD(NNI,1))
    PFIELD = 0.0
    SELECT CASE (YINMODEL)
      CASE ('ALADIN')
        CALL READ_BUFFER_SST(KLUOUT,YINMODEL,ZFIELD)
        PFIELD(:,1) = ZFIELD(:)
        DEALLOCATE(ZFIELD)
    END SELECT
!
!*      5.  Sea surface salinity and ice fraction
!           -------------------------------------
!
  CASE('SSS    ', 'SIC    ')
    ALLOCATE(PFIELD(NNI,1))
    PFIELD = 0.0
!
END SELECT

!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_BUFFER',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SEAFLUX_BUFFER
