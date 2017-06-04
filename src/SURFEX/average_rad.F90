!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_RAD(PFRAC_TILE,                              &
                   PDIR_ALB_TILE, PSCA_ALB_TILE, PEMIS_TILE, PTRAD_TILE,  &
                   PDIR_ALB, PSCA_ALB, PEMIS, PTRAD                       )  
!     #################################################################
!
!
!!****  *AVERAGE_RAD*  
!!
!!    PURPOSE
!!    -------
!      Average the radiative fluxes from the land and water surfaces depending on the
!      fraction of each surface cover type in the mesh area.
!     
!!**  METHOD
!!    ------
!
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
!!    AUTHOR
!!    ------
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!       B. decharme 04/2013  Optimization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PFRAC_TILE    ! Fraction in a mesh-area of 
!                                                   ! a given surface
REAL, DIMENSION(:,:),   INTENT(IN) :: PEMIS_TILE    ! emissivity
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDIR_ALB_TILE ! direct albedo
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSCA_ALB_TILE ! diffuse albedo
REAL, DIMENSION(:,:),   INTENT(IN) :: PTRAD_TILE    ! surface radiative temp.
REAL, DIMENSION(:),     INTENT(OUT):: PEMIS         ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT):: PDIR_ALB      ! direct albedo
REAL, DIMENSION(:,:),   INTENT(OUT):: PSCA_ALB      ! diffuse albedo
REAL, DIMENSION(:),     INTENT(OUT):: PTRAD         ! surface radiative temp.
!
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER    :: ZEPS = 1.E-10
!
INTEGER :: INI, INP, INSWB  ! dimenssion
INTEGER :: JI, JP, JSWB     ! loop counter on tiles
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_RAD',0,ZHOOK_HANDLE)
!
INI   = SIZE(PFRAC_TILE,1)
INP   = SIZE(PFRAC_TILE,2)
INSWB = SIZE(PDIR_ALB_TILE,2)
!
!       1.     Grid-Box average surface temperatures, radiative properties
!              -----------------------------------------------------------
!
! albedo:
! 
PDIR_ALB   (:,:) = 0.
PSCA_ALB   (:,:) = 0.
!
DO JSWB = 1,INSWB
  DO JP = 1,INP
    DO JI = 1,INI
      PDIR_ALB(JI,JSWB) = PDIR_ALB(JI,JSWB) + PFRAC_TILE(JI,JP) * PDIR_ALB_TILE(JI,JSWB,JP)
      PSCA_ALB(JI,JSWB) = PSCA_ALB(JI,JSWB) + PFRAC_TILE(JI,JP) * PSCA_ALB_TILE(JI,JSWB,JP)
    END DO
  END DO
END DO
!
! emissivity
!
PEMIS      (:)   = 0.
!
DO JP = 1,INP
  DO JI = 1,INI
     PEMIS(JI) = PEMIS(JI) + PFRAC_TILE(JI,JP) * PEMIS_TILE(JI,JP)
  END DO
END DO
!
! radiative surface temperature
!
PTRAD      (:)   = 0.
!
DO JP = 1, INP
  DO JI = 1,INI
     PTRAD(JI) = PTRAD(JI) + (PTRAD_TILE(JI,JP)**4)*PFRAC_TILE(JI,JP)*PEMIS_TILE(JI,JP)
  END DO
END DO
!
PTRAD(:) = ( PTRAD(:) / MAX(PEMIS(:),ZEPS) )**0.25
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_RAD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_RAD
