!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_TSURF(PFRAC_TILE, PTSURF_TILE, PTSURF)  
!     ######################################################
!
!
!!****  *AVERAGE_TSURF*  
!!
!!    PURPOSE
!!    -------
!      Average the surface temperature from the land and water surfaces depending on the
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
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/04/2013 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PFRAC_TILE  ! Fraction in a mesh-area of 
REAL, DIMENSION(:,:), INTENT(IN) :: PTSURF_TILE ! surface effective temperature (K)
REAL, DIMENSION(:),   INTENT(OUT):: PTSURF      ! surface effective temperature   (K)
!
!*      0.2    declarations of local variables
!
INTEGER :: INI, INP  ! dimenssion
INTEGER ::  JI, JP    ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_TSURF',0,ZHOOK_HANDLE)
!
INI  = SIZE(PFRAC_TILE,1)
INP  = SIZE(PFRAC_TILE,2)
!
PTSURF(:)   = 0.
!
DO JP = 1,INP
  DO JI = 1,INI
     PTSURF(JI) = PTSURF(JI) + PFRAC_TILE(JI,JP) * PTSURF_TILE(JI,JP)
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_TSURF',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_TSURF
