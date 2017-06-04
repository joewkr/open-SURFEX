!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_PHY(PFRAC_TILE,             &
                   PTSURF_TILE, PZ0_TILE,               &
                   PZ0H_TILE, PQSURF_TILE,              &   
                   PUREF, PZREF,                        &
                   PTSURF, PZ0, PZ0H, PQSURF            )  
!     ######################################################################
!
!
!!****  *AVERAGE_PHY*  
!!
!!    PURPOSE
!!    -------
!      Average the physical properties from the land and water surfaces depending on the
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
!
!      B. Decharme 07/2015 - Modification to deal with E-zone points in Arome/Aladin
!-----------------------------------------------------------------------------------
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
REAL, DIMENSION(:,:), INTENT(IN) :: PFRAC_TILE ! Fraction in a mesh-area of 
!
REAL, DIMENSION(:,:), INTENT(IN) :: PTSURF_TILE ! surface effective temperature (K)
REAL, DIMENSION(:,:), INTENT(IN) :: PZ0_TILE    ! roughness length for momentum (m)
REAL, DIMENSION(:,:), INTENT(IN) :: PZ0H_TILE   ! roughness length for heat     (m)
REAL, DIMENSION(:,:), INTENT(IN) :: PQSURF_TILE ! specific humidity at surface  (kg/kg)
!
REAL, DIMENSION(:),   INTENT(IN) :: PUREF     ! height of wind forcing                (m)
REAL, DIMENSION(:),   INTENT(IN) :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(:),   INTENT(OUT):: PTSURF    ! surface effective temperature   (K)
REAL, DIMENSION(:),   INTENT(OUT):: PZ0       ! roughness length for momentum   (m)
REAL, DIMENSION(:),   INTENT(OUT):: PZ0H      ! roughness length for heat       (m)
REAL, DIMENSION(:),   INTENT(OUT):: PQSURF    ! specific humidity at surface    (kg/kg)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PUREF)) :: ZWORK_Z0       ! work array for roughness length for momentum 
REAL, DIMENSION(SIZE(PUREF)) :: ZWORK_Z0H      ! work array for roughness length for heat     
!
INTEGER :: INI, INP  ! dimenssion
INTEGER ::  JI, JP    ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_PHY',0,ZHOOK_HANDLE)
!
INI  = SIZE(PFRAC_TILE,1)
INP  = SIZE(PFRAC_TILE,2)
!
PTSURF     (:)   = 0.
PZ0        (:)   = 0.
PZ0H       (:)   = 0.
PQSURF     (:)   = 0.
!
ZWORK_Z0   (:)   = 0.
ZWORK_Z0H  (:)   = 0.
!
!       1.     Grid-Box average 
!              ----------------
DO JP = 1, INP
!
  DO JI = 1, INI
!  
!    surface effective temperature
!
     PTSURF(JI) = PTSURF(JI) + PFRAC_TILE(JI,JP) * PTSURF_TILE(JI,JP)
!
!    specific humidity at surface
!
     PQSURF(JI) = PQSURF(JI) + PFRAC_TILE(JI,JP) * PQSURF_TILE(JI,JP)
!
!    roughness length for momentum and heat
!
     ZWORK_Z0 (JI) = ZWORK_Z0 (JI) + PFRAC_TILE(JI,JP) * 1.0/(LOG(PUREF(JI)/PZ0_TILE (JI,JP)))**2
     ZWORK_Z0H(JI) = ZWORK_Z0H(JI) + PFRAC_TILE(JI,JP) * 1.0/(LOG(PZREF(JI)/PZ0H_TILE(JI,JP)))**2
!
  ENDDO
!
ENDDO
!

DO JI = 1, INI
 IF(ZWORK_Z0(JI) /= 0 ) then
   PZ0 (JI) = PUREF(JI) * EXP( - SQRT(1./ZWORK_Z0 (JI)) )
   PZ0H(JI) = PZREF(JI) * EXP( - SQRT(1./ZWORK_Z0H(JI)) )
 ELSE
   PZ0 (JI) = XUNDEF
   PZ0H(JI) = XUNDEF
 ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_PHY',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_PHY
