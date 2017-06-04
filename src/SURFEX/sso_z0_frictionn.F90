!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################################
SUBROUTINE SSO_Z0_FRICTION_n (USS, PSEA,PUREF,PRHOA,PU,PV,PPEW_A_COEF,PPEW_B_COEF,PSFU,PSFV)
!     ################################################################################
!
!!****  *SSO_Z0_FRICTION_n * - Computes subgrid-scale orography friction
!                                  according to several options:
!                                CROUGH='Z01D' : orographic roughness length
!                                CROUGH='Z04D' : orographic roughness length
!                                                variable with wind direction
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
!!      Original    05/2010
!!      E. Martin   01/2012 Correction masque (compatibilité XUNDEF)
!!      B. Decharme 09/2012 new wind implicitation and sea fraction
!!      B. Decharme 06/2013 CIMPLICIT_WIND in MODD_REPROD_OPER
!!      J. Escobar  05/2014 for bug with ifort/10, replace WHERE by IF
!!      J. Escobar  06/2015 bug with gfortran ZZ0EFF to small, change with > XSURF_EPSILON
!----------------------------------------------------------------
!
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_REPROD_OPER, ONLY : CIMPLICIT_WIND
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, XSURF_EPSILON
USE MODD_CSTS,             ONLY : XKARMAN, XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SSO_t), INTENT(INOUT) :: USS
!
REAL, DIMENSION(:), INTENT(IN)    :: PSEA      ! Sea fraction                          (-)
REAL, DIMENSION(:), INTENT(IN)    :: PUREF     ! Wind forcing height                   (m)
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(:), INTENT(IN)    :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(:), INTENT(IN)    :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_A_COEF! implicit coefficients                (m2s/kg)
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_B_COEF! needed if HCOUPLING='I'              (m/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFV      ! meridian momentum flux                (Pa)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PU))    :: ZWIND   ! wind strength (m/s)
REAL, DIMENSION(SIZE(PU))    :: ZWORK   ! work array
REAL, DIMENSION(SIZE(PU))    :: ZDIR    ! wind direction (rad., clockwise)
REAL, DIMENSION(SIZE(PU))    :: ZALFA   ! angle between z0eff J axis and wind direction (rad., clockwise) 
REAL, DIMENSION(SIZE(PU))    :: ZCOS2, ZSIN2
REAL, DIMENSION(SIZE(PU))    :: ZZ0EFF  ! Momentum Roughness length
REAL, DIMENSION(SIZE(PU))    :: ZCD     ! drag coefficient
REAL, DIMENSION(SIZE(PU))    :: ZUSTAR2 ! square of friction velocity
REAL, DIMENSION(SIZE(PU))    :: ZSSO_SFU! zonal orographic momentum flux
REAL, DIMENSION(SIZE(PU))    :: ZSSO_SFV! meridian orographic momentum flux
LOGICAL, DIMENSION(SIZE(PU)) :: GMASK   ! mask where SSO exists
INTEGER                      :: II
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SSO_Z0_FRICTION_N',0,ZHOOK_HANDLE)
!
ZWORK(:) = XUNDEF
!
!*      1.     roughness length formalism
!              --------------------------
!
!* wind strength
!
ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
!
!* wind direction
!
ZDIR(:) = 0.
WHERE (ZWIND(:)>0.)  ZDIR(:)=ATAN2(PU(:),PV(:))
!
!* default value
!
GMASK(:)=(PSEA(:)/=1..AND. USS%XZ0REL(:)/=0.)
ZZ0EFF(:) = XUNDEF
!
!*      2.     Constant orographic roughness length
!              ------------------------------------
!
IF (USS%CROUGH=="Z01D") ZZ0EFF(:) = USS%XZ0REL(:)
!
!*      3.     Directionnal roughness length
!              -----------------------------
!
IF (USS%CROUGH=="Z04D") THEN
   DO II=1,SIZE(GMASK)
      IF (GMASK(II)) THEN
         !
         ZALFA(II) = ZDIR(II) - USS%XZ0EFFJPDIR(II) * XPI/180.
         !
         IF    (ZALFA(II)<=-XPI) THEN
            ZALFA(II) = ZALFA(II) + 2.*XPI
         ELSEIF(ZALFA(II)>  XPI) THEN
            ZALFA(II) = ZALFA(II) - 2.*XPI
         END IF
         !
         IF (ZALFA(II)>=-XPI.AND.ZALFA(II)<=XPI) THEN
            !
            ZSIN2(II) = SIN(ZALFA(II))**2
            ZCOS2(II) = COS(ZALFA(II))**2
            !
            IF (ZALFA(II)<0.) THEN
               ZZ0EFF(II)=USS%XZ0EFFIM(II)*ZSIN2(II)
            ELSE
               ZZ0EFF(II)=USS%XZ0EFFIP(II)*ZSIN2(II)
            END IF
            !
            IF (ZALFA(II)>=-XPI/2. .AND. ZALFA(II)<XPI/2.) THEN
               ZZ0EFF(II) = ZZ0EFF(II) + USS%XZ0EFFJP(II)*ZCOS2(II)
            ELSE
               ZZ0EFF(II) = ZZ0EFF(II) + USS%XZ0EFFJM(II)*ZCOS2(II)
            END IF
            !
         END IF
         !    
      END IF
   END DO
ENDIF
!
!*      4.     Friction coefficient
!              --------------------
!
ZCD    (:) = 0.
ZUSTAR2(:) = 0.
!
GMASK(:)=(GMASK(:).AND.ZZ0EFF(:)>XSURF_EPSILON)
!
DO II=1,SIZE(GMASK)
  !
  IF (GMASK(II)) THEN
    !
    !* sets a limit to roughness length
    ZZ0EFF(II) = MIN(ZZ0EFF(II),PUREF(II)/USS%XFRACZ0)
    !
    ! neutral case
    ZCD(II) = (XKARMAN/LOG(PUREF(II)/ZZ0EFF(II)))**2
  END IF
  !
END DO
!
!*      5.     Friction due to orography
!              -------------------------
!
! Modify flux-form implicit coupling coefficients:
!
IF(CIMPLICIT_WIND=='OLD')THEN
! old implicitation
  ZUSTAR2(:) =  ZCD(:)*ZWIND(:)*PPEW_B_COEF(:)   &
             / (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ELSE
! new implicitation
  ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*(2.*PPEW_B_COEF(:)-ZWIND(:))   )   &
             / (1.0-2.0*PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ENDIF
!
WHERE (GMASK(:))
!
  ZWORK(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
  ZWORK(:) = MAX(ZWORK(:),0.)
!
  WHERE(PPEW_A_COEF(:)/= 0.)
    ZUSTAR2(:) = MAX( ( ZWORK(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
  ENDWHERE
!
END WHERE
!
!*      6.     Projection of friction on wind components
!              -----------------------------------------
!
ZSSO_SFU (:) = 0.
ZSSO_SFV (:) = 0.
WHERE (ZWIND(:)>0.)
  ZSSO_SFU (:) = - PU(:)/ZWIND(:) * ZUSTAR2(:) * PRHOA(:)
  ZSSO_SFV (:) = - PV(:)/ZWIND(:) * ZUSTAR2(:) * PRHOA(:)
END WHERE
!
!*      7.     Adds orographic friction to other sources of friction
!              -----------------------------------------------------
!
PSFU(:) = PSFU(:) + ZSSO_SFU(:) * (1.0-PSEA(:))
PSFV(:) = PSFV(:) + ZSSO_SFV(:) * (1.0-PSEA(:))
!
IF (LHOOK) CALL DR_HOOK('SSO_Z0_FRICTION_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SSO_Z0_FRICTION_n
