!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE GET_Z0REL(ISS,OMASK  )
!     ######################################################################
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS,       ONLY : XKARMAN
USE MODD_ISBA_PAR,   ONLY : XCDZ0EFF
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SSO_t), INTENT(INOUT) :: ISS
!
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL,    DIMENSION(SIZE(ISS%XAOSIP)) :: ZLOC
LOGICAL, DIMENSION(SIZE(ISS%XAOSIP)) :: GMASK
!
INTEGER :: JJ      ! loop counter on points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_Z0REL',0,ZHOOK_HANDLE)
!
IF (PRESENT(OMASK)) THEN
  GMASK=OMASK
ELSE
  GMASK=(ISS%XAOSIP/=XUNDEF)
END IF
!
ISS%XZ0REL=XUNDEF
!
ZLOC(:) = 0.
!
WHERE (GMASK(:))
  ZLOC  (:) = 0.25 * XCDZ0EFF/(2.*XKARMAN**2)                  &
                   * (ISS%XAOSIP(:) + ISS%XAOSIM(:) + ISS%XAOSJP(:) + ISS%XAOSJM(:))        
  WHERE ( ZLOC(:) > 0. )
    ISS%XZ0REL(:) = 0.25 * (ISS%XHO2IP(:) + ISS%XHO2IM(:) + ISS%XHO2JP(:) + ISS%XHO2JM(:)) &
                     * EXP(-SQRT(1./ZLOC(:)))
    ISS%XZ0REL(:) = MAX(ISS%XZ0REL(:),1E-10)
  ELSEWHERE
    ISS%XZ0REL(:) = 0.
  END WHERE
END WHERE      
!
IF (LHOOK) CALL DR_HOOK('GET_Z0REL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_Z0REL
!
