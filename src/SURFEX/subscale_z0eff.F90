!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE SUBSCALE_Z0EFF(ISSK,PZ0VEG,OZ0REL,OMASK  )
!     ######################################################################
!
!!*SUBSCALE_Z0EFF  computes an effective roughness lenght deduced
!!                 from the subgrid-scale orography.
!!
!!
!!    METHOD
!!    ------
!!    See M.Georgelin and al. July 1994, Monthly Weather Review.
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
!!    AUTHOR
!!    ------
!!
!!    M. Georgelin      Laboratoire d'Aerologie
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    18/12/95
!!                22/12/97 (V Masson) call with dummy arguments
!!
!----------------------------------------------------------------------------
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
USE MODI_GET_Z0REL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SSO_t), INTENT(INOUT) :: ISSK
REAL, DIMENSION(:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
LOGICAL, INTENT(IN) :: OZ0REL
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL,    DIMENSION(SIZE(ISSK%XAOSIP)) :: ZLOC
LOGICAL, DIMENSION(SIZE(ISSK%XZ0EFFIM)) :: GMASK
!
INTEGER :: JJ      ! loop counter on points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF',0,ZHOOK_HANDLE)
!
IF (.NOT.PRESENT(OMASK)) THEN
  ISSK%XZ0EFFIP = XUNDEF
  ISSK%XZ0EFFIM = XUNDEF
  ISSK%XZ0EFFJP = XUNDEF
  ISSK%XZ0EFFJM = XUNDEF
ENDIF
!
!----------------------------------------------------------------------------
!
IF (PRESENT(OMASK)) THEN
  GMASK=OMASK
ELSEIF (ALL(PZ0VEG(:)==0.)) THEN
  GMASK = (ISSK%XAOSIP/=XUNDEF)    ! computations always performed where SSO data exist
ELSE
  GMASK=PZ0VEG(:) /= XUNDEF    ! computations always performed where defined
END IF
!
!*    1.     Computations from A/S and h/2
!            -----------------------------
!      
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),ISSK%XHO2JP(:),ISSK%XAOSJP(:),ISSK%XZ0EFFJP(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),ISSK%XHO2JM(:),ISSK%XAOSJM(:),ISSK%XZ0EFFJM(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),ISSK%XHO2IM(:),ISSK%XAOSIM(:),ISSK%XZ0EFFIM(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),ISSK%XHO2IP(:),ISSK%XAOSIP(:),ISSK%XZ0EFFIP(:))
!
IF (OZ0REL) CALL GET_Z0REL(ISSK,GMASK)
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE GET_Z0EFF(OCOMPUT,PZ0,PHO,PAO,PZ0EFF)
!
USE MODD_ISBA_PAR,   ONLY : XCDZ0EFF
USE MODD_CSTS,       ONLY : XKARMAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOMPUT
REAL,    DIMENSION(:), INTENT(IN) :: PZ0
REAL,    DIMENSION(:), INTENT(IN) :: PHO
REAL,    DIMENSION(:), INTENT(IN) :: PAO
REAL,    DIMENSION(:), INTENT(INOUT):: PZ0EFF
!
LOGICAL, DIMENSION(SIZE(PZ0)) :: LWORK1
!
REAL    :: ZLOC1,ZLOC2,ZLOC3
INTEGER :: JJ, INI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF:GET_ZOEFF',0,ZHOOK_HANDLE)
!
INI=SIZE(PZ0)
!
LWORK1(:)=(PHO(:)>PZ0(:).AND.(PZ0(:)/=0.0.OR.PAO(:)/=0.0))
!
DO JJ=1,INI
  IF (OCOMPUT(JJ)) THEN
    IF (LWORK1(JJ)) THEN 
      ZLOC1  = (XCDZ0EFF/(2.*XKARMAN**2))*PAO(JJ)
      IF ( PZ0(JJ) > 0. ) THEN
        ZLOC2 = 1./(ALOG(PHO(JJ)/PZ0(JJ)))**2
      ELSE
        ZLOC2 = 0.
      ENDIF 
      ZLOC3  = SQRT(1./(ZLOC1+ZLOC2))
      PZ0EFF(JJ) = PHO(JJ) * EXP(-ZLOC3)
    ELSE
      PZ0EFF(JJ) = PZ0(JJ) 
    ENDIF
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF:GET_ZOEFF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_Z0EFF

END SUBROUTINE SUBSCALE_Z0EFF
