!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE CLS_WIND( PZONA, PMERA, PHW, PCD, PCDN, PRI, PHV, PZON10M, PMER10M  )  
!     ###############################################################
!
!!****  *PARAMCLS*  
!!
!!    PURPOSE
!!    -------
!
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    USE MODD_CST
!!    USE MODD_GROUND_PAR
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    26/10/98
!!      S. Riette   06/2009  height of diagnostic becomes an argument
!!      S. Riette   01/2010 XUNDEF is sent where forcing level is below heigt of
!!                          diagnostic (no extrapolation, only interpolation)
!!      P. LeMoigne 02/2015 Suppress XUNDEF
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XKARMAN
USE MODD_SURF_PAR, ONLY : XUNDEF
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
!
REAL, DIMENSION(:), INTENT(IN)       :: PZONA  ! zonal wind component
REAL, DIMENSION(:), INTENT(IN)       :: PMERA  ! meridian wind component
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height (wind)
REAL, DIMENSION(:), INTENT(IN)       :: PCD    ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PCDN   ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN)       :: PRI    ! Richardson number
REAL, DIMENSION(:), INTENT(IN)       :: PHV    ! height of diagnostic (m)
!
REAL, DIMENSION(:), INTENT(OUT)      :: PZON10M! zonal wind at 10 meters
REAL, DIMENSION(:), INTENT(OUT)      :: PMER10M! meridian wind at 10 meters
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PHW)) :: ZBN,ZBD,ZRU
REAL, DIMENSION(SIZE(PHW)) :: ZLOGU,ZCORU,ZIV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLS_WIND',0,ZHOOK_HANDLE)
PZON10M(:) = XUNDEF
PMER10M(:) = XUNDEF
!
ZBN    (:) = 0.
ZBD    (:) = 0.
ZRU    (:) = 0.
ZLOGU  (:) = 0.
ZCORU  (:) = 0.
ZIV    (:) = 0.
!
!*      1.     preparatory calculations
!              ------------------------
!
ZBN(:)=XKARMAN/SQRT(PCDN(:))
!
ZBD(:)=XKARMAN/SQRT(PCD(:))
!
WHERE(PHV(:)<=PHW(:))
   ZRU(:)=MIN(PHV(:)/PHW(:),1.)
ELSEWHERE
   ZRU(:)=MIN(PHW(:)/PHV(:),1.)
END WHERE
!
ZLOGU(:)=LOG(1.+ZRU(:)*(EXP(ZBN(:)) -1.))
!
!*      2.     Stability effects
!              -----------------
!
WHERE (PRI(:)>=0.)
  ZCORU(:)=ZRU(:)*(ZBN (:)-ZBD(:))
END WHERE
!
WHERE (PRI(:)< 0.)
  ZCORU(:)=LOG(1.+ZRU(:)*(EXP(MAX(0.,ZBN (:)-ZBD(:)))-1.))
END WHERE
!
!*      3.     Interpolation of dynamical variables
!              ------------------------------------
!
!
ZIV(:)=MAX(0.,MIN(1.,(ZLOGU(:)-ZCORU(:))/ZBD(:)))
!
WHERE(PHV(:)<=PHW(:))
  PZON10M(:)=PZONA(:)*ZIV(:)
  PMER10M(:)=PMERA(:)*ZIV(:)
ELSEWHERE
  PZON10M(:)=PZONA(:)/MAX(1.,ZIV(:))
  PMER10M(:)=PMERA(:)/MAX(1.,ZIV(:))  
END WHERE
IF (LHOOK) CALL DR_HOOK('CLS_WIND',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLS_WIND
