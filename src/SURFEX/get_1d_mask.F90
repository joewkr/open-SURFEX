!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE GET_1D_MASK(KSIZE,KFRAC,PFRAC,KMASK)
!!
!!    PURPOSE
!!    -------
!     Create a surface mask which is 1D in space
!!
!!    AUTHOR
!!    ------
!!     A. Boone
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!------------------------------------------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                      :: KSIZE
INTEGER, INTENT(IN)                      :: KFRAC
REAL, DIMENSION(KFRAC), INTENT(IN)       :: PFRAC
!
INTEGER, DIMENSION(KSIZE), INTENT(OUT)   :: KMASK
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: JI, JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_1D_MASK',0,ZHOOK_HANDLE)
KMASK(:)            = 0
JI                  = 0
DO JJ=1,SIZE(PFRAC)
   IF(PFRAC(JJ) > 0.0)THEN
      JI            = JI + 1
      KMASK(JI)     = JJ
   ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('GET_1D_MASK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_1D_MASK
