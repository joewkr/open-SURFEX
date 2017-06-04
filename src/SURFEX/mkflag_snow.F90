!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE MKFLAG_SNOW(TPSNOW)
!          ###################
!
!!****  *MKFLAG_SNOW* - puts undefined value on some snow quantities
!!                      where snow is not present
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
!!      Original    01/2004
!!     A. Bogatchev 09/2005 EBA snow option
!!     B. Decharme  01/2009 Limit snow mass if Density=undef
!!     P. Samuelsson 10/2014 Additional snow albedos
!!------------------------------------------------------------------
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(SURF_SNOW)  :: TPSNOW ! snow state vector
!
INTEGER :: JLAYER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MKFLAG_SNOW',0,ZHOOK_HANDLE)
IF (TPSNOW%SCHEME=='NON' .AND. LHOOK) CALL DR_HOOK('MKFLAG_SNOW',1,ZHOOK_HANDLE)
IF (TPSNOW%SCHEME=='NON') RETURN
!
 IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' &
          .OR. TPSNOW%SCHEME=='CRO') THEN  
  DO JLAYER=1,TPSNOW%NLAYER
    WHERE ( TPSNOW%RHO(:,1)== XUNDEF .AND. TPSNOW%WSNOW(:,JLAYER) > 0.0 .AND. TPSNOW%WSNOW(:,1)/= XUNDEF )
      TPSNOW%WSNOW(:,JLAYER) = 0.0
    END WHERE
  END DO
 END IF
! 
 IF (TPSNOW%SCHEME=='1-L') THEN
  DO JLAYER=1,TPSNOW%NLAYER
    WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
      TPSNOW%T(:,JLAYER) = XUNDEF
    END WHERE
   END DO
 END IF
!
 IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' &
           .OR. TPSNOW%SCHEME=='CRO') THEN  
  DO JLAYER=1,TPSNOW%NLAYER
    WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
      TPSNOW%RHO(:,JLAYER) = XUNDEF
    END WHERE
  END DO
 END IF
!
 IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  DO JLAYER=1,TPSNOW%NLAYER
    WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
      TPSNOW%HEAT(:,JLAYER) = XUNDEF
      TPSNOW%AGE (:,JLAYER) = XUNDEF
     END WHERE
   END DO
 END IF
!
IF (TPSNOW%SCHEME=='CRO') THEN
  DO JLAYER=1,TPSNOW%NLAYER
    WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
      TPSNOW%GRAN1(:,JLAYER) = XUNDEF
      TPSNOW%GRAN2(:,JLAYER) = XUNDEF
      TPSNOW%HIST(:,JLAYER) = XUNDEF
     END WHERE
   END DO
 END IF
!
 IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='3-L' &
          .OR. TPSNOW%SCHEME=='CRO') THEN  
   WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
    TPSNOW%ALB(:) = XUNDEF
    TPSNOW%ALBVIS(:) = XUNDEF
    TPSNOW%ALBNIR(:) = XUNDEF
    TPSNOW%ALBFIR(:) = XUNDEF
   END WHERE
 END IF
!
 IF (TPSNOW%SCHEME=='1-L') THEN
   WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
    TPSNOW%EMIS(:) = XUNDEF
   END WHERE
 END IF
!
 IF (TPSNOW%SCHEME=='1-L') THEN
   WHERE ( TPSNOW%WSNOW(:,1)==0. .OR. TPSNOW%WSNOW(:,1)== XUNDEF )
    TPSNOW%TS(:) = XUNDEF
   END WHERE
 END IF

IF (LHOOK) CALL DR_HOOK('MKFLAG_SNOW',1,ZHOOK_HANDLE)
!
!--------------------------------------------------
!
END SUBROUTINE MKFLAG_SNOW
